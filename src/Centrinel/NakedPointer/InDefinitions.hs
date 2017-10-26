-- | Find uses of naked pointers to managed regions in C function defintions
{-# language DefaultSignatures #-}
module Centrinel.NakedPointer.InDefinitions (nakedPtrCheckDefn) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Reader.Class
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer.Class
import Control.Monad.Writer (execWriterT)

import Data.Foldable (forM_, traverse_)

import ZeptoLens

import qualified Language.C.Analysis.AstAnalysis as CT
import qualified Language.C.Analysis.DefTable as CDT
import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Analysis.TravMonad as CM
import qualified Language.C.Data.Node as C
import qualified Language.C.Data.Ident as C
import qualified Language.C.Syntax.AST as CStx
import qualified Language.C.Syntax.Constants as CStx

import Language.C.Analysis.TravMonad.Instances ()

import Centrinel.Control.Monad.Class.RegionResult
import Centrinel.Control.Monad.LocalSymtab (evalLocalSymtabT)

import Centrinel.NakedPointer.Env
import Centrinel.NakedPointer.InDeclarations
import Centrinel.NakedPointer.Utils
import Centrinel.NakedPointerError (NPEPosn(..), NPEVictims, NakedPointerError, mkNakedPointerError)

  
-- | Given a structure @a@ whose meaning may depend on a local symbol table
-- @nakedPointersFun a@ is a computation that 'tell's the naked
-- pointers that appear in a.
class NakedPointerFunSummary a where
  funNakedPointers ::  (RegionResultMonad m, MonadReader AnalysisEnv m, MonadWriter NPEVictims m, CM.MonadTrav m) => a -> m ()
  default funNakedPointers :: (RegionResultMonad m, MonadReader AnalysisEnv m, MonadWriter NPEVictims m, NakedPointerSummary a) => a -> m ()
  funNakedPointers = defaultFunNakedPointers

-- | default implementation for @'nakedPointers' :: 'NakedPointerSummary' a => a -> m ()@ when @a@ is an instance of 'NakedPointerFunSummary'
defaultFunNakedPointers :: (RegionResultMonad m, MonadReader AnalysisEnv m, MonadWriter NPEVictims m, NakedPointerSummary a) => a -> m ()
defaultFunNakedPointers = nakedPointers

instance NakedPointerFunSummary a => NakedPointerFunSummary (Maybe a) where
  funNakedPointers = traverse_ funNakedPointers

instance NakedPointerFunSummary C.Type

instance NakedPointerFunSummary C.FunDef where
  funNakedPointers (C.FunDef decl stmt _ni) =
    inFunctionScope $ do
      addparameters decl
      addAllLabels stmt
      funNakedPointers stmt
        where
          addparameters fundecl =
            case C.declType fundecl of
              C.FunctionType (C.FunType _ params _) _ -> mapM_ addparam params
              _ -> return () -- TODO: unexpected here.
          addAllLabels _stmt = return () -- TODO: bring all the statement labels
                               -- in the function body into scope.
          addparam p = case p of
            C.AbstractParamDecl {} -> return () -- TODO: warn that this is unexpected in a definition
            C.ParamDecl vardecl ni -> do
              let def = C.ObjectDef (C.ObjDef vardecl Nothing ni)
              _ <- CM.withDefTable $ CDT.defineScopedIdent (C.declIdent def) def
              return ()

instance NakedPointerFunSummary C.Stmt where
  funNakedPointers stmt0 = do
    case stmt0 of
      CStx.CCompound lblDecls items _ni  ->
        inBlockScope $ do
          mapM_ (CM.withDefTable . CDT.defineLabel) lblDecls
          let
            scopeFn :: MonadReader AnalysisEnv m => m a -> m a
            scopeFn = case findSuppressPragma items of
              Just suppress -> local (analysisSuppress .~ suppress)
              Nothing -> id
          scopeFn $ funCompoundBody items
      CStx.CLabel _lbl stmt _attrs _ni -> funNakedPointers stmt
      CStx.CSwitch e s ni -> do
        funNakedPointers e
        local (analysisPosn %~ NPEStmt ni) $ funNakedPointers s
      CStx.CCase _elit s ni -> local (analysisPosn %~ NPEStmt ni) $ funNakedPointers s
      CStx.CCases _e1 _e2 s ni -> local (analysisPosn %~ NPEStmt ni) $ funNakedPointers s
      CStx.CDefault s ni -> local (analysisPosn %~ NPEStmt ni) $ funNakedPointers s
      CStx.CExpr Nothing _ni -> return ()
      CStx.CExpr (Just e) ni -> local (analysisPosn %~ NPEStmt ni) $ funNakedPointers e
      CStx.CIf condE trueS mfalseS _ni -> do
        funNakedPointers condE
        funNakedPointers trueS
        traverse_ funNakedPointers mfalseS
      CStx.CWhile condE body _whatIsThisBool ni -> do
        local (analysisPosn %~ NPEStmt ni) $ funNakedPointers condE
        funNakedPointers body
      CStx.CFor initE guardE incE s ni ->
        local (analysisPosn %~ NPEStmt ni) $ do
        scope <- case initE of
          Left me -> do
            funNakedPointers me
            return id
          Right d -> do
            return (\cont -> inBlockScope (consumeDeclaration d >> cont))
        scope $ do
          funNakedPointers guardE
          funNakedPointers incE
          funNakedPointers s
      CStx.CCont {} -> return ()  -- ok
      CStx.CBreak {} -> return () -- ok
      CStx.CGoto {} -> return ()  -- ok
      CStx.CGotoPtr e ni -> local (analysisPosn %~ NPEStmt ni) $ funNakedPointers e
      CStx.CReturn me ni -> do
        local (analysisPosn %~ NPEStmt ni) $ traverse_ funNakedPointers me -- TODO: and also something about enter/return pairs?
      CStx.CAsm {} -> return () -- ugh

instance NakedPointerFunSummary C.Expr where
  funNakedPointers expr0 =
    case expr0 of
      CStx.CVar _ident _ni -> return () -- don't care, checked at declaration site
      CStx.CConst _constant -> return ()
      CStx.CCall callee args ni -> do
        funNakedPointers callee
        mapM_ funNakedPointers args
        let stmtctx = undefined
            side = undefined
        -- a bit unfortunate that we retraverse the subtree here
        returnType <- CT.tExpr stmtctx side expr0
        local (analysisPosn %~ NPETypeOfExpr ni) $ tellWhenManaged returnType -- check that the function doesn't return a naked pointer
      CStx.CComma es _ni -> traverse_ funNakedPointers es
      CStx.CAssign _asgnop lhs rhs _ni -> do
        funNakedPointers lhs
        funNakedPointers rhs
        -- TODO: and something about whether we're assigning through a naked pointer?
      CStx.CCond condE trueE falseE _ni -> do
        funNakedPointers condE
        funNakedPointers trueE -- apparently GNU allows leaving out the true case??
        funNakedPointers falseE
      CStx.CBinary _binop lhs rhs _ni -> do
        funNakedPointers lhs
        funNakedPointers rhs
      CStx.CCast _decl e _ni -> do
        funNakedPointers e
        return () -- TODO: analyze the 'decl' and type of 'e'
      CStx.CUnary _uop e _ni -> do
        funNakedPointers e
        return () -- TODO: handle derefs?
      CStx.CSizeofExpr _e _ni -> return () -- I don't care, I think
      CStx.CSizeofType _ty _ni -> return ()
      CStx.CAlignofExpr _e _ni -> return ()
      CStx.CAlignofType _ty _ni -> return ()
      CStx.CComplexReal e _ni -> funNakedPointers e
      CStx.CComplexImag e _ni -> funNakedPointers e
      CStx.CIndex earr eidx _ni -> do
        funNakedPointers earr
        funNakedPointers eidx
        return () -- TODO: check something here?
      CStx.CMember expr _fieldIdent _isDeref ni -> do
        funNakedPointers expr
        let stmtctx = undefined
            side = undefined
        returnType <- CT.tExpr stmtctx side expr0
        local (analysisPosn %~ NPETypeOfExpr ni) $ tellWhenManaged returnType
        return () -- TODO: if its a handle, disallow payload access
      CStx.CCompoundLit _decl ilist _ni ->
        -- TODO: check whether _decl is a handle or something?
        -- just traverse the initializers
        traverse_ (funNakedPointers . snd) ilist
      CStx.CGenericSelection {} -> error "C generic selection, in my code, really?"
      CStx.CStatExpr stmt ni ->
        local (analysisPosn %~ NPEStmt ni) $ funNakedPointers stmt
      CStx.CLabAddrExpr _id _ni -> return ()
      CStx.CBuiltinExpr _builtinThing -> return ()

instance NakedPointerFunSummary (CStx.CInitializer C.NodeInfo) where
  funNakedPointers init0 =
    case init0 of
      CStx.CInitExpr e _ni -> funNakedPointers e
      CStx.CInitList ilist _ni -> traverse_ (funNakedPointers . snd) ilist

funCompoundBody :: (CM.MonadSymtab m, RegionResultMonad m,
                    MonadWriter NPEVictims m, MonadReader AnalysisEnv m,
                    CM.MonadTrav m)
                => [CStx.CBlockItem] -> m ()
funCompoundBody = traverse_ compoundBodyItem 
  where
    compoundBodyItem item = case item of
      CStx.CBlockStmt stmt -> do
        funNakedPointers stmt
      CStx.CBlockDecl decl -> do
        consumeDeclaration decl
      CStx.CNestedFunDef {} -> return () -- TODO: warn unexpected

consumeDeclaration :: (RegionResultMonad m,
                       MonadReader AnalysisEnv m, MonadWriter NPEVictims m,
                       CM.MonadTrav m)
                   => CStx.CDecl
                   -> m ()
consumeDeclaration cdecl@(CStx.CDecl _declSpecs declarators _ni) = do
  forM_ declarators $ \(_declarator, minitializer, _size) -> funNakedPointers minitializer
  CT.analyseDecl True cdecl
consumeDeclaration (CStx.CStaticAssert {}) = return () -- TODO: handle static assertions?

around :: Monad m => m () -> m () -> m a -> m a
around enter leave comp = do
  enter
  x <- comp
  leave
  return x

inFunctionScope :: CM.MonadSymtab m => m a -> m a
inFunctionScope = around CM.enterFunctionScope CM.leaveFunctionScope

inBlockScope :: CM.MonadSymtab m => m a -> m a
inBlockScope = around CM.enterBlockScope CM.leaveBlockScope

-- | Find the statement @dummyLabel: __attribute__((__suppress([01]))) ;@ at the beginning of the given list of
-- block items.
findSuppressPragma :: [CStx.CBlockItem] -> Maybe Bool
findSuppressPragma items =
  case items of
    (CStx.CBlockStmt (CStx.CLabel _id (CStx.CExpr Nothing _exprNi) [labelAttr] _ni) : _)
      -> findSuppressAttribute labelAttr
    _ -> mzero

findSuppressAttribute :: CStx.CAttr -> Maybe Bool
findSuppressAttribute attr =
  case attr of
    (CStx.CAttr ident [CStx.CConst (CStx.CIntConst i _constNi)] _ni)
      | isSuppress ident
        -> case CStx.getCInteger i of
             0 -> return False
             1 -> return True
             _ -> mzero
    _ -> mzero

isSuppress:: C.Ident -> Bool
isSuppress ident = C.identToString ident == "__suppress"

nakedPtrCheckDefn :: (RegionResultMonad m, CM.MonadTrav m) => C.FunDef -> m (Maybe NakedPointerError)
nakedPtrCheckDefn defn = do
  symtab <- CM.getDefTable
  let
    initialEnv :: AnalysisEnv
    initialEnv = AnalysisEnv (NPEDefn $ C.declName defn) False
  npes <- execWriterT $ flip runReaderT initialEnv $ evalLocalSymtabT symtab $ funNakedPointers defn
  return $ case npes of
    [] -> Nothing
    _ ->  Just $ mkNakedPointerError (C.nodeInfo defn) npes

