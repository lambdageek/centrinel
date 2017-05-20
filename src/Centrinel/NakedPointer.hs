-- | Analyze function signatures to identify naked pointers into the
-- managed heap.
{-# language GeneralizedNewtypeDeriving, DefaultSignatures #-}
module Centrinel.NakedPointer (analyze, AnalysisOpts(..)) where

import Control.Monad (forM_, (<=<))

import Control.Monad.Reader.Class
import Control.Monad.Reader (runReaderT, ReaderT (..))
import Control.Monad.Writer.Class
import Control.Monad.Writer (execWriterT)

import Data.Foldable (traverse_)
import qualified Data.Map as Map

import Centrinel.RegionInferenceResult
import Centrinel.Region
import Centrinel.NakedPointerError

import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Data.Ident as C
import qualified Language.C.Data.Node as C
import qualified Language.C.Data.Position as C
import qualified Language.C.Analysis.AstAnalysis as CT

import qualified Language.C.Analysis.DefTable as CDT
import qualified Language.C.Analysis.TravMonad as CM

import qualified Language.C.Syntax.AST as CStx

import Language.C.Analysis.TravMonad.Instances ()

import Centrinel.Control.Monad.Class.RegionResult

import Centrinel.Control.Monad.LocalSymtab (evalLocalSymtabT)

class HasRegionScheme t where
  -- | Get the 'RegionScheme' for the given type
  getRegionScheme :: RegionResultMonad m => t -> m RegionScheme

instance HasRegionScheme StructTagRef where
  getRegionScheme = rrStructTagRegion

instance HasRegionScheme C.CompTypeRef where
  getRegionScheme ctr =
    case ctr of
      C.CompTypeRef _sueref C.UnionTag _ni -> return PolyRS
      C.CompTypeRef sueref C.StructTag _ni -> getRegionScheme (StructTagRef sueref)

instance HasRegionScheme C.TypeName where
  getRegionScheme tn =
    case tn of
      C.TyVoid -> return PolyRS
      C.TyIntegral {} -> return PolyRS
      C.TyFloating {} -> return PolyRS
      C.TyComplex {} -> return PolyRS
      C.TyComp ctr -> getRegionScheme ctr
      C.TyEnum {} -> return PolyRS
      C.TyBuiltin {} -> return PolyRS

instance HasRegionScheme C.TypeDefRef where
  getRegionScheme tdr =
    case tdr of
      C.TypeDefRef _ident ty _ni -> getRegionScheme ty

instance HasRegionScheme C.TypeDef where
  getRegionScheme td =
    case td of
      C.TypeDef _ident ty _attrs _ni -> getRegionScheme ty

instance HasRegionScheme C.Type where
  getRegionScheme ty =
    case ty of
      C.DirectType tn _qs _attrs -> getRegionScheme tn
      C.PtrType _ty _qs _attrs -> return PolyRS
      C.ArrayType _ty _sz _qs _attrs -> return PolyRS
      C.FunctionType _ft _attrs -> return PolyRS
      C.TypeDefType tdr _qs _attrs -> getRegionScheme tdr


class PointerRegionScheme t where
  -- | If @t@ is a pointer type @ty *@ (or a typedef thereof), return @Just rs@ wregion scheme
  -- of @ty@. Otherwise return @Nothing@
  pointerRegionScheme :: RegionResultMonad m => t -> m (Maybe RegionScheme)

instance PointerRegionScheme C.Type where
  pointerRegionScheme ty =
    case ty of
      C.DirectType {} -> return Nothing
      C.PtrType ty' _qs _attrs -> Just <$> getRegionScheme ty'
      C.ArrayType {} -> return Nothing -- FIXME: decay to ptr?
      C.FunctionType {} -> return Nothing
      C.TypeDefType tdr _qs _attrs -> pointerRegionScheme tdr

instance PointerRegionScheme C.TypeDefRef where
  pointerRegionScheme tdr =
    case tdr of
      C.TypeDefRef _ident ty _ni -> pointerRegionScheme ty

instance PointerRegionScheme C.TypeDef where
  pointerRegionScheme td =
    case td of
      C.TypeDef _ident ty _attrs _qs -> pointerRegionScheme ty

-- | Tell when the given type is a naked pointer to the managed heap
tellWhenManaged :: (MonadReader NPEPosn m, MonadWriter NPEVictims m, RegionResultMonad m)
                => C.Type -> m ()
tellWhenManaged ty = do
  x <- pointerRegionScheme ty
  case x of
    Just rs | isManagedRegion rs -> tellNPE ty
    _ -> return ()

-- | Given a structure @a@, @nakedPointers a@ is a computation that
-- 'tell's the naked pointers that appear in a.
class NakedPointerSummary a where
  nakedPointers :: (RegionResultMonad m, MonadReader NPEPosn m, MonadWriter NPEVictims m) => a -> m ()

-- | Given a structure @a@ whose meaning may depend on a local symbol table
-- @nakedPointersFun a@ is a computation that 'tell's the naked
-- pointers that appear in a.
class NakedPointerFunSummary a where
  funNakedPointers ::  (RegionResultMonad m, MonadReader NPEPosn m, MonadWriter NPEVictims m, CM.MonadTrav m) => a -> m ()
  default funNakedPointers :: (RegionResultMonad m, MonadReader NPEPosn m, MonadWriter NPEVictims m, NakedPointerSummary a) => a -> m ()
  funNakedPointers = defaultFunNakedPointers

-- | default implementation for @'nakedPointers' :: 'NakedPointerSummary' a => a -> m ()@ when @a@ is an instance of 'NakedPointerFunSummary'
defaultFunNakedPointers :: (RegionResultMonad m, MonadReader NPEPosn m, MonadWriter NPEVictims m, NakedPointerSummary a) => a -> m ()
defaultFunNakedPointers = nakedPointers

instance NakedPointerSummary C.Type where
  nakedPointers ty_ =
    case ty_ of
      C.DirectType {} -> return ()
      C.PtrType ty _qs _attrs -> nakedPointers ty
      C.ArrayType ty _sz _qs _attrs -> nakedPointers ty
      C.TypeDefType tdr _qs _attrs -> nakedPointers tdr
      C.FunctionType fty _attrs -> nakedPointers fty
instance NakedPointerFunSummary C.Type

instance NakedPointerSummary C.TypeDefRef where
  nakedPointers tdr =
    case tdr of
      -- always lookup the typedef even if the type is available - it makes the
      -- victim look nicer.  also if we start avoiding revisiting the same
      -- typedef, this will save us a type traversal.
      --
      -- C.TypeDefRef _ (Just ty) ni ->
      --  local (NPETypeDefRef ni) $ nakedPointers ty
      C.TypeDefRef ident _ {-Nothing-} ni ->
        local (NPETypeDefRef ni) (rrLookupTypedef ident >>= nakedPointers)

instance NakedPointerSummary C.TypeDef where
  nakedPointers td =
    case td of
      C.TypeDef _ident ty _attrs ni ->
        local (NPETypeDefDef ni) (nakedPointers ty)

instance NakedPointerSummary C.FunType where
  nakedPointers fty =
    -- for every type comprising a function type, we do two things:
    -- 1. if it's a pointer, make sure it doesn't point into the managed region;
    -- 2. if it's a function type, recursively check that that function doesn't
    -- have args/return types that point into the managed region.
    --
    -- The former is done by pointerRegionScheme & declPtrRegion,
    -- the latter by recursively calling go.
    --
    -- TODO: this does mean we potentially revisit the same typedef many times.
    -- Maybe use a visited set?
    case fty of
      C.FunTypeIncomplete {} ->
        -- analysis is after typechecking, all params seen already.
        --
        -- FIXME: There's one place where these can still happen.  If a
        -- function is declared without a definition as "int foo ();" then
        -- GlobalDecls still has it as a FunTypeIncomplete
        --
        -- (as expected, "int foo (void);" will be a FunType with no arguments)
        error "unexpected FunTypeIncomplete in nakedPtrCheckIdentDecl"
      C.FunType retty params _variadic -> do
          local NPERet $ do
            tellWhenManaged retty
            -- if return is a function, check its args too
            nakedPointers retty
          forM_ (zip params [0..]) $ \(param, j) -> do
            let ctx = NPEArg j (C.declName param) (C.nodeInfo param)
            local ctx $ do
              let ty = C.declType param
              -- first check if the arg is a ptr to managed
              tellWhenManaged ty
               -- then if arg is a function type, check its args
              nakedPointers ty

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
          funCompoundBody items
      CStx.CLabel _lbl stmt _attrs _ni -> funNakedPointers stmt
      CStx.CSwitch {} -> return ()  -- TODO
      CStx.CCase {} -> return ()    -- TODO
      CStx.CCases {} -> return ()   -- TODO
      CStx.CDefault {} -> return () -- TODO
      CStx.CExpr Nothing _ni -> return ()
      CStx.CExpr (Just e) ni -> local (NPEStmt ni) $ funNakedPointers e
      CStx.CIf condE trueS mfalseS _ni -> do
        funNakedPointers condE
        funNakedPointers trueS
        traverse_ funNakedPointers mfalseS
      CStx.CWhile condE body _whatIsThisBool ni -> do
        local (NPEStmt ni) $ funNakedPointers condE
        funNakedPointers body
      CStx.CFor {} -> return () -- TODO
      CStx.CCont {} -> return ()  -- ok
      CStx.CBreak {} -> return () -- ok
      CStx.CGoto {} -> return ()  -- ok
      CStx.CGotoPtr e ni -> local (NPEStmt ni) $ funNakedPointers e
      CStx.CReturn me ni -> do
        local (NPEStmt ni) $ traverse_ funNakedPointers me -- TODO: and also something about enter/return pairs?
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
        local (NPETypeOfExpr ni) $ tellWhenManaged returnType -- check that the function doesn't return a naked pointer
      CStx.CComma es _ni -> traverse_ funNakedPointers es
      CStx.CAssign _asgnop lhs rhs _ni -> do
        funNakedPointers lhs
        funNakedPointers rhs
        -- TODO: and something about whether we're assigning through a naked pointer?
      CStx.CCond condE trueE falseE _ni -> do
        funNakedPointers condE
        traverse_ funNakedPointers trueE -- apparently GNU allows leaving out the true case??
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
        local (NPETypeOfExpr ni) $ tellWhenManaged returnType
        return () -- TODO: if its a handle, disallow payload access
      CStx.CCompoundLit _decl ilist _ni ->
        -- TODO: check whether _decl is a handle or something?
        -- just traverse the initializers
        traverse_ (funNakedPointers . snd) ilist
      CStx.CGenericSelection {} -> error "C generic selection, in my code, really?"
      CStx.CStatExpr stmt ni ->
        local (NPEStmt ni) $ funNakedPointers stmt
      CStx.CLabAddrExpr _id _ni -> return ()
      CStx.CBuiltinExpr _builtinThing -> return ()

instance NakedPointerFunSummary (CStx.CInitializer C.NodeInfo) where
  funNakedPointers init0 =
    case init0 of
      CStx.CInitExpr e _ni -> funNakedPointers e
      CStx.CInitList ilist _ni -> traverse_ (funNakedPointers . snd) ilist

funCompoundBody :: (CM.MonadSymtab m, RegionResultMonad m,
                    MonadWriter NPEVictims m, MonadReader NPEPosn m,
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
                       MonadReader NPEPosn m, MonadWriter NPEVictims m,
                       CM.MonadTrav m)
                   => CStx.CDecl
                   -> m ()
consumeDeclaration cdecl@(CStx.CDecl _declSpecs declarators _ni) = do
  forM_ declarators $ \(_declarator, minitializer, _size) -> do
    traverse funNakedPointers minitializer
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

tellNPE :: (MonadReader NPEPosn m, MonadWriter NPEVictims m) => C.Type -> m ()
tellNPE ty = ask >>= \npe -> tell [NPEVictim ty npe]

nakedPtrCheckDecl :: (RegionResultMonad m, C.Declaration d, C.CNode d) => d -> m (Maybe NakedPointerError)
nakedPtrCheckDecl dcl = do
  npes <- execWriterT $ flip runReaderT (NPEDecl $ C.declName dcl) $ nakedPointers (C.declType dcl)
  return $ case npes of
    [] -> Nothing
    _ ->  Just $ mkNakedPointerError (C.nodeInfo dcl) npes

nakedPtrCheckDefn :: (RegionResultMonad m, CM.MonadTrav m) => C.FunDef -> m (Maybe NakedPointerError)
nakedPtrCheckDefn defn = do
  symtab <- CM.getDefTable
  npes <- execWriterT $ flip runReaderT (NPEDefn $ C.declName defn) $ evalLocalSymtabT symtab $ funNakedPointers defn
  return $ case npes of
    [] -> Nothing
    _ ->  Just $ mkNakedPointerError (C.nodeInfo defn) npes

analyze :: (RegionResultMonad m, CM.MonadTrav m) => AnalysisOpts -> Map.Map C.Ident C.IdentDecl -> m ()
analyze opts m = do
  let (fnDecls, (_globalDefns, _enumDefns, fnDefns)) = C.splitIdentDecls True m
  forM_ (optFilterDecls opts fnDecls) (recordNPE <=< nakedPtrCheckDecl)
  forM_ fnDefns (recordNPE <=< nakedPtrCheckDefn)
  where
    recordNPE :: (CM.MonadCError m) => Maybe NakedPointerError -> m ()
    recordNPE = traverse_ CM.recordError
      

-- | Options guiding the naked pointer analysis
data AnalysisOpts =
  AnalysisOpts
  { analysisOptFilterPath :: Maybe FilePath -- ^ @Just fp@ if we should only
                                            -- consider declarations from the
                                            -- given path, otherwise consider
                                            -- every declaration.
  }

optFilterDecls :: AnalysisOpts -> Map.Map C.Ident a -> Map.Map C.Ident a
optFilterDecls opts =
  case analysisOptFilterPath opts of
    Just fp -> Map.filterWithKey $ \k _v ->
      let pos = C.posOf k
      in C.isSourcePos pos && fp == C.posFile pos
    Nothing -> id
