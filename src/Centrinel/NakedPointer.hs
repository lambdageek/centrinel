-- | Analyze function signatures to identify naked pointers into the
-- managed heap.
{-# language GeneralizedNewtypeDeriving #-}
module Centrinel.NakedPointer (analyze, AnalysisOpts(..)) where

import Control.Monad (forM_, (<=<))

import Control.Monad.Reader.Class
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer.Class
import Control.Monad.Writer (execWriterT)
import Control.Monad.State.Class
import Control.Monad.State.Strict (StateT, evalStateT)

import Data.Foldable (traverse_)
import qualified Data.Map as Map

import Centrinel.RegionInferenceResult
import Centrinel.Region
import Centrinel.NakedPointerError

import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Data.Ident as C
import qualified Language.C.Data.Node as C
import qualified Language.C.Data.Position as C

import qualified Language.C.Analysis.DefTable as CDT
import qualified Language.C.Analysis.TravMonad as CM

import Centrinel.RegionResultMonad

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

-- | @declPtrRegion d = Just rs@ if the 'Declaration' @d@ has pointer type @ty *@ and
-- @ty@ has region scheme @rs@.  Otherwise returns @Nothing@.
declPtrRegion :: (RegionResultMonad m, C.Declaration d) => d -> m (Maybe RegionScheme)
declPtrRegion = pointerRegionScheme . C.declType

class NakedPointerSummary a where
  nakedPointers :: (RegionResultMonad m, MonadReader NPEPosn m, MonadWriter NPEVictims m) => a -> m ()

instance NakedPointerSummary C.Type where
  nakedPointers ty_ =
    case ty_ of
      C.DirectType {} -> return ()
      C.PtrType ty _qs _attrs -> nakedPointers ty
      C.ArrayType ty _sz _qs _attrs -> nakedPointers ty
      C.TypeDefType tdr _qs _attrs -> nakedPointers tdr
      C.FunctionType fty _attrs -> nakedPointers fty

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
            x <- pointerRegionScheme retty
            case x of
              Just rs | isManagedRegion rs -> tellNPE retty
              _ -> return ()
            -- if return is a function, check its args too
            nakedPointers retty
          forM_ (zip params [0..]) $ \(param, j) -> do
            let ctx = NPEArg j (C.declName param) (C.nodeInfo param)
            local ctx $ do
              -- first check if the arg is a ptr to managed
              x <- declPtrRegion param
              case x of
                Just rs | isManagedRegion rs -> tellNPE (C.declType param)
                _ -> return ()
               -- then if arg is a function type, check its args
              nakedPointers (C.declType param)

instance NakedPointerSummary C.FunDef where
  nakedPointers (C.FunDef _decl _stmt _ni) = do
    let symtab = (undefined :: CDT.DefTable) -- TODO: need to get my hands on a symtab.  possibly i need to be called back from the main analysis?
    evalLocalSymtabT symtab $ inFunctionScope $ return () -- FIXME: finish me and get rid of the undefined DefTable

-- TODO: move this to a separate file
newtype LocalSymtabT m a = LocalSymtabT { unLocalSymtabT :: StateT CDT.DefTable m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => CM.MonadSymtab (LocalSymtabT m) where
  getDefTable = LocalSymtabT get
  withDefTable f = LocalSymtabT $ do
    x <- get
    let ~(a, y) = f x
    put y
    return a

evalLocalSymtabT :: Monad m => CDT.DefTable -> LocalSymtabT m a -> m a
evalLocalSymtabT st0 comp = evalStateT (unLocalSymtabT comp) st0

-- TODO: move this to a separate file
inFunctionScope :: CM.MonadSymtab m => m a -> m a
inFunctionScope comp = do
  CM.enterFunctionScope
  x <- comp
  CM.leaveFunctionScope
  return x
  

tellNPE :: (MonadReader NPEPosn m, MonadWriter NPEVictims m) => C.Type -> m ()
tellNPE ty = ask >>= \npe -> tell [NPEVictim ty npe]

nakedPtrCheckDecl :: (RegionResultMonad m, C.Declaration d, C.CNode d) => d -> m (Maybe NakedPointerError)
nakedPtrCheckDecl dcl = do
  npes <- execWriterT $ flip runReaderT (NPEDecl $ C.declName dcl) $ nakedPointers (C.declType dcl)
  return $ case npes of
    [] -> Nothing
    _ ->  Just $ mkNakedPointerError (C.nodeInfo dcl) npes

nakedPtrCheckDefn :: (RegionResultMonad m) => C.FunDef -> m (Maybe NakedPointerError)
nakedPtrCheckDefn defn = do
  npes <- execWriterT $ flip runReaderT (NPEDefn $ C.declName defn) $ nakedPointers defn
  return $ case npes of
    [] -> Nothing
    _ ->  Just $ mkNakedPointerError (C.nodeInfo defn) npes

analyze :: (RegionResultMonad m, CM.MonadCError m) => AnalysisOpts -> Map.Map C.Ident C.IdentDecl -> m ()
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
