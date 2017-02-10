{-# language GeneralizedNewtypeDeriving #-}
module HeapGuard.NakedPointer (analyze, runInferenceResultT, InferenceResultT) where

import Control.Monad (forM_)

import Control.Monad.Reader.Class
import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad.Error.Class
import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.Writer.Class
import Control.Monad.Writer (execWriterT, WriterT)
import Control.Monad.Trans.Class (lift)

import qualified Data.Map as Map
import qualified Data.Assoc

import HeapGuard.RegionInferenceResult
import HeapGuard.Region
import HeapGuard.NakedPointerError

import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Data.Ident as C
import qualified Language.C.Data.Node as C

import qualified Language.C.Analysis.TravMonad as CM

class Monad m => RegionResultMonad m where
  rrStructTagRegion :: StructTagRef -> m RegionScheme
  rrLookupTypedef :: C.Ident -> m C.TypeDef

instance (Monoid w, RegionResultMonad m) => RegionResultMonad (WriterT w m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

instance RegionResultMonad m => RegionResultMonad (ReaderT r m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

instance RegionResultMonad m => RegionResultMonad (ExceptT e m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

newtype InferenceResultT m a = InferenceResultT { unInferenceResultT :: ReaderT (Map.Map C.Ident C.TypeDef, RegionInferenceResult) m a }
  deriving (Functor, Applicative, Monad)

instance CM.MonadCError m => CM.MonadCError (InferenceResultT m) where
  throwTravError = InferenceResultT . lift . CM.throwTravError
  catchTravError = error "finish catchTravError for InferenceResultT" -- FIXME: finish me
  recordError = InferenceResultT . lift . CM.recordError
  getErrors = InferenceResultT $ lift $ CM.getErrors
  

instance Monad m => RegionResultMonad (InferenceResultT m) where
  rrStructTagRegion sr = InferenceResultT $ asks (certain . Map.lookup sr . Data.Assoc.getAssocMap . snd)
    where
      certain Nothing = error "cannot get Nothing from rrStructTagRegion"
      certain (Just a) = a
  rrLookupTypedef ident = InferenceResultT $ asks (certain . Map.lookup ident . fst)
    where
      certain Nothing = error "cannot get Nothing  from rrLookupTypedef"
      certain (Just a) = a

runInferenceResultT :: Monad m => InferenceResultT m a -> Map.Map C.Ident C.TypeDef -> RegionInferenceResult -> m a
runInferenceResultT comp = curry (runReaderT (unInferenceResultT comp))

class HasRegionScheme t where
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
      C.TypeDefRef _ident (Just ty) _ni -> getRegionScheme ty
      C.TypeDefRef ident Nothing _ni -> rrLookupTypedef ident >>= getRegionScheme

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
      C.TypeDefRef _ident (Just ty) _ni -> pointerRegionScheme ty
      C.TypeDefRef ident Nothing _ni -> rrLookupTypedef ident >>= pointerRegionScheme

instance PointerRegionScheme C.TypeDef where
  pointerRegionScheme td =
    case td of
      C.TypeDef _ident ty _attrs _qs -> pointerRegionScheme ty

-- | @declPtrRegion d = Just rs@ if the 'Declaration' @d@ has pointer type @ty *@ and
-- @ty@ has region scheme @rs@.  Otherwise returns @Nothing@.
declPtrRegion :: (RegionResultMonad m, C.Declaration d) => d -> m (Maybe RegionScheme)
declPtrRegion = pointerRegionScheme . C.declType

nakedPtrCheckDecl :: (RegionResultMonad m,
                            MonadError NakedPointerError m, C.Declaration d, C.CNode d) => d -> m ()
nakedPtrCheckDecl dcl =
  do
    npes <- execWriterT $ flip runReaderT (NPEDecl $ C.declName dcl) $ go (C.declType dcl)
    case npes of
      [] -> return ()
      _ ->  throwError $ mkNakedPointerError (C.nodeInfo dcl) npes
  where
    go :: (RegionResultMonad m, MonadReader NPEPosn m, MonadWriter NPEVictims m) => C.Type -> m ()
    go ty_ =
      case ty_ of
        C.DirectType {} -> return ()
        C.PtrType ty _qs _attrs -> go ty
        C.ArrayType ty _sz _qs _attrs -> go ty
        C.TypeDefType tdr _qs _attrs -> goTypeDefRef tdr
        C.FunctionType fty _attrs -> checkFunty fty
    goTypeDefRef :: (RegionResultMonad m, MonadReader NPEPosn m, MonadWriter NPEVictims m)
                 => C.TypeDefRef -> m ()
    goTypeDefRef tdr =
      case tdr of
        -- always lookup the typedef even if the type is available - it makes the
        -- victim look nicer.  also if we start avoiding revisiting the same
        -- typedef, this will save us a type traversal.
        --
        -- C.TypeDefRef _ (Just ty) ni ->
        --  local (NPETypeDefRef ni) $ go ty
        C.TypeDefRef ident _ {-Nothing-} ni ->
          local (NPETypeDefRef ni) (rrLookupTypedef ident >>= goTypeDef)
    goTypeDef :: (RegionResultMonad m, MonadReader NPEPosn m, MonadWriter NPEVictims m)
              => C.TypeDef -> m ()
    goTypeDef td =
      case td of
        C.TypeDef _ident ty _attrs ni ->
          local (NPETypeDefDef ni) (go ty)
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
    checkFunty fty =
      case fty of
        C.FunTypeIncomplete {} ->
          -- analysis is after typechecking, all params seen already.
          error "unexpected FunTypeIncomplete in nakedPtrCheckIdentDecl"
        C.FunType retty params _variadic -> do
          local NPERet $ do
            x <- pointerRegionScheme retty
            case x of
              Just rs | isManagedRegion rs -> tellNPE retty
              _ -> return ()
            -- if return is a function, check its args too
            go retty
          forM_ (zip params [0..]) $ \(param, j) -> do
            let ctx = NPEArg j (C.declName param) (C.nodeInfo param)
            local ctx $ do
              -- first check if the arg is a ptr to managed
              x <- declPtrRegion param
              case x of
                Just rs | isManagedRegion rs -> tellNPE (C.declType param)
                _ -> return ()
               -- then if arg is a function type, check its args
              go (C.declType param)
    tellNPE ty = ask >>= \npe -> tell [NPEVictim ty npe]

analyze :: (RegionResultMonad m, CM.MonadCError m) => Map.Map C.Ident C.IdentDecl -> m ()
analyze m = do
  let (m', _) = C.splitIdentDecls True m
  forM_ m' $ \decl -> do
    ans <- runExceptT (nakedPtrCheckDecl decl)
    case ans of
      Left err -> CM.recordError err
      Right () -> return ()
