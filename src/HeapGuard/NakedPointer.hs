module HeapGuard.NakedPointer where

import Control.Monad (forM_)

import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.Writer.Class
import Control.Monad.Writer (execWriterT, WriterT)
import Control.Monad.Trans.Class (lift)

import HeapGuard.RegionInferenceResult
import HeapGuard.Region

import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Data.Ident as C
import qualified Language.C.Data.Node as C

import qualified Language.C.Data.Error as CErr


class Monad m => RegionResultMonad m where
  rrStructTagRegion :: StructTagRef -> m RegionScheme
  rrLookupTypedef :: C.Ident -> m C.TypeDef

instance (Monoid w, RegionResultMonad m) => RegionResultMonad (WriterT w m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

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

    
-- a naked pointer to the managed heap in a function declaration. NodeInfo for the return value, or zero or more arguments.
data NakedPointerError = NakedPointerError !C.NodeInfo ![NPEPosn] !CErr.ErrorLevel
  deriving Show

-- trace of an error position
data NPEPosn = NPEArg !Int !NPEPosn -- function argument j
  | NPERet !NPEPosn -- function return value
  | NPEDecl -- a declaration (this is always the end of the error position)
  | NPETypeDefRef !C.NodeInfo !NPEPosn -- a typedef occurrence
  | NPETypeDefDef !C.NodeInfo !NPEPosn -- the typedef declaration
  deriving Show

nakedPtrCheckIdentDecl :: (RegionResultMonad m, MonadReader NPEPosn m,
                            MonadError NakedPointerError m) => C.IdentDecl -> m ()
nakedPtrCheckIdentDecl idcl =
  do
    npes <- execWriterT $ local (const $ NPEDecl) $ go (C.declType idcl)
    case npes of
      [] -> return ()
      _ ->  throwError $ NakedPointerError (C.nodeInfo idcl) npes CErr.LevelError
  where
    go ty =
      case ty of
        C.DirectType {} -> return ()
        C.PtrType ty _qs _attrs -> go ty
        C.ArrayType ty _sz _qs _attrs -> go ty
        C.TypeDefType tdr _qs _attrs -> goTypeDefRef tdr
        C.FunctionType fty _attrs -> checkFunty fty
    goTypeDefRef tdr =
      case tdr of
        C.TypeDefRef _ (Just ty) ni ->
          local (NPETypeDefRef ni) $ go ty
        C.TypeDefRef ident Nothing ni ->
          local (NPETypeDefRef ni) (rrLookupTypedef ident >>= goTypeDef)
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
              Just rs | isManagedRegion rs -> tellNPE
              _ -> return ()
          local NPERet $ go retty -- if return is a function, check its args too
          forM_ (zip params [0..]) $ \(param, j) -> do
            local (NPEArg j) $ do
              x <- declPtrRegion param
              case x of
                Just rs | isManagedRegion rs -> tellNPE
                _ -> return ()
            local (NPEArg j) $ go (C.declType param) -- if ar is a function, check its args
    tellNPE = ask >>= \npe -> tell [npe]
