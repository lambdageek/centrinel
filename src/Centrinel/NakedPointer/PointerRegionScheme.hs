-- | Utility class to find the 'RegionScheme' of a pointer type
module Centrinel.NakedPointer.PointerRegionScheme (
  PointerRegionScheme(..)
  ) where

import qualified Language.C.Analysis.SemRep as C

import Centrinel.Control.Monad.Class.RegionResult
import Centrinel.Region.Region (RegionScheme(..))
import Centrinel.RegionInferenceResult (StructTagRef(..))

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


