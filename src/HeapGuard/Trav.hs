module HeapGuard.Trav where

import Data.Monoid (First(..))

-- data
import qualified Language.C.Data.Ident as Id

-- syntax
import qualified Language.C.Syntax.AST as Syn
import qualified Language.C.Syntax.Constants as Syn

-- semantics
import qualified Language.C.Analysis.TravMonad as AM
import qualified Language.C.Analysis.SemRep as A

-- utils
import qualified Language.C.Pretty as P

import Language.C.Analysis.Debug () -- P.Pretty instances 

import HeapGuard.Warning (hgWarn)
import HeapGuard.Region
import HeapGuard.RegionUnification

extraDecl :: (RegionUnification v m, AM.MonadTrav m) => A.DeclEvent -> m ()
extraDecl e =
  case e of
    A.TagEvent (A.CompDef structTy@(A.CompType suref A.StructTag _ attrs ni)) -> do
      m <- deriveRegionFromMember structTy
      r <- assignRegion suref
      case m of
        Just r' -> sameRegion r' r
        Nothing -> return ()
    _ -> return ()
--      AM.warn (hgWarn ("Found struct " ++  show (P.pretty suref) ++ " with at least one member and region " ++ show r) ni)

assignRegion :: (RegionUnification v m, AM.MonadTrav m) => Id.SUERef -> m v
assignRegion _sueref = newRegion

hasRegionAttr :: A.Attributes -> Maybe Region
hasRegionAttr = getFirst . foldMap (First . from)
  where
    from (A.Attr ident [Syn.CConst (Syn.CIntConst r _)] _ni) | Id.identToString ident == "__region" = Just (Region $ fromInteger $ Syn.getCInteger r)
    from _ = Nothing

deriveRegionFromMember :: (RegionUnification v m) => A.CompType -> m (Maybe v)
deriveRegionFromMember (A.CompType suref A.StructTag (A.MemberDecl (A.VarDecl _varName _dattrs memberType) Nothing _niMember :_) _ _ni) =
  deriveRegionFromType memberType
deriveRegionFromMember _ = return Nothing

deriveRegionFromType :: (RegionUnification v m) => A.Type -> m (Maybe v)
deriveRegionFromType (A.DirectType t _qs _attrs) = deriveRegionFromTypeName t
deriveRegionFromType (A.TypeDefType td _qs _attrs) = deriveRegionFromTypeDefRef td
deriveRegionFromType _ = return Nothing

deriveRegionFromTypeName :: (RegionUnification v m) => A.TypeName -> m (Maybe v)
deriveRegionFromTypeName (A.TyComp (A.CompTypeRef sueref A.StructTag _ni)) = deriveRegionFromSUERef sueref
deriveRegionFromTypeName _ = return Nothing

deriveRegionFromTypeDefRef :: RegionUnification v m => A.TypeDefRef -> m (Maybe v)
deriveRegionFromTypeDefRef (A.TypeDefRef ident Nothing _ni) = lookupTypedefRegion ident
deriveRegionFromTypeDefRef (A.TypeDefRef _ (Just t) _ni) = deriveRegionFromType t

deriveRegionFromSUERef :: RegionUnification v m => Id.SUERef -> m (Maybe v)
deriveRegionFromSUERef (Id.NamedRef ident) = Just <$> lookupStructTagRegion ident
deriveRegionFromSUERef _ = return Nothing

lookupTypedefRegion :: RegionUnification v m => Id.Ident -> m (Maybe v)
lookupTypedefRegion _ = return Nothing

lookupStructTagRegion :: RegionUnification v m => Id.Ident -> m v
lookupStructTagRegion _ = return (error "finish me")
