{-# LANGUAGE FunctionalDependencies #-}
module HeapGuard.RegionInference (inferDeclEvent, hasRegionAttr) where

import Data.Monoid (First(..))

-- data
import qualified Language.C.Data.Ident as Id

-- syntax
import qualified Language.C.Syntax.AST as Syn
import qualified Language.C.Syntax.Constants as Syn

-- semantics
import qualified Language.C.Analysis.TravMonad as AM
import qualified Language.C.Analysis.SemRep as A

import Language.C.Analysis.Debug () -- P.Pretty instances 

import HeapGuard.Region
import HeapGuard.RegionUnification
import HeapGuard.RegionIdent

inferDeclEvent :: (RegionAssignment RegionIdent v m, RegionUnification v m, AM.MonadTrav m) => A.DeclEvent -> m ()
inferDeclEvent e =
  case e of
    A.TagEvent (A.CompDef structTy@(A.CompType suref A.StructTag _ _attrs _ni)) -> do
      m <- deriveRegionFromMember structTy
      r <- assignRegion (StructTagId suref)
      case m of
        Just r' -> sameRegion r' r
        Nothing -> return ()
    A.TypeDefEvent (A.TypeDef typedefIdent ty _ _) -> do
      m <- deriveRegionFromType ty
      r <- assignRegion (TypedefId typedefIdent)
      case m of
        Just r' -> sameRegion r' r
        Nothing -> return ()
    _ -> return ()

hasRegionAttr :: A.Attributes -> Maybe Region
hasRegionAttr = getFirst . foldMap (First . from)
  where
    from (A.Attr ident [Syn.CConst (Syn.CIntConst r _)] _ni) | Id.identToString ident == "__region" = Just (Region $ fromInteger $ Syn.getCInteger r)
    from _ = Nothing

deriveRegionFromMember :: (RegionAssignment RegionIdent v m, RegionUnification v m) => A.CompType -> m (Maybe v)
deriveRegionFromMember (A.CompType _suref A.StructTag (A.MemberDecl (A.VarDecl _varName _dattrs memberType) Nothing _niMember :_) _ _ni) =
  deriveRegionFromType memberType
deriveRegionFromMember _ = return Nothing

deriveRegionFromType :: (RegionAssignment RegionIdent v m, RegionUnification v m) => A.Type -> m (Maybe v)
deriveRegionFromType (A.DirectType t _qs _attrs) = deriveRegionFromTypeName t
deriveRegionFromType (A.TypeDefType td _qs _attrs) = deriveRegionFromTypeDefRef td
deriveRegionFromType _ = return Nothing

deriveRegionFromTypeName :: (RegionAssignment RegionIdent v m, RegionUnification v m) => A.TypeName -> m (Maybe v)
deriveRegionFromTypeName (A.TyComp (A.CompTypeRef sueref A.StructTag _ni)) = Just <$> deriveRegionFromSUERef sueref
deriveRegionFromTypeName _ = return Nothing

deriveRegionFromTypeDefRef :: (RegionAssignment RegionIdent v m, RegionUnification v m) => A.TypeDefRef -> m (Maybe v)
deriveRegionFromTypeDefRef (A.TypeDefRef ident Nothing _ni) = Just <$> lookupTypedefRegion ident
deriveRegionFromTypeDefRef (A.TypeDefRef _ (Just t) _ni) = deriveRegionFromType t

deriveRegionFromSUERef :: (RegionAssignment RegionIdent v m, RegionUnification v m) => Id.SUERef -> m v
deriveRegionFromSUERef suref = assignRegion (StructTagId suref)

lookupTypedefRegion :: (RegionAssignment RegionIdent v m, RegionUnification v m) => Id.Ident -> m v
lookupTypedefRegion ident = assignRegion (TypedefId ident)

