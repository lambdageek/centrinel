-- | Analyze a parsed C file and create region unification constraints
--
{-# LANGUAGE FunctionalDependencies #-}
module Centrinel.RegionInference (inferDeclEvent, hasRegionAttr) where

import Control.Monad (void)
import Data.Monoid (First(..))

-- data
import qualified Language.C.Data.Ident as Id
import qualified Language.C.Data.Node as C

-- syntax
import qualified Language.C.Syntax.AST as Syn
import qualified Language.C.Syntax.Constants as Syn

-- semantics
import qualified Language.C.Analysis.SemRep as A

import Centrinel.Region.Region
import Centrinel.Region.Class
import Centrinel.Region.Unification.Term (RegionVar, regionUnifyVar)
import Centrinel.Region.Ident

inferDeclEvent :: (RegionAssignment m, RegionUnification m) => A.DeclEvent -> m ()
inferDeclEvent e =
  case e of
    A.TagEvent (A.CompDef structTy@(A.CompType suref A.StructTag _ attrs ni)) -> do
      -- Effect order matters here: first add the location from the attribute,
      -- then try to unify with region from the first member.  That way we have
      -- both if unification fails.
      r <- assignRegion (StructTagId suref)
      unifyWithAttrs r attrs ni
      m <- deriveRegionFromMember structTy
      case m of
        Just r' -> void $ sameRegion (regionUnifyVar r') (regionUnifyVar r)
        Nothing -> return ()
    A.TypeDefEvent (A.TypeDef typedefIdent ty attrs ni) -> do
      m <- deriveRegionFromType ty
      r <- assignRegion (TypedefId typedefIdent)
      unifyWithAttrs r attrs ni
      case m of
        Just r' -> void $ sameRegion (regionUnifyVar r') (regionUnifyVar r)
        Nothing -> return ()
    _ -> return ()

-- | @unifyWithAttrs r attr ni@ unifies the region @r@ with any regions attributes found among @attr@
-- and adds the location of @ni@ to the occurrences of @r@.
unifyWithAttrs :: (C.CNode n, RegionUnification m) => RegionVar -> A.Attributes -> n -> m ()
unifyWithAttrs r attrs ni =
  case hasRegionAttr attrs of
    Just rc -> do
      constantRegion r rc
      regionAddLocation r ni
    Nothing -> return ()

hasRegionAttr :: A.Attributes -> Maybe Region
hasRegionAttr = getFirst . foldMap (First . from)
  where
    from (A.Attr ident [Syn.CConst (Syn.CIntConst r _)] _ni) | Id.identToString ident == "__region" =
                                                               Just (Region $ fromInteger $ Syn.getCInteger r)
    from _ = Nothing

withLocation :: (RegionUnification m, C.CNode n) => n -> Maybe RegionVar -> m (Maybe RegionVar)
withLocation ni m = do
  case m of
    Just r -> regionAddLocation r ni
    Nothing -> return ()
  return m

deriveRegionFromMember :: (RegionAssignment m, RegionUnification m) => A.CompType -> m (Maybe RegionVar)
deriveRegionFromMember (A.CompType _suref A.StructTag (A.MemberDecl (A.VarDecl _varName _dattrs memberType) Nothing niMember :_) _ _ni) =
  deriveRegionFromType memberType >>= withLocation niMember
deriveRegionFromMember _ = return Nothing

deriveRegionFromType :: (RegionAssignment m, RegionUnification m) => A.Type -> m (Maybe RegionVar)
deriveRegionFromType (A.DirectType t _qs _attrs) =
  -- the _attrs here don't seem to work when, for example, we have
  --   typedef struct __attribute__((...)) TagName TypeDefName;
  -- (not clear if that's to be expected, or a language-c bug).
  deriveRegionFromTypeName t
deriveRegionFromType (A.TypeDefType td _qs _attrs) = deriveRegionFromTypeDefRef td
deriveRegionFromType _ = return Nothing

deriveRegionFromTypeName :: (RegionAssignment m) => A.TypeName -> m (Maybe RegionVar)
deriveRegionFromTypeName (A.TyComp (A.CompTypeRef sueref A.StructTag _ni)) = Just <$> deriveRegionFromSUERef sueref
deriveRegionFromTypeName _ = return Nothing

deriveRegionFromTypeDefRef :: (RegionAssignment m, RegionUnification m) => A.TypeDefRef -> m (Maybe RegionVar)
deriveRegionFromTypeDefRef (A.TypeDefRef _ t _ni) = deriveRegionFromType t

deriveRegionFromSUERef :: (RegionAssignment m) => Id.SUERef -> m RegionVar
deriveRegionFromSUERef suref = assignRegion (StructTagId suref)

