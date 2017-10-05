module Centrinel.Region.Unification.Term where

import Data.Functor.Contravariant (Contravariant (..))
import Data.Monoid ((<>), Monoid(..))
import Data.Traversable (fmapDefault, foldMapDefault)

import qualified Control.Unification as U
import qualified Control.Unification.IntVar as U

import qualified Language.C.Data.Node as C

import Centrinel.Region.Region (Region)

import Centrinel.Region.Unification.Loc


newtype RegionVar = RegionVar { unRegionVar :: U.IntVar }
  deriving (Show, Eq)

instance U.Variable RegionVar where
  getVarID = U.getVarID . unRegionVar

-- | Terms for region unification.
-- This is slighlty non-trivial in order to bring along source location information.
data RegionTerm a =
  -- | A region constant
  ConstRegionTerm !Region !LocTerm
  -- | A dummy term that doesn't constrain the region, but just carries some source location info.
  -- (Unifying with a DummyLocTerm always succeeds and incorporates the additional source info into the other term).
  | DummyLocTerm !LocTerm
  deriving (Show)

constRegionTerm :: Region -> RegionTerm a
constRegionTerm r = ConstRegionTerm r mempty

dummyLocationTerm :: C.NodeInfo -> RegionTerm a
dummyLocationTerm l = DummyLocTerm (locSingleton l)

type RegionUnifyTerm = U.UTerm RegionTerm RegionVar

regionUnifyVar :: RegionVar -> RegionUnifyTerm
regionUnifyVar = U.UVar

regionUnifyTerm :: RegionTerm RegionUnifyTerm -> RegionUnifyTerm
regionUnifyTerm = U.UTerm

instance U.Unifiable RegionTerm where
  zipMatch (ConstRegionTerm r1 l1) (ConstRegionTerm r2 l2) | r1 == r2 = Just (ConstRegionTerm r1 (l1 <> l2))
                                                           | otherwise = Nothing
  zipMatch t1 (DummyLocTerm l2) = Just (additionalLocTerm t1 l2)
  zipMatch (DummyLocTerm l1) t2 = Just (additionalLocTerm t2 l1)

additionalLocTerm :: RegionTerm a -> LocTerm -> RegionTerm b
additionalLocTerm (ConstRegionTerm r l1) l2 = ConstRegionTerm r (l1 <> l2)
additionalLocTerm (DummyLocTerm l1) l2 = DummyLocTerm (l1 <> l2)

instance Functor RegionTerm where
  fmap = fmapDefault

instance Contravariant RegionTerm where
  contramap _f (ConstRegionTerm r l) = ConstRegionTerm r l
  contramap _f (DummyLocTerm l) = DummyLocTerm l

instance Foldable RegionTerm where
  foldMap = foldMapDefault

instance Traversable RegionTerm where
  traverse _f (ConstRegionTerm r l) = pure (ConstRegionTerm r l)
  traverse _f (DummyLocTerm l) = pure (DummyLocTerm l)

