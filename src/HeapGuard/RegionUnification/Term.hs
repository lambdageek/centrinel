module HeapGuard.RegionUnification.Term where

import Data.Functor.Contravariant (Contravariant (..))
import Data.Traversable (fmapDefault, foldMapDefault)

import qualified Control.Unification as U
import qualified Control.Unification.IntVar as U

import HeapGuard.Region (Region)

newtype RegionVar = RegionVar { unRegionVar :: U.IntVar }
  deriving (Show, Eq)

instance U.Variable RegionVar where
  getVarID = U.getVarID . unRegionVar

-- trivial unification terms with no structure
data RegionTerm a = ConstRegionTerm Region
  deriving (Show)

constRegionTerm :: Region -> RegionTerm a
constRegionTerm = ConstRegionTerm

type RegionUnifyTerm = U.UTerm RegionTerm RegionVar

regionUnifyVar :: RegionVar -> RegionUnifyTerm
regionUnifyVar = U.UVar

regionUnifyTerm :: RegionTerm RegionUnifyTerm -> RegionUnifyTerm
regionUnifyTerm = U.UTerm

instance U.Unifiable RegionTerm where
  zipMatch (ConstRegionTerm r1) (ConstRegionTerm r2) | r1 == r2 = Just (ConstRegionTerm r1)
                                                     | otherwise = Nothing

instance Functor RegionTerm where
  fmap = fmapDefault

instance Contravariant RegionTerm where
  contramap _f (ConstRegionTerm r) = ConstRegionTerm r

instance Foldable RegionTerm where
  foldMap = foldMapDefault

instance Traversable RegionTerm where
  traverse _f (ConstRegionTerm r) = pure (ConstRegionTerm r)

