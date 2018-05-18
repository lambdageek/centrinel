-- | Type classes for region inference computations
module Centrinel.Region.Class where

import Control.Monad.Trans.Class
import qualified Control.Monad.State.Lazy as StL
import qualified Control.Monad.Reader as Rd

import qualified Language.C.Data.Node as C

import Centrinel.Region.Ident (RegionIdent)
import Centrinel.Region.Region (Region)
import Centrinel.Region.Unification.Term (RegionVar, RegionUnifyTerm)

-- | Computations for unification of region variables with specific regions
class Monad m => RegionUnification m where
  newRegion :: m RegionVar
  sameRegion :: RegionUnifyTerm -> RegionUnifyTerm -> m RegionUnifyTerm
  constantRegion :: RegionVar -> Region -> m ()
  -- | attach a source location to a region variable
  regionAddLocation :: C.CNode n => RegionVar -> n -> m ()

-- | Monads @m@ that assign region variables to C identifiers
class Monad m => RegionAssignment m where
  assignRegion :: RegionIdent -> m RegionVar

instance RegionUnification m => RegionUnification (StL.StateT s m) where
  newRegion = lift newRegion
  sameRegion = \v1 v2 -> lift (sameRegion v1 v2)
  constantRegion = \v r -> lift (constantRegion v r)
  regionAddLocation = \v n -> lift (regionAddLocation v n)

instance RegionUnification m => RegionUnification (Rd.ReaderT r m) where
  newRegion = lift newRegion
  sameRegion = \v1 v2 -> lift (sameRegion v1 v2)
  constantRegion = \v r -> lift (constantRegion v r)
  regionAddLocation = \v n -> lift (regionAddLocation v n)

instance RegionAssignment m => RegionAssignment (Rd.ReaderT r m) where
  assignRegion = lift . assignRegion

instance RegionAssignment m => RegionAssignment (StL.StateT s m) where
  assignRegion = lift . assignRegion

