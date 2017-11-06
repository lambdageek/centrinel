module Centrinel.RegionMismatchError (
  -- * Region mismatch error
  RegionMismatchError
  , regionMismatchVictims
  -- * Region mismatch victims
  , RegionVictim (..)
    -- * Unification failure positions
  , extractVictimLocations
) where

import Data.Monoid ((<>))

import qualified Control.Unification.Types as U

import Language.C.Data.Error as Err
import Language.C.Data.Node (NodeInfo)

import qualified Centrinel.PrettyPrint as PP
import Centrinel.PrettyPrint ((<+>))
import Centrinel.Region.Region
import Centrinel.Region.Unification.Term
import Centrinel.Region.Unification.Loc

data RegionMismatchError = RegionMismatchError !RegionVictim !RegionVictim !Err.ErrorLevel

-- the inferred region and a source node
data RegionVictim = RegionVictim { victimRegion :: !Region
                                 , victimLoc :: !LocTerm
                                 }

instance Show RegionMismatchError where
  show = Err.showError ""

mismatchFailureUnexpectedHere :: a
mismatchFailureUnexpectedHere = error "mismatchFailure with DummyLocTerm arg cannot happen - it always unifies"

instance U.Fallible RegionTerm RegionVar RegionMismatchError where
  occursFailure _ _ = error "occursFalure cannot happen: RegionTerm contains no unification variables"
  mismatchFailure _ (DummyLocTerm {}) = mismatchFailureUnexpectedHere
  mismatchFailure (DummyLocTerm {}) _ = mismatchFailureUnexpectedHere
  mismatchFailure (ConstRegionTerm r1 l1) (ConstRegionTerm r2 l2) =
    RegionMismatchError victim1 victim2 Err.LevelError
    where
      victim1 = RegionVictim r1 l1
      victim2 = RegionVictim r2 l2

instance Err.Error RegionMismatchError where
  errorInfo e@(RegionMismatchError v1 v2 _vl) = Err.mkErrorInfo (getErrorLevel e) msg bestNode
    where
      (bestNode, otherNodes) = extractVictimLocations (v1,v2)
      msg = PP.render $ PP.vcat [ PP.text "Region mismatch:" <+> PP.pretty (victimRegion v1)
                                  <+> PP.text "and" <+> PP.pretty (victimRegion v2)
                                , if null otherNodes then PP.empty else PP.text "Additional locations:"
                                , PP.nest 8 $ PP.vcat (map PP.prettyPos otherNodes)
                                ]
  changeErrorLevel (RegionMismatchError v1 v2 _) lvl = RegionMismatchError v1 v2 lvl

getErrorLevel :: RegionMismatchError -> ErrorLevel
getErrorLevel (RegionMismatchError _ _ lvl) = lvl

-- | Given two mismatched regions, extract the best location
-- responsible for the mismatch and the list of the other locations
-- attributed to either victim.  (The notion of "best" is somewhat
-- imprecise and dependent on the order of contraints created during
-- unification)
extractVictimLocations :: (RegionVictim, RegionVictim) -> (NodeInfo, [NodeInfo])
extractVictimLocations (v1, v2) = bestNodeLocTerm (victimLoc v1 <> victimLoc v2)

regionMismatchVictims :: RegionMismatchError -> (RegionVictim, RegionVictim)
regionMismatchVictims (RegionMismatchError v1 v2 _) = (v1, v2)
