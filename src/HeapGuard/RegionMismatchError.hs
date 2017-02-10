module HeapGuard.RegionMismatchError (RegionMismatchError) where

import Data.Monoid ((<>))

import qualified Control.Unification.Types as U

import Language.C.Data.Error as Err
import Language.C.Data.Node (NodeInfo)

import qualified HeapGuard.PrettyPrint as PP
import HeapGuard.PrettyPrint ((<+>))
import HeapGuard.Region
import HeapGuard.RegionUnification.Term
import HeapGuard.RegionUnification.Loc

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
      (bestNode, otherNodes) = getPrimaryNodeInfo e
      msg = PP.render $ PP.vcat [ PP.text "Region mismatch:" <+> PP.pretty (victimRegion v1)
                                  <+> PP.text "and" <+> PP.pretty (victimRegion v2)
                                , if null otherNodes then PP.empty else PP.text "Additional locations:"
                                , PP.nest 8 $ PP.vcat (map PP.prettyPos otherNodes)
                                ]
  changeErrorLevel (RegionMismatchError v1 v2 _) lvl = RegionMismatchError v1 v2 lvl

getErrorLevel :: RegionMismatchError -> ErrorLevel
getErrorLevel (RegionMismatchError _ _ lvl) = lvl

getPrimaryNodeInfo :: RegionMismatchError -> (NodeInfo, [NodeInfo])
getPrimaryNodeInfo (RegionMismatchError v1 v2 _) = bestNodeLocTerm (victimLoc v1 <> victimLoc v2)
