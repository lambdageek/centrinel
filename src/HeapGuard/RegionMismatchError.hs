module HeapGuard.RegionMismatchError (RegionMismatchError) where

import qualified Control.Unification.Types as U

import Language.C.Data.Error as Err
import Language.C.Data.Node (NodeInfo, undefNode)

import HeapGuard.Region
import HeapGuard.RegionUnification.Term

data RegionMismatchError = RegionMismatchError !Region !Region !NodeInfo !Err.ErrorLevel

instance Show RegionMismatchError where
  show = Err.showError ""

instance U.Fallible RegionTerm RegionVar RegionMismatchError where
  occursFailure _ _ = error "occursFalure cannot happen: RegionTerm contains no unification variables"
  mismatchFailure (ConstRegionTerm r1) (ConstRegionTerm r2) = RegionMismatchError r1 r2 undefNode Err.LevelError -- FIXME: drop some nodeInfo into the terms and bring it here.

instance Err.Error RegionMismatchError where
  errorInfo e@(RegionMismatchError r1 r2 _ni _vl) = Err.mkErrorInfo (getErrorLevel e) msg (getPrimaryNodeInfo e)
    where
      msg = "Region mismatch: " ++ show r1 ++ " and " ++ show r2
  changeErrorLevel (RegionMismatchError r1 r2 ni _) lvl = RegionMismatchError r1 r2 ni lvl

getErrorLevel :: RegionMismatchError -> ErrorLevel
getErrorLevel (RegionMismatchError _ _ _ lvl) = lvl

-- eventually want to have NodeInfo in the RegionTerms, and some way to judge
-- which one is "better" (ie one is the original struct def with an annotation,
-- another is a conflicting derived type, etc).
getPrimaryNodeInfo :: RegionMismatchError -> NodeInfo
getPrimaryNodeInfo (RegionMismatchError _ _ ni _) = ni
