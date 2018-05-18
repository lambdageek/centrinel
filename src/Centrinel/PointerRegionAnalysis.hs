-- | Pointer region analysis pass monad
{-# language GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Centrinel.PointerRegionAnalysis (PointerRegionAnalysisT (..)
                                       , RegionIdentMap
                                       , getInferredStructTagRegions
                                       , evalPointerRegionAnalysisT
                                       ) where

import Control.Monad.Trans.Class
import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State

import qualified Data.Map.Lazy as Map

import Language.C.Data.Ident (SUERef)
import qualified Language.C.Analysis.TravMonad as AM

import Language.C.Analysis.TravMonad.Instances ()

import qualified Centrinel.Region.Ident as HGId
import Centrinel.Region.Region (RegionScheme)
import Centrinel.RegionInferenceResult
import qualified Centrinel.Region.Class as U
import qualified Centrinel.Region.Unification as U
import qualified Centrinel.Region.Unification.Term as U

type RegionIdentMap = Map.Map HGId.RegionIdent U.RegionVar

-- | Monad transformer that adds pointer region analysis on top of any underlying monad.
newtype PointerRegionAnalysisT m a = PointerRegionAnalysisT { unPointerRegionAnalysisT :: StateT RegionIdentMap (U.UnifyRegT m) a }
  deriving (Functor, Applicative, Monad
           , AM.MonadName, AM.MonadSymtab, AM.MonadCError
           , U.RegionUnification)

instance MonadTrans PointerRegionAnalysisT where
  lift = PointerRegionAnalysisT . lift . lift

instance AM.MonadCError m => U.RegionAssignment (PointerRegionAnalysisT m) where
  assignRegion i = PointerRegionAnalysisT $ do
    m <- State.gets (Map.lookup i)
    case m of
      Nothing -> do
        v <- U.newRegion
        State.modify' (Map.insert i v)
        return v
      Just v -> return v

-- | Gets a mapping of the region identifiers that have been noted by
-- unification to their 'RegionScheme' as implied by the constraints available
-- at the time of the call.
frozenRegionUnificationState :: AM.MonadCError m => PointerRegionAnalysisT m (Map.Map SUERef RegionScheme)
frozenRegionUnificationState = do
  sueRegions <- PointerRegionAnalysisT $ State.gets munge
  traverse applyUnificationResults sueRegions
  where
    applyUnificationResults :: AM.MonadCError m => U.RegionVar -> PointerRegionAnalysisT m RegionScheme
    applyUnificationResults v = do
      t <- PointerRegionAnalysisT $ lift $ U.applyUnificationState $ U.regionUnifyVar v
      return (U.extractRegionScheme t)
    munge :: RegionIdentMap -> Map.Map SUERef U.RegionVar
    munge = Map.mapKeysMonotonic onlySUERef . Map.filterWithKey (\k -> const (isStructTag k))
    onlySUERef :: HGId.RegionIdent -> SUERef
    onlySUERef (HGId.StructTagId sue) = sue
    onlySUERef (HGId.TypedefId {}) = error "unexpected TypedefId in onlySUERef"
    isStructTag :: HGId.RegionIdent -> Bool
    isStructTag (HGId.StructTagId {}) = True
    isStructTag (HGId.TypedefId {}) = False

getInferredStructTagRegions :: AM.MonadCError m => PointerRegionAnalysisT m RegionInferenceResult
getInferredStructTagRegions = makeRegionInferenceResult <$> frozenRegionUnificationState

evalPointerRegionAnalysisT :: Monad m => PointerRegionAnalysisT m a -> m a
evalPointerRegionAnalysisT comp = U.runUnifyRegT (State.evalStateT (unPointerRegionAnalysisT comp) Map.empty)
