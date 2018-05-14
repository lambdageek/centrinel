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
import qualified Centrinel.Region.Unification as U
import qualified Centrinel.Region.Unification.Term as U
import Centrinel.Warning (hgWarn)

type RegionIdentMap = Map.Map HGId.RegionIdent U.RegionUnifyTerm

-- | Monad transformer that adds pointer region analysis on top of any underlying monad.
newtype PointerRegionAnalysisT m a = PointerRegionAnalysisT { unPointerRegionAnalysisT :: StateT RegionIdentMap (U.UnifyRegT m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans PointerRegionAnalysisT where
  lift m = PointerRegionAnalysisT (lift $ lift m)

deriving instance AM.MonadName m => AM.MonadName (PointerRegionAnalysisT m)

deriving instance AM.MonadSymtab m => AM.MonadSymtab (PointerRegionAnalysisT m)

instance AM.MonadCError m => AM.MonadCError (PointerRegionAnalysisT m) where
  throwTravError = lift . AM.throwTravError
  catchTravError (PointerRegionAnalysisT c) handler = PointerRegionAnalysisT (AM.catchTravError c (unPointerRegionAnalysisT . handler))
  recordError = lift . AM.recordError
  getErrors = lift AM.getErrors

deriving instance AM.MonadCError m => U.RegionUnification (PointerRegionAnalysisT m)

deriving instance AM.MonadCError m => U.ApplyUnificationState (PointerRegionAnalysisT m)

(-:=) :: (AM.MonadCError m, U.RegionUnification m) => U.RegionVar -> Maybe U.RegionUnifyTerm -> m U.RegionUnifyTerm
v -:= Nothing = return (U.regionUnifyVar v)
v -:= Just r = do
  AM.catchTravError (U.sameRegion (U.regionUnifyVar v) r)
    (\_err -> do
        AM.recordError (hgWarn "failed to unify regions" Nothing) -- TODO: region info
        return (U.regionUnifyVar v))

getRegionIdent :: Monad m => HGId.RegionIdent -> PointerRegionAnalysisT m (Maybe (U.RegionUnifyTerm))
getRegionIdent i = PointerRegionAnalysisT $ State.gets (Map.lookup i)

putRegionIdent :: Monad m => HGId.RegionIdent -> U.RegionUnifyTerm -> PointerRegionAnalysisT m ()
putRegionIdent i m = PointerRegionAnalysisT $ State.modify' (Map.insert i m)

instance AM.MonadCError m => HGId.RegionAssignment (PointerRegionAnalysisT m) where
  assignRegion i = do
    v <- U.newRegion
    r <- getRegionIdent i
    r' <- v -:= r
    putRegionIdent i r'
    return v

-- | Gets a mapping of the region identifiers that have been noted by
-- unification to their 'RegionScheme' as implied by the constraints available
-- at the time of the call.
frozenRegionUnificationState :: AM.MonadCError m => PointerRegionAnalysisT m (Map.Map SUERef RegionScheme)
frozenRegionUnificationState = do
  sueRegions <- PointerRegionAnalysisT $ State.gets munge
  traverse (fmap U.extractRegionScheme . U.applyUnificationState) sueRegions
  where
    munge :: Map.Map HGId.RegionIdent U.RegionUnifyTerm -> Map.Map SUERef U.RegionUnifyTerm
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
