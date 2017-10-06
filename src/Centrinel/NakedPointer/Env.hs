-- | Defines the environment type for the naked pointer analysis
module Centrinel.NakedPointer.Env where

import Centrinel.NakedPointerError (NPEPosn)

-- | The environment for the naked pointer analysis
data AnalysisEnv = AnalysisEnv
  {
    _analysisPosn :: NPEPosn -- ^ position to report if analysis finds a problem
  , _analysisSuppress :: Bool  -- ^ 'True' if analysis is suppressed by a local pragma
  }

analysisPosn :: Functor f => (NPEPosn -> f NPEPosn) -> AnalysisEnv -> f AnalysisEnv
analysisPosn inj (AnalysisEnv p s) = flip AnalysisEnv s <$> inj p
{-# INLINE analysisPosn #-}

analysisSuppress :: Functor f => (Bool -> f Bool) -> AnalysisEnv -> f AnalysisEnv
analysisSuppress inj (AnalysisEnv p s) = AnalysisEnv p <$> inj s
{-# INLINE analysisSuppress #-}
