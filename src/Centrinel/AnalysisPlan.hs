-- | Run an analysis plan on a translation unit
module Centrinel.AnalysisPlan (Plan, defaultPlan, runPlan) where

import Control.Monad.Except (ExceptT(..), withExceptT)

import Language.C.Syntax.AST (CTranslUnit)

import Language.C.Data.Error (changeErrorLevel, ErrorLevel(LevelWarn))

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A
import qualified Language.C.Analysis.TravMonad as A

import qualified Centrinel.Trav as HG
import qualified Centrinel.RegionInference as HG
import Centrinel.RegionInferenceResult
import Centrinel.Types

import qualified Centrinel.NakedPointer as NP
import qualified Centrinel.Control.Monad.InferenceResult as NP

-- | The plan specifies which analyses to run
data Plan = Plan
  deriving (Show)

-- | The default plan to run
defaultPlan :: Plan
defaultPlan = Plan

-- | Run the given plan
runPlan :: Monad m => Plan -> NP.AnalysisOpts -> CTranslUnit -> ExceptT CentrinelFatalError m ((A.GlobalDecls, RegionInferenceResult), [CentrinelAnalysisError])
runPlan Plan = think

getInferredStructTagRegions :: HG.HGTrav s RegionInferenceResult
getInferredStructTagRegions = makeRegionInferenceResult <$> HG.frozenRegionUnificationState

-- | Run the "language-c" semantic analysis pass on the given C translation unit and simultaneously
-- apply the region unification algorithm to all structs with a region attribute.
--
-- Return the global declarations from the semantic analysis and a mapping from
-- struct tags to their inferred region schemes.
inferRegions :: CTranslUnit -> HG.HGTrav s (A.GlobalDecls, RegionInferenceResult)
inferRegions u = do
  g <- HG.withHGAnalysis (nonFatal . HG.inferDeclEvent) $ A.analyseAST u
  regions <- getInferredStructTagRegions
  return (g, regions)

-- | Catch any errors due to the given computation, record them as warnings and continue.
nonFatal :: A.MonadCError m => m () -> m ()
nonFatal comp = A.catchTravError comp (\e -> A.recordError $ changeErrorLevel e LevelWarn)

-- | Given a parsed translation unit and some options, infer the regions for
-- all the pointers and then scan the declarations and definitions to find any
-- uses of raw pointers into the managed region.  Throws a 'CentrinelFatalError' if
-- there was a fatal error, otherwise returns inference results and a list of
-- non-fatal analysis errors.
think :: Monad m => NP.AnalysisOpts -> CTranslUnit -> ExceptT CentrinelFatalError m ((A.GlobalDecls, RegionInferenceResult), [CentrinelAnalysisError])
think npOpts u = withExceptT CentAbortedAnalysisError $ HG.evalHGTrav $ do
  grir@(g,rir) <- inferRegions u
  NP.runInferenceResultT (nonFatal $ NP.analyze npOpts $ A.gObjs g) (A.gTypeDefs g) rir
  return grir
