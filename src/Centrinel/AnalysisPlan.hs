-- | Run an analysis plan on a translation unit
module Centrinel.AnalysisPlan (Plan, defaultPlan, runPlan) where

import Control.Monad.Except (ExceptT (..))

import Language.C.Syntax.AST (CTranslUnit)

import Language.C.Data.Error (changeErrorLevel, ErrorLevel(LevelWarn))

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A
import qualified Language.C.Analysis.TravMonad as A

import qualified Centrinel.Trav as HG
import qualified Centrinel.RegionInference as HG
import Centrinel.PointerRegionAnalysis (getInferredStructTagRegions)
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
runPlan :: Monad m => Plan -> NP.AnalysisOpts -> CTranslUnit -> ExceptT CentrinelFatalError m CentrinelAnalysisErrors
runPlan Plan opts = think opts

regionInference :: HG.HGAnalysis s
regionInference = HG.singleHGAnalysis (nonFatal . HG.inferDeclEvent)

-- | Run the "language-c" semantic analysis pass on the given C translation unit and simultaneously
-- apply the region unification algorithm to all structs with a region attribute.
--
-- Return the global declarations from the semantic analysis and a mapping from
-- struct tags to their inferred region schemes.
inferRegions :: CTranslUnit -> HG.HGTrav s (A.GlobalDecls, RegionInferenceResult)
inferRegions u = do
  g <- HG.withHGAnalysis regionInference $ A.analyseAST u
  regions <- HG.hoistPointerRegionAnalysis $ getInferredStructTagRegions
  return (g, regions)

-- | Catch any errors due to the given computation, record them as warnings and continue.
nonFatal :: A.MonadCError m => m () -> m ()
nonFatal comp = A.catchTravError comp (\e -> A.recordError $ changeErrorLevel e LevelWarn)

-- | Given a parsed translation unit and some options, infer the regions for
-- all the pointers and then scan the declarations and definitions to find any
-- uses of raw pointers into the managed region.  Throws a 'CentrinelFatalError' if
-- there was a fatal error, otherwise returns inference results and a list of
-- non-fatal analysis errors.
think :: Monad m => NP.AnalysisOpts -> CTranslUnit -> ExceptT CentrinelFatalError m CentrinelAnalysisErrors
think npOpts u = HG.evalHGTrav $ do
  (g,rir) <- inferRegions u
  NP.runInferenceResultT (nonFatal $ NP.analyze npOpts $ A.gObjs g) (A.gTypeDefs g) rir
  return ()
