module Centrinel where

import Control.Monad.Except (ExceptT(..), withExceptT)

import Language.C.Parser (parseC)

import Language.C.Syntax.AST (CTranslUnit)

import Language.C.Data.Error (CError, changeErrorLevel, ErrorLevel(LevelWarn))
import Language.C.Data.Position (initPos)

import Language.C.System.GCC (GCC)
import qualified Language.C.System.Preprocess as CPP

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A
import qualified Language.C.Analysis.TravMonad as A

import qualified Centrinel.Trav as HG
import qualified Centrinel.RegionInference as HG
import Centrinel.RegionInferenceResult
import Centrinel.Types

import qualified Centrinel.NakedPointer as NP
import qualified Centrinel.Control.Monad.InferenceResult as NP

import qualified Centrinel.Util.Datafiles as HGData

makeNakedPointerOpts :: FilePath -> NP.AnalysisOpts
makeNakedPointerOpts fp = NP.AnalysisOpts {NP.analysisOptFilterPath = Just fp }

-- | Remove any 'CPP.outputFile' options, and add
-- preprocessor defines and definitions for Centrinel to successfully parse and
-- analize the specified input file.
cppArgsForCentrinel :: CPP.CppArgs -> HGData.Datafiles -> CPP.CppArgs
cppArgsForCentrinel cppArgs datafiles =
  let centrinelHeader = HGData.datafileCentrinelHeader datafiles
  in cppArgs
     { CPP.cppOptions = CPP.cppOptions cppArgs ++ [ CPP.IncludeFile centrinelHeader ]
     , CPP.outputFile = Nothing
     }

parseCFile :: CPP.Preprocessor cpp => cpp -> CPP.CppArgs -> ExceptT CentrinelError IO CTranslUnit
{-# specialize parseCFile :: GCC -> CPP.CppArgs -> ExceptT CentrinelError IO CTranslUnit #-}
parseCFile cpp cppArgs = do
    inputStream <- withExceptT CentCPPError $ ExceptT $ CPP.runPreprocessor cpp cppArgs
    withExceptT CentParseError $ ExceptT $ return $ parseC inputStream (initPos $ CPP.inputFile cppArgs)

getInferredRegions :: A.GlobalDecls -> HG.HGTrav s RegionInferenceResult
getInferredRegions g = do
  let structDefs = HG.justStructTagDefs (A.gTags g)
  makeRegionInferenceResult <$> traverse HG.applyBindingTagDef structDefs

inferRegions :: CTranslUnit -> HG.HGTrav s (A.GlobalDecls, RegionInferenceResult)
inferRegions u = do
  g <- HG.withHGAnalysis (nonFatal . HG.inferDeclEvent) $ A.analyseAST u
  regions <- getInferredRegions g
  return (g, regions)
  where
    -- catch any errors due to this declaration, record them and continue.
    nonFatal :: A.MonadCError m => m () -> m ()
    nonFatal comp = A.catchTravError comp (\e -> A.recordError $ changeErrorLevel e LevelWarn)

think :: Monad m => NP.AnalysisOpts -> CTranslUnit -> ExceptT CentrinelError m ((A.GlobalDecls, RegionInferenceResult), [CError])
think npOpts u = withExceptT CentAnalysisError $ HG.evalHGTrav $ do
  grir@(g,rir) <- inferRegions u
  NP.runInferenceResultT (NP.analyze npOpts $ A.gObjs g) (A.gTypeDefs g) rir
  return grir

-- | Run the preprocessor with the given arguments, parse the result and run
-- the Centrinel analysis.
runCentrinel :: CPP.Preprocessor cpp => HGData.Datafiles -> cpp -> CPP.CppArgs -> ExceptT CentrinelError IO ((), [CentrinelAnalysisError])
{-# specialize runCentrinel :: HGData.Datafiles -> GCC -> CPP.CppArgs -> ExceptT CentrinelError IO ((), [CentrinelAnalysisError]) #-}
runCentrinel datafiles cpp cppArgs_ = do
  let cppArgs = cppArgsForCentrinel cppArgs_ datafiles
  ast <- parseCFile cpp cppArgs
  let opts = makeNakedPointerOpts (CPP.inputFile cppArgs)
  fmap (\(_, warns) -> ((), warns)) (think opts ast)


