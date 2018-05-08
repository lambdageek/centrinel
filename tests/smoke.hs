module Main where

import Control.Monad.Except (ExceptT)

import Test.Tasty
import Test.Tasty.HUnit

import Language.C.System.GCC (GCC, newGCC)
import qualified Language.C.System.Preprocess as CPP

import Centrinel.Types
import Centrinel.AnalysisPlan (defaultPlan, runPlan)
import qualified Centrinel.Report as C
import qualified Centrinel.NakedPointer as NP
import Centrinel.System.ParseCFile (parseCFile)
import Centrinel.System.RunLikeCC (runLikeCC, ParsedCC(..), cppArgsForCentrinel)
import qualified Centrinel.Util.Datafiles as CData

main :: IO ()
main = defaultMain smokeTests


smokeTests :: TestTree
smokeTests = testGroup "Smoke Tests"
  [ testGroup "Examples run"
    [ assertRunsTestCase "c-examples/incl.c"
    , assertRunsTestCase "c-examples/c99.c"
    ]
  ]

assertRunsTestCase :: FilePath -> TestTree
assertRunsTestCase fp = testCase (fp ++ " runs") cmd
  where
    cmd = do
      case runLikeCC gcc [fp] of
        ParsedCC args [] -> do
          ec <- C.report C.defaultOutputMethod fp $ runCentrinel datafiles gcc args
          assertEqual "exit code" (Just ()) (const () <$> ec) -- throw away analysis results
        NoInputFilesCC -> assertFailure $ "expected input files in smoketest " ++ fp
        ErrorParsingCC err -> assertFailure $ "unexpected parse error \"" ++ err ++ "\" in smoketest " ++ fp
        ParsedCC _args ignoredArgs -> assertFailure $ "unepxected ignored args " ++ show ignoredArgs ++ " in smoketest " ++ fp
    gcc = newGCC "cc"
    datafiles = CData.Datafiles "include/centrinel.h"

-- | Run the preprocessor with the given arguments, parse the result and run
-- the Centrinel analysis.
runCentrinel :: CPP.Preprocessor cpp => CData.Datafiles -> cpp -> CPP.CppArgs -> ExceptT CentrinelFatalError IO ((), [CentrinelAnalysisError])
{-# specialize runCentrinel :: CData.Datafiles -> GCC -> CPP.CppArgs -> ExceptT CentrinelFatalError IO ((), [CentrinelAnalysisError]) #-}
runCentrinel datafiles cpp cppArgs_ = do
  let cppArgs = cppArgsForCentrinel cppArgs_ datafiles
  ast <- parseCFile cpp cppArgs
  let opts = makeNakedPointerOpts (CPP.inputFile cppArgs)
  fmap (\(_, warns) -> ((), warns)) (runPlan defaultPlan opts ast)

makeNakedPointerOpts :: FilePath -> NP.AnalysisOpts
makeNakedPointerOpts fp = NP.AnalysisOpts {NP.analysisOptFilterPath = Just fp }
