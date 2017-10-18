module Main where

import System.Exit (ExitCode(ExitSuccess))

import Test.Tasty
import Test.Tasty.HUnit

import qualified Centrinel as C
import Centrinel.System.RunLikeCC (runLikeCC, ParsedCC(..))
import Language.C.System.GCC (newGCC)
import qualified Centrinel.Util.Datafiles as CData



main :: IO ()
main = defaultMain smokeTests


smokeTests :: TestTree
smokeTests = testGroup "Smoke Tests"
  [ testGroup "Examples run"
    [ assertRunsTestCase "c-examples/incl.c"
    ]
  ]

assertRunsTestCase :: FilePath -> TestTree
assertRunsTestCase fp = testCase (fp ++ " runs") cmd
  where
    cmd = do
      case runLikeCC gcc [fp] of
        ParsedCC args [] -> do
          ec <- C.report $ C.runCentrinel datafiles gcc args
          assertEqual "exit code" ExitSuccess ec
        NoInputFilesCC -> assertFailure $ "expected input files in smoketest " ++ fp
        ErrorParsingCC err -> assertFailure $ "unexpected parse error \"" ++ err ++ "\" in smoketest " ++ fp
        ParsedCC _args ignoredArgs -> assertFailure $ "unepxected ignored args " ++ show ignoredArgs ++ " in smoketest " ++ fp
    gcc = newGCC "cc"
    datafiles = CData.Datafiles "include/centrinel.h"

