module Centrinel.Main where

import Control.Monad (when, liftM)

import System.Exit (exitWith, exitSuccess, exitFailure)
import System.Environment (lookupEnv)

import Centrinel (runCentrinel, report)
import Centrinel.System.RunLikeCC (runLikeCC, ParsedCC(..))
import qualified Centrinel.Util.Datafiles as HGData

import Language.C.System.GCC (newGCC)

import Centrinel.Debug.PrettyCppArgs (showCppArgs)

type CppArgStrings = [String]

-- | Centrinel command to run
data CentrinelCmd =
  -- | Run Centrinel on a single C source file.
  RunOneCentrinelCmd CentrinelOptions CppArgStrings

data CentrinelOptions = CentrinelOptions

-- | Run the main computation
--
-- Usage: @:main /preprocessor opts/ FILE.c
--
-- The /preprocessor opts/ may include all the usual arguments to @cc(1)@.  The program
-- will pick out the relevant ones (such as @-I@ and @-D@) and ignore the rest
--
-- >>> :main c-examples/attrib.c
--
main :: CentrinelCmd -> IO ()
main cmd =
  case cmd of
    RunOneCentrinelCmd CentrinelOptions args -> do
      gcc <- liftM newGCC getCC
      cppArgs <- case runLikeCC gcc args of
        NoInputFilesCC -> exitSuccess -- nothing to do
        ErrorParsingCC err -> do
          putStrLn $ "error parsing cc arguments: " ++ err
          exitFailure
        ParsedCC cppArgs ignoredArgs -> do
          let debugging = True
          when (debugging && not (null ignoredArgs)) $ do
            putStrLn "Ignored args:"
            mapM_ (putStrLn . ("\t"++)) ignoredArgs
          putStrLn (showCppArgs cppArgs)
          return cppArgs
      datafiles <- HGData.getDatafiles
      n <- report (runCentrinel datafiles gcc cppArgs)
      exitWith n

-- | Look for "REAL_CC" environment variable and return that path, if unset, return "cc"
getCC :: IO FilePath
getCC = maybe "cc" id <$> lookupEnv "REAL_CC"
