module Centrinel.Main where

import Control.Monad (void)
import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName, lookupEnv)

import Centrinel (parseCFile, think', cppArgsForCentrinel, makeNakedPointerOpts)
import Centrinel.System.RunLikeCC (runLikeCC)
import qualified Centrinel.Util.Datafiles as HGData

import Language.C.System.Preprocess (Preprocessor, CppArgs)
import qualified Language.C.System.Preprocess as CPP
import Language.C.System.GCC (GCC, newGCC)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  cc <- getCC
  let gcc = newGCC cc
  cppArgs <- runLikeCC progName gcc args
  datafiles <- HGData.getDatafiles
  runCentrinel datafiles gcc cppArgs

-- | Look for "REAL_CC" environment variable and return that path, if unset, return "cc"
getCC :: IO FilePath
getCC = maybe "cc" id <$> lookupEnv "REAL_CC"

-- | Run the preprocessor with the given arguments, parse the result and run
-- the Centrinel analysis.
runCentrinel :: Preprocessor cpp => HGData.Datafiles -> cpp -> CppArgs -> IO ()
{-# specialize runCentrinel :: HGData.Datafiles -> GCC -> CppArgs -> IO () #-}
runCentrinel datafiles cpp cppArgs_ = do
  let cppArgs = cppArgsForCentrinel cppArgs_ datafiles
  ast <- do
    x <- parseCFile cpp cppArgs
    case x of
      Left err -> do
        print err
        exitFailure
      Right ast -> return ast
  let opts = makeNakedPointerOpts (CPP.inputFile cppArgs)
  void $ think' opts ast
