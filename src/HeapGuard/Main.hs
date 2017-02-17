module HeapGuard.Main where

import Control.Monad (void)
import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName, lookupEnv)

import HeapGuard (parseCFile, think', cppArgsForHeapGuard, makeNakedPointerOpts)
import HeapGuard.System.RunLikeCC (runLikeCC)
import qualified HeapGuard.Util.Datafiles as HGData

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
  runHeapGuard datafiles gcc cppArgs

-- | Look for "REAL_CC" environment variable and return that path, if unset, return "cc"
getCC :: IO FilePath
getCC = maybe "cc" id <$> lookupEnv "REAL_CC"

-- | Run the preprocessor with the given arguments, parse the result and run
-- the HeapGuard analysis.
runHeapGuard :: Preprocessor cpp => HGData.Datafiles -> cpp -> CppArgs -> IO ()
{-# specialize runHeapGuard :: HGData.Datafiles -> GCC -> CppArgs -> IO () #-}
runHeapGuard datafiles cpp cppArgs_ = do
  let cppArgs = cppArgsForHeapGuard cppArgs_ datafiles
  ast <- do
    x <- parseCFile cpp cppArgs
    case x of
      Left err -> do
        print err
        exitFailure
      Right ast -> return ast
  let opts = makeNakedPointerOpts (CPP.inputFile cppArgs)
  void $ think' opts ast
