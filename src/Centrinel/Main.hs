module Centrinel.Main where

import Control.Monad (liftM)

import System.Exit (exitWith)
import System.Environment (getArgs, getProgName, lookupEnv)

import Centrinel (runCentrinel, report)
import Centrinel.System.RunLikeCC (runLikeCC)
import qualified Centrinel.Util.Datafiles as HGData

import Language.C.System.GCC (newGCC)

-- | Run the main computation
--
-- Usage: @:main /preprocessor opts/ FILE.c
--
-- The /preprocessor opts/ may include all the usual arguments to @cc(1)@.  The program
-- will pick out the relevant ones (such as @-I@ and @-D@) and ignore the rest
--
-- >>> :main c-examples/attrib.c
--
main :: IO ()
main = do
  progName <- getProgName
  gcc <- liftM newGCC getCC
  cppArgs <- getArgs >>= runLikeCC progName gcc
  datafiles <- HGData.getDatafiles
  n <- report (runCentrinel datafiles gcc cppArgs)
  exitWith n

-- | Look for "REAL_CC" environment variable and return that path, if unset, return "cc"
getCC :: IO FilePath
getCC = maybe "cc" id <$> lookupEnv "REAL_CC"
