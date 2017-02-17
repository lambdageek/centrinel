-- | Parse command line arguments like the given preprocessor
module HeapGuard.System.RunLikeCC (
  -- * Command line parsing
  runLikeCC
  -- * Utilities
  , usage
  , anySourceArgs
  -- * Re-exported definitons
  , Preprocessor
  , GCC
  , newGCC
  , CppArgs) where

import Control.Monad (when, unless)
import qualified Data.List
import Data.Monoid (Any(..))

import System.Exit (exitSuccess, exitFailure)

import Language.C.System.Preprocess (Preprocessor(..), CppArgs)
import Language.C.System.GCC (GCC, newGCC)

import HeapGuard.Debug.PrettyCppArgs (showCppArgs)

-- | @runLikeCC progName gcc args@ parses the command line arguments like the
-- given preprocessor.
-- @progName@ is used in error messages.
runLikeCC :: Preprocessor cpp => FilePath -> cpp -> [String] -> IO CppArgs
{-# specialize runLikeCC :: FilePath -> GCC -> [String] -> IO CppArgs #-}
runLikeCC progName cpp args = do
  when (null args) (putStrLn $ usage progName)
  -- if there's no source file among the arguments,
  -- (if we're called from the linking step), do nothing.
  unless (getAny $ anySourceArgs args) exitSuccess
  (cppArgs, ignoredArgs) <- case parseCPPArgs cpp args of
    Left err -> do
      putStrLn $ progName ++ ": error parsing cc arguments: " ++ err
      exitFailure
    Right ok -> return ok
  let debugging = True
  when debugging $ do
    unless (null ignoredArgs) $ do
      putStrLn "Ignored args:"
      mapM_ (putStrLn . ("\t"++)) ignoredArgs
    putStrLn (showCppArgs cppArgs)
  return cppArgs


-- | Returns @Any True@ iff any of the given strings looks like a source file.
-- It uses a pretty rough heuristic: the string must not begin with a @'-'@ and
-- it must end with @".c"@.  This seems good enough.
anySourceArgs :: [String] -> Any
anySourceArgs = foldMap (Any . isSourceArg)
  where
    -- borrow from sparse cgcc script's regex: do check if /^[^-].*\.c$/
    isSourceArg :: String -> Bool
    isSourceArg s = not ("-" `Data.List.isPrefixOf` s) && ".c" `Data.List.isSuffixOf` s


-- | Constructs a brief usage message.
usage :: FilePath -> String
usage progName = "Usage: " ++ progName ++ " [cc opts] FILENAME.c"
