-- | Parse command line arguments like the given preprocessor
{-# language DeriveFunctor #-}
module Centrinel.System.RunLikeCC (
  -- * Data types of compiler invocations
  RunLikeCC (..)
  , ParsedCC (..)
  -- * Command line parsing
  , runLikeCC
  -- * Utilities
  , anySourceArgs
  -- * Re-exported definitons
  , Preprocessor
  , GCC
  , newGCC
  , CppArgs) where

import Data.Text (Text)
import qualified Data.List
import Data.Monoid (Any(..))

import Language.C.System.Preprocess (Preprocessor(..), CppArgs)
import Language.C.System.GCC (GCC, newGCC)


-- | The compiler command that the build process ran on the given file
-- and the working directory at the time.
data RunLikeCC a = RunLikeCC { file :: Text, workingDirectory :: Text, artifact :: a }
  deriving (Show, Functor)

-- | The result of parsing a set of cc-like command line arguments
data ParsedCC =
  -- | There were no input files in the command line
  NoInputFilesCC
  -- | There was an error parsing some arguments
  | ErrorParsingCC String
  -- | Parsed the command line, possibly with some ignored args.
  | ParsedCC CppArgs [String]

-- | @runLikeCC gcc args@ parses the command line arguments like the
-- given preprocessor.
-- @progName@ is used in error messages.
runLikeCC :: Preprocessor cpp => cpp -> [String] -> ParsedCC
{-# specialize runLikeCC :: GCC -> [String] -> ParsedCC #-}
runLikeCC cpp args =
  -- if there's no source file among the arguments,
  -- (if we're called from the linking step), do nothing.
  if not $ getAny $ anySourceArgs args
  then NoInputFilesCC
  else case parseCPPArgs cpp args of
    Left err -> ErrorParsingCC err
    Right (cppArgs, ignoredArgs) -> ParsedCC cppArgs ignoredArgs

-- | Returns @Any True@ iff any of the given strings looks like a source file.
-- It uses a pretty rough heuristic: the string must not begin with a @'-'@ and
-- it must end with @".c"@.  This seems good enough.
anySourceArgs :: [String] -> Any
anySourceArgs = foldMap (Any . isSourceArg)
  where
    -- borrow from sparse cgcc script's regex: do check if /^[^-].*\.c$/
    isSourceArg :: String -> Bool
    isSourceArg s = not ("-" `Data.List.isPrefixOf` s) && ".c" `Data.List.isSuffixOf` s
