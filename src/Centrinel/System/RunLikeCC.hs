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
  , cppArgsInputFile
  -- * Re-exported definitons
  , Preprocessor
  , GCC
  , newGCC
  , CppArgs
    -- * Argument filtering for Centrinel
  , cppArgsForCentrinel
  ) where

import Data.Text (Text)
import qualified Data.List
import Data.Monoid (Any(..))

import Language.C.System.Preprocess (Preprocessor(..), CppArgs)
import qualified Language.C.System.Preprocess as Cpp
import Language.C.System.GCC (GCC, newGCC)

import Centrinel.Util.Datafiles as Datafiles

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
runLikeCC cpp args0 =
  let (preIgnored, args) = prefilterCCArgs args0
  in if not $ getAny $ anySourceArgs args
  -- if there's no source file among the arguments,
  -- (if we're called from the linking step), do nothing.
  then NoInputFilesCC
  else case parseCPPArgs cpp args of
    Left err -> ErrorParsingCC err
    Right (cppArgs, ignoredArgs) -> ParsedCC cppArgs (preIgnored ++ ignoredArgs)

-- | Filter out known problematic CFLAGS that haven't made it into language-c yet.
prefilterCCArgs :: [String] -> ([String], [String])
prefilterCCArgs = Data.List.partition isProblem
  where
    isProblem flag =
      getAny $ foldMap (\(f,_excuse) -> Any (f flag)) predicates
    predicates :: [(String -> Bool, String)]
    predicates =
      [(Data.List.isPrefixOf "-g", "-ggdb3 leaves #defines in preprocessor output")
      ]

cppArgsInputFile :: CppArgs -> FilePath
cppArgsInputFile = Cpp.inputFile

-- | Returns @Any True@ iff any of the given strings looks like a source file.
-- It uses a pretty rough heuristic: the string must not begin with a @'-'@ and
-- it must end with @".c"@.  This seems good enough.
anySourceArgs :: [String] -> Any
anySourceArgs = foldMap (Any . isSourceArg)
  where
    -- borrow from sparse cgcc script's regex: do check if /^[^-].*\.c$/
    isSourceArg :: String -> Bool
    isSourceArg s = not ("-" `Data.List.isPrefixOf` s) && ".c" `Data.List.isSuffixOf` s

-- | Remove any 'Cpp.outputFile' options, and add
-- preprocessor defines and definitions for Centrinel to successfully parse and
-- analize the specified input file.
cppArgsForCentrinel :: Cpp.CppArgs -> Datafiles.Datafiles -> Cpp.CppArgs
cppArgsForCentrinel cppArgs datafiles =
  let centrinelHeader = Datafiles.datafileCentrinelHeader datafiles
  in cppArgs
     { Cpp.cppOptions = Cpp.cppOptions cppArgs ++ [ Cpp.IncludeFile centrinelHeader ]
     , Cpp.outputFile = Nothing
     }

