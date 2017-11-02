{-# language NamedFieldPuns #-}
module Centrinel.Main where

import Control.Monad (when, liftM)
import Control.Monad.Except (runExceptT)

import Data.Either (isRight)
import Data.Monoid (Monoid(..), First(..))
import Data.Foldable (forM_)
import qualified Data.List as L
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as B

import qualified System.Directory as Dir
import System.Exit (exitSuccess, exitFailure)
import System.Environment (lookupEnv)
import qualified System.FilePath as FilePath

import Centrinel (runCentrinel)
import Centrinel.Report (OutputMethod, OutputFn, withOutputMethod, presentReport)
import Centrinel.System.RunLikeCC (RunLikeCC(..)
                                  , CppArgs, cppArgsInputFile
                                  , runLikeCC, ParsedCC(..))
import qualified Centrinel.Util.Datafiles as HGData
import qualified Centrinel.Util.CompilationDatabase as CDB

import Language.C.System.GCC (newGCC, GCC)

import Centrinel.Debug.PrettyCppArgs (showCppArgs)

type CppArgStrings = [String]
type ExcludedDir = FilePath

-- | Centrinel command to run
data CentrinelCmd =
  -- | Run Centrinel on a single C source file.
  RunOneCentrinelCmd CentrinelOptions CppArgStrings
  -- | Run Centrinel on each file in a Clang Compilation Database JSON file
  -- using the preprocessor options found in the compilation database.
  -- (If a file appears multiple times, it will be scanned multiple times)
  | RunProjectCentrinelCmd CentrinelOptions FilePath

-- | Additional options
data CentrinelOptions = CentrinelOptions {
  -- | If @Just fp@ use @fp@ as the preprocessor for parsing C files.
  compilerCentrinelOpt :: Maybe FilePath
  -- | How to present the analysis results
  , outputCentrinelOpt :: OutputMethod
  , excludeDirsCentrinelOpt :: [FilePath]
  }

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
    RunOneCentrinelCmd options args -> runOneCentrinelCmd options args
    RunProjectCentrinelCmd options fp -> runProjectCentrinelCmd options fp


runOneCentrinelCmd :: CentrinelOptions -> [String] -> IO ()
runOneCentrinelCmd options args = do
      env <- prepareEnvironment options
      let compilation = CDB.makeStandaloneRunLikeCC args
      result <- withOutputMethod (outputCentrinelOpt options) $ \present ->
        analyzeTranslationUnit env present compilation 
      if result then exitSuccess else exitFailure

runProjectCentrinelCmd :: CentrinelOptions -> FilePath -> IO ()
runProjectCentrinelCmd options fp = do
      env <- prepareEnvironment options
      putStrLn $ "Project is: '" ++ fp  ++ "'"
      cdb <- do
        res <- CDB.parseCompilationDatabase <$> B.readFile fp
        case res of
          Left err -> do
            putStrLn $ "error parsing compilation database " ++ fp ++ ": " ++ err
            exitFailure
          Right ok -> return ok
      withOutputMethod (outputCentrinelOpt options) $ \present -> 
        forM_ cdb $ \compilation -> do
        _ <- analyzeTranslationUnit env present compilation
        return ()

analyzeTranslationUnit :: (GCC, [ExcludedDir], HGData.Datafiles)
                       -> OutputFn
                       -> RunLikeCC CDB.Invoke
                       -> IO Bool
analyzeTranslationUnit (gcc, excludeDirs, datafiles) present compilation = do
  let RunLikeCC {file, workingDirectory, artifact} = compilation
  Dir.withCurrentDirectory (T.unpack workingDirectory) $ do
    putStrLn $ "Analyzing " ++ show file
    case runLikeCC gcc (T.unpack <$> CDB.invokeArguments artifact) of
      NoInputFilesCC -> return True
      ErrorParsingCC err -> do
        putStrLn $ "error parsing cc arguments: " ++ err
        return False
      ParsedCC cppArgs ignoredArgs -> do
        when (not $ null ignoredArgs) $ do
          putStrLn "Ignored args:"
          mapM_ (putStrLn . ("\t"++)) ignoredArgs
        ignoreArtifact <- excludeAnalysis excludeDirs cppArgs
        if ignoreArtifact
          then
          do
            putStrLn "in excluded directory, skipping"
            return True
          else
          do
            putStrLn (showCppArgs cppArgs)
            result <- runExceptT (runCentrinel datafiles gcc cppArgs)
            _ <- presentReport present result
            return (isRight result)


prepareEnvironment :: CentrinelOptions -> IO (GCC, [ExcludedDir], HGData.Datafiles)
prepareEnvironment options = do
  gcc <- liftM newGCC (getCC options)
  excludeDirs <- traverse Dir.canonicalizePath (excludeDirsCentrinelOpt options)
  datafiles <- HGData.getDatafiles
  return (gcc, excludeDirs, datafiles)


-- | Look for "REAL_CC" environment variable and return that path, if unset, return "cc"
getCC :: CentrinelOptions -> IO FilePath
getCC options =
  (maybe "cc" id . getFirst . mconcat) <$> alts
  where
    alts :: IO [First FilePath]
    alts = sequence [checkOpt, checkEnv]
    checkOpt :: IO (First FilePath)
    checkOpt = return $ First $ compilerCentrinelOpt options
    checkEnv :: IO (First FilePath)
    checkEnv = First <$> lookupEnv "REAL_CC"

-- | @excludeAnalysis dirs cppArgs@ returns 'True' iff the 'inputFile' from the
-- given CPP args is in one of the excluded directories (or a
-- subdirectory). The directories @dirs@ are assumed to be absolute the
-- inputfile in @cppArgs@ may be absolute or relative to the current working
-- directory.
excludeAnalysis :: [FilePath] -> CppArgs -> IO Bool
excludeAnalysis excludeDirs cppArgs = do
  absoluteInput <- Dir.canonicalizePath (cppArgsInputFile cppArgs)
  let inputDirComponents = FilePath.splitDirectories absoluteInput
      excludeDirComponents = map FilePath.splitDirectories excludeDirs
      inputExcludedByDir d = d `L.isPrefixOf` inputDirComponents
  return (any inputExcludedByDir excludeDirComponents)
