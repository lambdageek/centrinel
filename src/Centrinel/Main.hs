{-# language NamedFieldPuns #-}
module Centrinel.Main where

import Control.Monad (when, liftM)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Functor.Identity (Identity(..))
import Data.Monoid (Monoid(..), First(..))
import Data.Foldable (forM_)
import qualified Data.List as L
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as B

import qualified System.Directory as Dir
import System.Exit (exitSuccess, exitFailure)
import System.Environment (lookupEnv)
import qualified System.FilePath as FilePath

import Language.C.Syntax.AST (CTranslUnit)
import Language.C.System.GCC (newGCC, GCC)
import qualified Language.C.Analysis.SemRep as A

import Centrinel.AnalysisPlan (Plan, defaultPlan, runPlan)
import qualified Centrinel.NakedPointer as NP
import Centrinel.Report (OutputMethod, withOutputMethod, withWorkingDirectory,
                         MonadOutputMethod(..))
import Centrinel.Types (CentrinelFatalError, CentrinelAnalysisError)
import Centrinel.Report.Types (Message(..))
import Centrinel.System.RunLikeCC (RunLikeCC(..)
                                  , CppArgs, cppArgsInputFile
                                  , runLikeCC, ParsedCC(..)
                                  , cppArgsForCentrinel)
import Centrinel.System.ParseCFile (parseCFile)
import qualified Centrinel.Util.Datafiles as HGData
import qualified Centrinel.Util.CompilationDatabase as CDB
import Centrinel.RegionInferenceResult (RegionInferenceResult)

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
    RunOneCentrinelCmd options args -> runOneCentrinelCmd plan options args
    RunProjectCentrinelCmd options fp -> runProjectCentrinelCmd plan options fp
  where
    plan = defaultPlan

runOneCentrinelCmd :: Plan -> CentrinelOptions -> [String] -> IO ()
runOneCentrinelCmd plan options args = do
      env <- prepareEnvironment options
      let compilation = CDB.makeStandaloneRunLikeCC args
      result <- withOutputMethod (outputCentrinelOpt options) $ analyzeTranslationUnit plan env compilation 
      if result then exitSuccess else exitFailure

runProjectCentrinelCmd :: Plan -> CentrinelOptions -> FilePath -> IO ()
runProjectCentrinelCmd plan options fp = do
      env <- prepareEnvironment options
      verboseDebugLn $ "Project is: '" ++ fp  ++ "'"
      cdb_ <- do
        res <- CDB.parseCompilationDatabase <$> B.readFile fp
        case res of
          Left err -> do
            putStrLn $ "error parsing compilation database " ++ fp ++ ": " ++ err
            exitFailure
          Right ok -> return ok
      let
        -- Reorganize the cdb by workingDirectory/file mapped to a list of invocations
        -- to make sure we only look at each translation unit once even if the compilation
        -- database had multiple invocations on that file.
        cdb = CDB.combineDuplicateRuns cdb_
      withOutputMethod (outputCentrinelOpt options) $ do
        forM_ cdb $ \compilations -> do
          case CDB.divideRunLikeCC compilations of
            [] ->
              return () -- can't really happen, but it's benign
            cs@(compilation:_) -> do
              let tu = T.unpack (CDB.workingDirectory compilation) FilePath.</> T.unpack (CDB.file compilation)
              when (length cs > 1) $
                verboseDebugLn $ tu ++ ": picking first of " ++ show (length cs) ++ " compiler invocations for analysis"
              _ <- analyzeTranslationUnit plan env compilation
              return ()

analyzeTranslationUnit :: (MonadIO m, MonadOutputMethod m)
                       => Plan
                       -> (GCC, [ExcludedDir], HGData.Datafiles)
                       -> RunLikeCC CDB.Invoke
                       -> m Bool
analyzeTranslationUnit plan (gcc, excludeDirs, datafiles) compilation = do
  let RunLikeCC {file = file_, workingDirectory, artifact} = compilation
      file = T.unpack file_
  withWorkingDirectory (T.unpack workingDirectory) $ do
    verboseDebugLn $ "Analyzing " ++ show file
    case runLikeCC gcc (T.unpack <$> CDB.invokeArguments artifact) of
      NoInputFilesCC -> return True
      ErrorParsingCC err -> do
        verboseDebugLn $ "error parsing cc arguments: " ++ err
        return False
      ParsedCC cppArgs ignoredArgs -> do
        when (not $ null ignoredArgs) $ do
          verboseDebugLn "Ignored args:"
          mapM_ (verboseDebugLn . ("\t"++)) ignoredArgs
        ignoreArtifact <- liftIO $ excludeAnalysis excludeDirs cppArgs
        if ignoreArtifact
          then
          do
            verboseDebugLn "in excluded directory, skipping"
            return True
          else
          do
            verboseDebugLn (showCppArgs cppArgs)
            mast <- liftIO $ runExceptT $ parseCFile gcc (cppArgsForCentrinel cppArgs datafiles)
            case mast of
              Left fatalErr -> do
                present file (Abnormal fatalErr)
                return False
              Right ast ->
                let opts = makeNakedPointerOpts (cppArgsInputFile cppArgs)
                in case think' plan opts ast of
                    Left err -> do
                      present file (Abnormal err)
                      return False
                    Right (_, warns) -> do
                      present file (Normal warns)
                      return True

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


-- | Same as 'think' but returns @Left err@ for a fatal error or @Right (res,
-- warns)@ for a result and non-fatal warnings.
think' :: Plan -> NP.AnalysisOpts -> CTranslUnit
       -> Either CentrinelFatalError ((A.GlobalDecls, RegionInferenceResult), [CentrinelAnalysisError])
think' plan npOpts = runIdentity . runExceptT . runPlan plan npOpts

verboseDebugLn :: MonadIO m => String -> m ()
verboseDebugLn = liftIO . putStrLn

makeNakedPointerOpts :: FilePath -> NP.AnalysisOpts
makeNakedPointerOpts fp = NP.AnalysisOpts {NP.analysisOptFilterPath = Just fp }

