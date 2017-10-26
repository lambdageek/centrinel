{-# language NamedFieldPuns #-}
module Centrinel.Main where

import Control.Monad (when, liftM)

import Data.Monoid (Monoid(..), First(..))
import Data.Foldable (forM_)
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as B

import qualified System.Directory as Dir
import System.Exit (exitWith, exitSuccess, exitFailure)
import System.Environment (lookupEnv)

import Centrinel (runCentrinel, report)
import Centrinel.System.RunLikeCC (runLikeCC, RunLikeCC(..), ParsedCC(..))
import qualified Centrinel.Util.Datafiles as HGData
import qualified Centrinel.Util.CompilationDatabase as CDB

import Language.C.System.GCC (newGCC)

import Centrinel.Debug.PrettyCppArgs (showCppArgs)

type CppArgStrings = [String]

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
  -- ^ If @Just fp@ use @fp@ as the preprocessor for parsing C files.
  compilerCentrinelOpt :: Maybe FilePath
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
    RunOneCentrinelCmd options args -> do
      gcc <- liftM newGCC (getCC options)
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
    RunProjectCentrinelCmd options fp -> do
      gcc <- liftM newGCC (getCC options)
      datafiles <- HGData.getDatafiles
      putStrLn $ "Project is: '" ++ fp  ++ "'"
      cdb <- do
        res <- CDB.parseCompilationDatabase <$> B.readFile fp
        case res of
          Left err -> do
            putStrLn $ "error parsing compilation database " ++ fp ++ ": " ++ err
            exitFailure
          Right ok -> return ok
      forM_ cdb $ \(RunLikeCC {file, workingDirectory, artifact}) -> Dir.withCurrentDirectory (T.unpack workingDirectory) $ do
        putStrLn $ "Analyzing " ++ show file
        case runLikeCC gcc (T.unpack <$> CDB.invokeArguments artifact) of
          NoInputFilesCC -> return ()
          ErrorParsingCC err -> do
            putStrLn $ "error parsing cc arguments: " ++ err
            return ()
          ParsedCC cppArgs ignoredArgs -> do
            when (not $ null ignoredArgs) $ do
              putStrLn "Ignored args:"
              mapM_ (putStrLn . ("\t"++)) ignoredArgs
            putStrLn (showCppArgs cppArgs)
            _ <- report (runCentrinel datafiles gcc cppArgs)
            return ()
      return ()

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
