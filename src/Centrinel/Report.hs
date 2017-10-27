module Centrinel.Report where

import Control.Monad.Except

import qualified System.Exit

import Centrinel.Types

-- | Describes the methods of presenting the analysis results
data OutputMethod = OutputMethod
  { outputMethodDestination :: OutputDestination -- ^ Write results here
  , outputMethodFormat :: OutputFormat -- ^ Write the results in this format
  }

-- | How to present the output
data OutputFormat =
  PlainTextOutputFormat
  {- - JSONOutputFormat -}

data OutputDestination =
  StdOutOutputDestination
  | FilePathOutputDestination FilePath

-- | Default output method is plain text on stdOut
defaultOutputMethod :: OutputMethod
defaultOutputMethod = OutputMethod StdOutOutputDestination PlainTextOutputFormat

report :: Show w => OutputMethod -> ExceptT CentrinelError IO (a, [w]) -> IO System.Exit.ExitCode
report output comp = do
  x <- runExceptT comp
  case x of
    Left centErr -> do
      case centErr of
        CentCPPError exitCode -> print $ "Preprocessor failed with " ++ show exitCode
        CentParseError err -> print err
        CentAnalysisError errs -> do
          putStrLn "Errors:"
          mapM_ print errs
          let n = length errs
          when (n > 20) $ putStrLn ("There were " ++ show n ++ " errors")
      return (System.Exit.ExitFailure 1)
    Right (_, warns) -> do
      unless (null warns) $ do
        putStrLn "Warnings:"
        mapM_ print warns
      return System.Exit.ExitSuccess
