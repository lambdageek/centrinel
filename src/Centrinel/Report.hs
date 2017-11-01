{-# language DeriveGeneric, OverloadedStrings #-}
module Centrinel.Report where

import qualified System.IO as IO
import Control.Exception (bracket_)
import Control.Monad.Except
import Data.Monoid ((<>))

import Centrinel.Types
import Centrinel.Report.Types
import qualified Centrinel.Report.Json as J

-- | Describes the methods of presenting the analysis results
data OutputMethod = OutputMethod
  { outputMethodDestination :: OutputDestination -- ^ Write results here
  , outputMethodFormat :: OutputFormat -- ^ Write the results in this format
  }

-- | How to present the output
data OutputFormat =
  PlainTextOutputFormat
  | JSONOutputFormat

data OutputDestination =
  StdOutOutputDestination
  | FilePathOutputDestination FilePath

-- | Default output method is plain text on stdOut
defaultOutputMethod :: OutputMethod
defaultOutputMethod = OutputMethod StdOutOutputDestination PlainTextOutputFormat

report :: OutputMethod -> ExceptT CentrinelError IO (a, [CentrinelAnalysisError]) -> IO (Maybe a)
report output comp = do
  x <- runExceptT comp
  withOutputMethod output $ \present -> presentReport present x

presentReport :: OutputFn -> Either CentrinelError (a, [CentrinelAnalysisError]) -> IO (Maybe a)
presentReport present x =
  case x of
    Left centErr -> do
      present (Abnormal centErr)
      return Nothing
    Right (a, warns) -> do
      present (Normal warns)
      return (Just a)

type OutputFn = Message -> IO ()

withOutputMethod :: OutputMethod -> (OutputFn -> IO a) -> IO a
withOutputMethod (OutputMethod dest fmt) =
  let (acquire, release, present) = case fmt of
        PlainTextOutputFormat -> (const (return ()), const (return ()), plainTextOutput)
        JSONOutputFormat -> (J.header, J.footer, J.output)
      runKont isFile h kont =
        bracket_ (acquire h) (release h) (kont $ present isFile h)
  in case dest of
    StdOutOutputDestination -> runKont False IO.stdout
    FilePathOutputDestination fp -> \kont -> IO.withFile fp IO.AppendMode $ \h ->
      runKont True h kont

plainTextOutput :: Bool -> IO.Handle -> Message -> IO ()
plainTextOutput isFile h = \msg ->
  case msg of
    Normal warns ->
      unless (null warns) $ do
      mapM_ (IO.hPutStrLn h . show) warns
      when isFile (summarize "notices" $ length warns)
    Abnormal centErr -> 
      case centErr of
        CentCPPError exitCode ->
          IO.hPutStrLn h $ "Preprocessor failed with " <> show exitCode
        CentParseError err -> IO.hPutStrLn h (show err)
        CentAbortedAnalysisError errs -> do
          mapM_ (IO.hPutStrLn h . show) errs
          when isFile (summarize "notices" $ length errs)
          IO.hPutStrLn h $ "Analysis of translation unit was aborted after the preceeding errors"
  where
    summarize sortOfThings n =
      when (n >= 20) $ IO.hPutStrLn h $ "There were " <> show n <> " " <> sortOfThings

