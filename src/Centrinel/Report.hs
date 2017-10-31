{-# language DeriveGeneric, OverloadedStrings #-}
module Centrinel.Report where

import qualified System.IO as IO
import Control.Exception (bracket_)
import Control.Monad.Except

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

report :: Show w => OutputMethod -> ExceptT CentrinelError IO (a, [w]) -> IO (Maybe a)
report output comp = do
  x <- runExceptT comp
  withOutputMethod output $ \present -> presentReport present x

presentReport :: Show w => OutputFn -> Either CentrinelError (a, [w]) -> IO (Maybe a)
presentReport present x =
  case x of
    Left centErr -> do
      case centErr of
        CentCPPError exitCode -> present $ Abnormal $ "Preprocessor failed with " ++ show exitCode
        CentParseError err -> present $ Abnormal $ show err
        CentAnalysisError errs -> do
          present $ Normal "Errors:"
          mapM_ (present . Normal . show) errs
          let n = length errs
          present $ Verbose (n <= 20) ("There were " ++ show n ++ " errors")
      return Nothing
    Right (a, warns) -> do
      unless (null warns) $ do
        present $ Normal "Warnings:"
        mapM_ (present . Normal . show) warns
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
    Normal txt -> IO.hPutStrLn h txt
    Abnormal txt -> IO.hPutStrLn h txt
    Verbose suppress txt -> when (not suppress || isFile) $ IO.hPutStrLn h txt

