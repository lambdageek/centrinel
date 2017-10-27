{-# language DeriveGeneric, OverloadedStrings #-}
module Centrinel.Report where

import GHC.Generics (Generic)
import qualified System.IO as IO
import Control.Exception (bracket_)

import Data.Aeson (ToJSON)
import qualified Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

import Control.Monad.Except

import Centrinel.Types

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
        JSONOutputFormat -> (jsonAcquire, jsonRelease, jsonOutput)
      runKont isFile h kont =
        bracket_ (acquire h) (release h) (kont $ present isFile h)
  in case dest of
    StdOutOutputDestination -> runKont False IO.stdout
    FilePathOutputDestination fp -> \kont -> IO.withFile fp IO.AppendMode $ \h ->
      runKont True h kont


data Message = Normal !String
  | Abnormal !String
  | Verbose !Bool !String
  deriving (Generic)

instance ToJSON Message where
  toEncoding = Data.Aeson.genericToEncoding Data.Aeson.defaultOptions

plainTextOutput :: Bool -> IO.Handle -> Message -> IO ()
plainTextOutput isFile h = \msg ->
  case msg of
    Normal txt -> IO.hPutStrLn h txt
    Abnormal txt -> IO.hPutStrLn h txt
    Verbose suppress txt -> when (not suppress || isFile) $ IO.hPutStrLn h txt


jsonOutput :: Bool -> IO.Handle -> Message -> IO ()
jsonOutput _isFile h = \msg -> do
    BS.hPut h (Data.Aeson.encode msg)
    BS8.hPutStrLn h ","

jsonAcquire :: IO.Handle -> IO ()
jsonAcquire h = do
  BS8.hPutStr h "{\"centrinel_report_version\": \"0\","
  BS8.hPutStr h " \"messages\": [\n"

jsonRelease :: IO.Handle -> IO ()
jsonRelease h =
  BS8.hPutStrLn h "{}\n]}" -- need a trailing empty object because of the trailing comma, in jsonOutput
