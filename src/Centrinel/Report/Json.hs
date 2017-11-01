-- | JSON Output format for the analysis report
{-# language DeriveGeneric, OverloadedStrings #-}
module Centrinel.Report.Json (header, footer, output) where

import GHC.Generics (Generic)

import Control.Monad (unless)
import qualified Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Monoid ((<>))
import qualified System.IO as IO

import qualified Centrinel.Report.Types as R

-- | Keep in sync with <https://github.com/lambdageek/centrinel-report>
jsonBlobVersion :: BS.ByteString
jsonBlobVersion = "0"

data Message =
  Normal !String
  | Abnormal !String
  | Verbose !Bool !String
  deriving (Generic)

instance Data.Aeson.ToJSON Message where
  toEncoding = Data.Aeson.genericToEncoding Data.Aeson.defaultOptions

output :: Bool -> IO.Handle -> R.Message -> IO ()
output _isFile h = \rmsg ->
  case rmsg of
    R.Normal warns ->
      unless (null warns) $ do
      let n = length warns
      mapM_ (put . Normal . show) warns
      put $ Verbose (n <= 20) $ "There were " <> show n <> " notices"
    R.Abnormal centErr ->
      case centErr of
        R.CentCPPError exitCode -> put $ Abnormal $ "Preprocessor failed with " ++ show exitCode
        R.CentParseError err -> put $ Abnormal $ show err
        R.CentAbortedAnalysisError errs -> do
          mapM_ (put . Normal . show) errs
          let n = length errs
          put $ Verbose (n <= 20) $ "There were " <> show n <> " notices"
          put $ Abnormal $ "Analysis of translation unit was aborted after the preceeding errors"
  where
    put msg = do
      BS.hPut h (Data.Aeson.encode msg)
      BS8.hPutStrLn h ","

header :: IO.Handle -> IO ()
header h = do
  BS8.hPutStr h $ "{\"centrinel_report_version\": \"" <> jsonBlobVersion <> "\","
  BS8.hPutStr h " \"messages\": [\n"

footer :: IO.Handle -> IO ()
footer h =
  BS8.hPutStrLn h "{}\n]}" -- need a trailing empty object because of the trailing comma, in output
