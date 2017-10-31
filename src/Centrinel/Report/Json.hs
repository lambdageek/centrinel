-- | JSON Output format for the analysis report
{-# language DeriveGeneric, OverloadedStrings #-}
module Centrinel.Report.Json (header, footer, output) where

import qualified System.IO as IO
import qualified Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Monoid ((<>))

import Centrinel.Report.Types

-- | Keep in sync with <https://github.com/lambdageek/centrinel-report>
jsonBlobVersion :: BS.ByteString
jsonBlobVersion = "0"


output :: Bool -> IO.Handle -> Message -> IO ()
output _isFile h = \msg -> do
    BS.hPut h (Data.Aeson.encode msg)
    BS8.hPutStrLn h ","

header :: IO.Handle -> IO ()
header h = do
  BS8.hPutStr h $ "{\"centrinel_report_version\": \"" <> jsonBlobVersion <> "\","
  BS8.hPutStr h " \"messages\": [\n"

footer :: IO.Handle -> IO ()
footer h =
  BS8.hPutStrLn h "{}\n]}" -- need a trailing empty object because of the trailing comma, in output
