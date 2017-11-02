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
jsonBlobVersion = "1"

newtype Message = Message String
  deriving (Generic)

data TranslationUnitMessage =
  TranslationUnitMessage { translationUnit :: FilePath
                         , isAbnormal :: Bool
                         , messages :: [Message]
                         }
  deriving Generic

instance Data.Aeson.ToJSON Message where
  toEncoding = Data.Aeson.genericToEncoding Data.Aeson.defaultOptions

instance Data.Aeson.ToJSON TranslationUnitMessage where
  toEncoding = Data.Aeson.genericToEncoding Data.Aeson.defaultOptions

output :: Bool -> IO.Handle -> FilePath -> R.Message -> IO ()
output _isFile h = \fp rmsg ->
  case rmsg of
    R.Normal warns ->
      unless (null warns) $ put $ TranslationUnitMessage fp False $ map (Message . show) warns
    R.Abnormal centErr ->
      put $ TranslationUnitMessage fp True $ case centErr of
        R.CentCPPError exitCode -> [ Message $ "Preprocessor failed with " ++ show exitCode ]
        R.CentParseError err -> [ Message $ show err ]
        R.CentAbortedAnalysisError errs -> map (Message . show) errs
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
