-- | JSON Output format for the analysis report
{-# language DeriveGeneric, OverloadedStrings, LambdaCase #-}
module Centrinel.Report.Json (header, footer, output) where

import GHC.Generics (Generic)

import Control.Monad (unless)
import qualified Data.Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Encoding as AE
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Monoid (Monoid(..), (<>))
import System.Exit (ExitCode)
import qualified System.IO as IO

import Language.C.Parser (ParseError)
import Language.C.Data.Error (CError)
import qualified Language.C.Data.Error as CError
import Centrinel.RegionMismatchError (RegionMismatchError(..))
import Centrinel.NakedPointerError (NakedPointerError)

import qualified Centrinel.Report.Types as R

-- | Keep in sync with <https://github.com/lambdageek/centrinel-report>
jsonBlobVersion :: BS.ByteString
jsonBlobVersion = "2"

data CentrinelAnalysisMessage =
  RegionMismatchMessage RegionMismatchError
  | NakedPointerMessage NakedPointerError
  | CErrorMessage CError

-- | A fatal error due to tool failure
data ToolFail =
  -- | The preprocessor exited with an error code
  CPPToolFail ExitCode
  -- | The C parser had an error.  (For Centrinel this is a tool failure because
  -- we expected the analysis to run after a normal C compiler run, meaning that
  -- it should have been a parser error for the real C compiler, too.)
  | ParseToolFail ParseError

data Message =
  NormalMessages
  { isAbnormal :: Bool -- ^ translation was aborted due to one of the messages
  , messages :: [CentrinelAnalysisMessage] -- ^ what the analyses found
  }
  | ToolFailMessage
    {
      toolFailure :: ToolFail
    }
  deriving (Generic)

data TranslationUnitMessage =
  TranslationUnitMessage
  { workingDirectory :: FilePath -- ^ working directory 
  , translationUnit :: FilePath -- ^ main translation unit, relative to 'workingDirectory'
  , message :: Message  -- ^ what happened
  }
  deriving Generic

instance Data.Aeson.ToJSON Message where
  toEncoding = Data.Aeson.genericToEncoding Data.Aeson.defaultOptions

instance Data.Aeson.ToJSON TranslationUnitMessage where
  toEncoding = Data.Aeson.genericToEncoding Data.Aeson.defaultOptions

instance Data.Aeson.ToJSON CentrinelAnalysisMessage where
  toJSON e = Data.Aeson.object (encodeCentrinelAnalysisMessage e)
  toEncoding e = AE.pairs (mconcat $ encodeCentrinelAnalysisMessage e)

tag :: Data.Aeson.KeyValue kv => String -> [kv]
tag s = [ "tag" .= s ]

encodeCentrinelAnalysisMessage :: Data.Aeson.KeyValue kv => CentrinelAnalysisMessage -> [kv]
encodeCentrinelAnalysisMessage e = encodeErrorInfo (errorInfo e) <> encodeSpecificMessage e
    where
      errorInfo (RegionMismatchMessage m) = CError.errorInfo m
      errorInfo (NakedPointerMessage m) = CError.errorInfo m
      errorInfo (CErrorMessage m) = CError.errorInfo m

encodeErrorInfo :: Data.Aeson.KeyValue kv => CError.ErrorInfo -> [kv]
encodeErrorInfo (CError.ErrorInfo errorLevel position lines) =
  [ "errorLevel" .= (show errorLevel)
  , "position" .= (show position)
  , "lines" .= lines
  ]

ea = Data.Aeson.Array mempty

encodeSpecificMessage :: Data.Aeson.KeyValue kv => CentrinelAnalysisMessage -> [kv]
encodeSpecificMessage (RegionMismatchMessage rme) = tag "regionMismatchMessage" <> ["regionMismatchMessage" .= ea] -- [vic1, vic2]
encodeSpecificMessage (NakedPointerMessage npe) = tag "nakedPointerMessage" <> ["nakedPointerMessage" .= ea]
encodeSpecificMessage (CErrorMessage npe) = []

instance Data.Aeson.ToJSON ToolFail where
  toJSON tf = Data.Aeson.object (encodeToolFail tf)
  toEncoding tf = AE.pairs (mconcat $ encodeToolFail tf)

encodeToolFail :: Data.Aeson.KeyValue kv => ToolFail -> [kv]
encodeToolFail (CPPToolFail ec) = tag "cppToolFail" <> [ "cppToolFail" .= show ec ]
encodeToolFail (ParseToolFail pe) = tag "parseToolFail" <> [ "parseToolFail" .= show pe ]

output :: Bool -> IO.Handle -> FilePath -> FilePath -> R.Message -> IO ()
output _isFile h = \workDir fp rmsg ->
  case rmsg of
    R.Normal warns ->
      unless (null warns) $ put $ TranslationUnitMessage workDir fp $ NormalMessages False $ map massageError warns
    R.Abnormal centErr ->
      put $ TranslationUnitMessage workDir fp $ case centErr of
        R.CentCPPError exitCode -> ToolFailMessage $ CPPToolFail exitCode
        R.CentParseError err -> ToolFailMessage $ ParseToolFail err
        R.CentAbortedAnalysisError errs -> NormalMessages True $ map massageError errs
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

massageError :: R.CentrinelAnalysisError -> CentrinelAnalysisMessage
massageError =
  \case
    R.CACError cerror -> CErrorMessage cerror
    R.CARegionMismatchError rme -> RegionMismatchMessage rme
    R.CANakedPointerError npe -> NakedPointerMessage npe
