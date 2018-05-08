-- | JSON Output format for the analysis report
{-# language DeriveGeneric, OverloadedStrings, LambdaCase #-}
{-# options_ghc -fno-warn-orphans #-}
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

import qualified Language.C.Analysis.SemRep as C
import Language.C.Parser (ParseError)
import Language.C.Data.Error (CError)
import qualified Language.C.Data.Error as CError
import Centrinel.RegionMismatchError (RegionMismatchError)
import Centrinel.NakedPointerError (NakedPointerError(..), NPEVictim (..))
import qualified Centrinel.PrettyPrint as PP

import qualified Centrinel.Report.Types as R

import Centrinel.Data.CodePosition

-- | Keep in sync with <https://github.com/lambdageek/centrinel-report>
jsonBlobVersion :: BS.ByteString
jsonBlobVersion = "3"

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
encodeCentrinelAnalysisMessage e = uncurry encodeErrorInfo (errorInfo e) <> encodeSpecificMessage e
    where
      errorInfo (RegionMismatchMessage m) =  (CError.errorInfo m, True)
      errorInfo (NakedPointerMessage m) = (CError.errorInfo m, False)
      errorInfo (CErrorMessage m) = (CError.errorInfo m, True)

encodeErrorInfo :: Data.Aeson.KeyValue kv => CError.ErrorInfo -> Bool -> [kv]
encodeErrorInfo (CError.ErrorInfo errorLevel position msgLines) includeLines =
  [ "errorLevel" .= (show errorLevel)
  , "position" .= (show position)
  ]
  <> if includeLines then ["lines" .= msgLines ] else mempty

ea :: Data.Aeson.Value
ea = Data.Aeson.Array mempty

encodeSpecificMessage :: Data.Aeson.KeyValue kv => CentrinelAnalysisMessage -> [kv]
encodeSpecificMessage (RegionMismatchMessage _rme) = tag "regionMismatchMessage" <> ["regionMismatchMessage" .= ea] -- [vic1, vic2]
encodeSpecificMessage (NakedPointerMessage npe) = tag "nakedPointerMessage" <> ["nakedPointerMessage" .= npe]
encodeSpecificMessage (CErrorMessage _npe) = []

instance Data.Aeson.ToJSON NakedPointerError where
  toJSON = Data.Aeson.object . encodeNakedPointerError
  toEncoding = AE.pairs . mconcat . encodeNakedPointerError

instance Data.Aeson.ToJSON ToolFail where
  toJSON = Data.Aeson.object . encodeToolFail
  toEncoding = AE.pairs . mconcat . encodeToolFail

encodeToolFail :: Data.Aeson.KeyValue kv => ToolFail -> [kv]
encodeToolFail (CPPToolFail ec) = tag "cppToolFail" <> [ "cppToolFail" .= show ec ]
encodeToolFail (ParseToolFail pe) = tag "parseToolFail" <> [ "parseToolFail" .= show pe ]

encodeNakedPointerError :: Data.Aeson.KeyValue kv => NakedPointerError -> [kv]
encodeNakedPointerError (NakedPointerError {inDefinition = inD, victims = vs}) =
  [ "inDefintion" .= inD, "victims" .= vs]

instance Data.Aeson.ToJSON NPEVictim where
  toJSON = Data.Aeson.object . encodeNPEVictim
  toEncoding = AE.pairs . mconcat . encodeNPEVictim

encodeNPEVictim :: Data.Aeson.KeyValue kv => NPEVictim -> [kv]
encodeNPEVictim (NPEVictim ty codePosn) =
  [ "type" .= ty, "position" .= codePosn ]

instance Data.Aeson.ToJSON C.Type where
  toJSON = Data.Aeson.toJSON . PP.render . PP.pretty

instance Data.Aeson.ToJSON NPEPosn where
  toJSON = Data.Aeson.object . encodeCodePosition
  toEncoding = AE.pairs . mconcat . encodeCodePosition

encodeCodePosition :: Data.Aeson.KeyValue kv => NPEPosn -> [kv]
encodeCodePosition p =
  case p of
    NPEArg j v ni p' -> tag "arg" <> ["index" .= j, "var" .= pretty v, "position" .= pos ni, "next" .= p']
    NPERet p' -> tag "ret" <> ["next" .= p']
    NPEDecl fname -> tag "decl" <> ["fname" .= pretty fname]
    NPETypeDefRef ni p' -> tag "typedefRef" <> ["position" .= pos ni, "next" .= p']
    NPETypeDefDef ni p' -> tag "typedef" <> ["position" .= pos ni, "next" .= p']
    NPEDefn fname -> tag "defn" <> ["fname" .= pretty fname]
    NPEStmt ni p' -> tag "stmt" <> ["position" .= pos ni, "next" .= p']
    NPETypeOfExpr ni p' -> tag "typeOfExpr" <> ["position" .= pos ni, "next" .= p']
  where
    pos = PP.render . PP.prettyPos
    pretty :: PP.Pretty a => a -> String
    pretty = PP.render . PP.pretty 


output :: Bool -> IO.Handle -> FilePath -> FilePath -> R.Message -> IO ()
output _isFile h = \workDir fp rmsg ->
  case rmsg of
    R.Normal warns_ ->
      let warns = R.getCentrinelAnalysisErrors warns_
      in
        unless (null warns) $ put $ TranslationUnitMessage
        { workingDirectory = workDir
        , translationUnit = fp
        , message = NormalMessages
          { isAbnormal = False
          , messages = map massageError warns
          }
        }
    R.Abnormal centErr ->
      put $ TranslationUnitMessage workDir fp $ case centErr of
        R.CentCPPError exitCode -> ToolFailMessage { toolFailure = CPPToolFail exitCode }
        R.CentParseError err -> ToolFailMessage { toolFailure = ParseToolFail err }
        R.CentAbortedAnalysisError errs -> NormalMessages
          { isAbnormal = True
          , messages = map massageError (R.getCentrinelAnalysisErrors errs)
          }
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
