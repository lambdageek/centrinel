-- | Types for the analysis reports
{-# language DeriveGeneric #-}
module Centrinel.Report.Types where

import GHC.Generics (Generic)

import qualified Data.Aeson
import Data.Aeson (ToJSON)

data Message = Normal !String
  | Abnormal !String
  | Verbose !Bool !String
  deriving (Generic)

instance ToJSON Message where
  toEncoding = Data.Aeson.genericToEncoding Data.Aeson.defaultOptions
