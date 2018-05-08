-- | Types for the analysis reports
module Centrinel.Report.Types (Message(..), module Centrinel.Types) where

import Centrinel.Types

data Message = Normal !CentrinelAnalysisErrors
  | Abnormal !CentrinelFatalError

