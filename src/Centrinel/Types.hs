-- | Types relevant to the toplevel interface to Centrinel
module Centrinel.Types where

import qualified System.Exit 

import Language.C.Parser (ParseError)
import Language.C.Data.Error (CError)

import Centrinel.RegionMismatchError (RegionMismatchError)
import Centrinel.NakedPointerError (NakedPointerError)

-- | Errors that may prematurely abort a centrinel analysis of a given
-- translation unit.
data CentrinelFatalError =
  CentCPPError !System.Exit.ExitCode -- ^ Error while invoking the C preprocessor
  | CentParseError !ParseError -- ^ Error while parsing the input C file
  -- | Error reports from Centrinel when analysis of a translation unit was aborted.
  | CentAbortedAnalysisError ![CentrinelAnalysisError]

-- | Errors that may arize in the course of centrinel analysis
data CentrinelAnalysisError =
  -- | Error due to C semantics, independent of the centrinel analysis
  CACError !CError
  -- | Error due to region inference
  | CARegionMismatchError !RegionMismatchError
  -- | Error due to naked pointer analysis
  | CANakedPointerError !NakedPointerError
