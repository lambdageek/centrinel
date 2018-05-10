module Centrinel.FunTags.Tag where

import qualified Data.Set as S

-- | A single tag
newtype BareTag = BareTag String
  deriving (Eq, Ord, Show)

singleBareTag :: String -> S.Set BareTag
singleBareTag = S.singleton . BareTag

