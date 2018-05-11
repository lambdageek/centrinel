module Centrinel.FunTags.Tag where

import qualified Data.Set as S

-- | A single tag
newtype BareTag = BareTag String
  deriving (Eq, Ord, Show)

type BareTagSet = S.Set BareTag

singleBareTag :: String -> BareTagSet
singleBareTag = S.singleton . BareTag

