{-# language FunctionalDependencies, UndecidableInstances #-}
module Centrinel.Region.Ident where

import qualified Language.C.Data.Ident as Id

-- | C identifiers that may have a region placed on them: either struct tags or typedefs.
-- 
data RegionIdent = StructTagId !Id.SUERef
                 | TypedefId !Id.Ident
                 deriving (Show, Eq, Ord)

