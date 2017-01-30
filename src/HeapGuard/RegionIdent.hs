{-# language FunctionalDependencies #-}
module HeapGuard.RegionIdent where

import qualified Language.C.Data.Ident as Id

-- | C identifiers that may have a region placed on them: either struct tags or typedefs.
-- 
data RegionIdent = StructTagId !Id.SUERef
                 | TypedefId !Id.Ident
                 deriving (Show, Eq, Ord)

-- | Monads @m@ that assign region variables @v@ to identifiers @i@
class Monad m => RegionAssignment i v m | m -> i v where
  assignRegion :: i -> m v


