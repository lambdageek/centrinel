{-# language FunctionalDependencies, UndecidableInstances #-}
module Centrinel.Region.Ident where

import qualified Language.C.Data.Ident as Id
import qualified Control.Monad.Reader as Rd
import qualified Control.Monad.State.Lazy as StL
import Control.Monad.Trans.Class

-- | C identifiers that may have a region placed on them: either struct tags or typedefs.
-- 
data RegionIdent = StructTagId !Id.SUERef
                 | TypedefId !Id.Ident
                 deriving (Show, Eq, Ord)

-- | Monads @m@ that assign region variables @v@ to identifiers @i@
class Monad m => RegionAssignment i v m | m -> i v where
  assignRegion :: i -> m v

instance RegionAssignment i v m => RegionAssignment i v (Rd.ReaderT r m) where
  assignRegion = lift . assignRegion

instance RegionAssignment i v m => RegionAssignment i v (StL.StateT s m) where
  assignRegion = lift . assignRegion
