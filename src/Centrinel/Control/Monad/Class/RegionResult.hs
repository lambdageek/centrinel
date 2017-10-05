-- | Deifnes the typeclass of monads that carry region inference
-- results.
module Centrinel.Control.Monad.Class.RegionResult (
  -- * RegionResultMonad type class
  RegionResultMonad(..)
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Class (MonadTrans(..))

import qualified Language.C.Data.Ident as C
import qualified Language.C.Analysis.SemRep as C

import Centrinel.Region.Region (RegionScheme)
import Centrinel.RegionInferenceResult (StructTagRef)

class Monad m => RegionResultMonad m where
  rrStructTagRegion :: StructTagRef -> m RegionScheme
  rrLookupTypedef :: C.Ident -> m C.TypeDef

instance (Monoid w, RegionResultMonad m) => RegionResultMonad (WriterT w m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

instance RegionResultMonad m => RegionResultMonad (ReaderT r m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

instance RegionResultMonad m => RegionResultMonad (StateT s m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

instance RegionResultMonad m => RegionResultMonad (ExceptT e m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef


