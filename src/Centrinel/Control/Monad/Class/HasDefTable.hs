module Centrinel.Control.Monad.Class.HasDefTable where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Writer (WriterT (..))
import Control.Monad.State.Strict (StateT (..))

import Data.Monoid (Monoid (..))

import qualified Language.C.Analysis.DefTable as CDT

class HasDefTable m where
  getDefTable :: m CDT.DefTable

instance HasDefTable m => HasDefTable (ReaderT r m) where
  getDefTable = ReaderT (const getDefTable)

instance (Monoid w, Monad m, HasDefTable m) => HasDefTable (WriterT w m) where
  getDefTable = WriterT (getDefTable >>= \x -> return (x, mempty))
  
instance (Monad m, HasDefTable m) => HasDefTable (StateT s m) where
  getDefTable = StateT (\s -> getDefTable >>= \x -> return (x, s))

