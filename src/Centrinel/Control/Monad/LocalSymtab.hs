-- | Provides a monad transformer that has a local def table.
-- An instance of 'CM.MonadSymtab' and 'H.HasDefTable'
{-# language GeneralizedNewtypeDeriving #-}
module Centrinel.Control.Monad.LocalSymtab (
  LocalSymtabT(..)
  , evalLocalSymtabT
  ) where

import Control.Monad.State.Class
import Control.Monad.State.Strict (StateT (..), evalStateT)
import Control.Monad.Trans (MonadTrans (..))

import qualified Language.C.Analysis.DefTable as CDT
import qualified Language.C.Analysis.TravMonad as CM

-- TODO: move this to a separate file
newtype LocalSymtabT m a = LocalSymtabT { unLocalSymtabT :: StateT CDT.DefTable m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => CM.MonadSymtab (LocalSymtabT m) where
  getDefTable = LocalSymtabT get
  withDefTable f = LocalSymtabT $ do
    x <- get
    let ~(a, y) = f x
    put y
    return a

instance MonadTrans LocalSymtabT where
  lift = LocalSymtabT . lift

evalLocalSymtabT :: Monad m => CDT.DefTable -> LocalSymtabT m a -> m a
evalLocalSymtabT st0 comp = evalStateT (unLocalSymtabT comp) st0
