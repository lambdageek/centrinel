-- | Provides a monad transformer that has a local def table.
-- An instance of 'CM.MonadSymtab' and 'H.HasDefTable'
{-# language GeneralizedNewtypeDeriving,
      UndecidableInstances,
      StandaloneDeriving
  #-}
module Centrinel.Control.Monad.LocalSymtab (
  LocalSymtabT(..)
  , evalLocalSymtabT
  ) where

import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.State.Strict (StateT (..), evalStateT)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans (MonadTrans (..))

import Centrinel.Control.Monad.Class.RegionResult

import qualified Language.C.Analysis.DefTable as CDT
import qualified Language.C.Analysis.TravMonad as CM

-- TODO: move this to a separate file
newtype LocalSymtabT m a = LocalSymtabT { unLocalSymtabT :: StateT CDT.DefTable m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

deriving instance MonadReader r m => MonadReader r (LocalSymtabT m)
deriving instance MonadWriter w m => MonadWriter w (LocalSymtabT m)
deriving instance RegionResultMonad m => RegionResultMonad (LocalSymtabT m)

instance Monad m => CM.MonadSymtab (LocalSymtabT m) where
  getDefTable = LocalSymtabT get
  withDefTable f = LocalSymtabT $ do
    x <- get
    let ~(a, y) = f x
    put y
    return a

instance CM.MonadName m => CM.MonadName (LocalSymtabT m) where
  genName = lift CM.genName

instance CM.MonadCError m => CM.MonadCError (LocalSymtabT m) where
  throwTravError = lift . CM.throwTravError
  catchTravError (LocalSymtabT c) handler = LocalSymtabT (State.liftCatch CM.catchTravError c (unLocalSymtabT . handler))
  recordError = lift . CM.recordError
  getErrors = lift CM.getErrors

-- | Note that global handlers will be invoked with a copy of the local DefTable in the global DefTable which
-- is then copied back into the local DefTable after the handlers return, and the global DefTable is restored.
instance CM.MonadTrav m => CM.MonadTrav (LocalSymtabT m) where
  handleDecl evt = do
    -- snapshot global def table
    snapshotGlobal <- lift CM.getDefTable
    lcl <- CM.getDefTable
    -- set global def table to current local
    final <- lift $ do
      setDefTable lcl
      CM.handleDecl evt -- XXX catch and restore?
      CM.getDefTable
    -- set local def table to final global
    setDefTable final
    -- restore global to the snapshot
    lift (setDefTable snapshotGlobal)

setDefTable :: CM.MonadSymtab m => CDT.DefTable -> m ()
setDefTable t = CM.withDefTable (const ((), t))

evalLocalSymtabT :: Monad m => CDT.DefTable -> LocalSymtabT m a -> m a
evalLocalSymtabT st0 comp = evalStateT (unLocalSymtabT comp) st0

