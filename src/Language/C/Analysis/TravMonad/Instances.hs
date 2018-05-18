-- | Defines extra instances of the monad classes in "Lanugage.C.Analysis.TravMonad"
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.C.Analysis.TravMonad.Instances () where

import qualified Control.Monad.Trans.Reader as Rd
import qualified Control.Monad.Trans.State.Lazy as StL
import qualified Control.Monad.Trans.State.Strict as StS
import qualified Control.Monad.Trans.Writer.Lazy as WrL
import qualified Control.Monad.Trans.Except as Ex
import Control.Monad.Trans (MonadTrans (..))

import qualified Control.Unification.IntVar as UInt
import qualified Control.Unification.IntVar.Extras as UIntExtras

import Data.Monoid (Monoid)

import Language.C.Analysis.TravMonad (MonadCError (..), MonadName (..), MonadSymtab (..), MonadTrav (..))

instance MonadCError m => MonadCError (Rd.ReaderT r m) where
  throwTravError = lift . throwTravError
  catchTravError = Rd.liftCatch catchTravError
  recordError = lift . recordError
  getErrors = lift getErrors

instance MonadName m => MonadName (Rd.ReaderT r m) where
  genName = lift genName

instance MonadSymtab m => MonadSymtab (Rd.ReaderT r m) where
  getDefTable = lift getDefTable
  withDefTable = lift . withDefTable

instance MonadTrav m => MonadTrav (Rd.ReaderT r m) where
  handleDecl = lift . handleDecl

instance (Monoid w, MonadCError m) => MonadCError (WrL.WriterT w m) where
  throwTravError = lift . throwTravError
  catchTravError = WrL.liftCatch catchTravError
  recordError = lift . recordError
  getErrors = lift getErrors

instance (Monoid w, MonadName m) => MonadName (WrL.WriterT w m) where
  genName = lift genName

instance (Monoid w, MonadSymtab m) => MonadSymtab (WrL.WriterT w m) where
  getDefTable = lift getDefTable
  withDefTable = lift . withDefTable

instance (Monoid w, MonadTrav m) => MonadTrav (WrL.WriterT w m) where
  handleDecl = lift . handleDecl

instance MonadName m => MonadName (StL.StateT s m) where
  genName = lift genName

instance MonadSymtab m => MonadSymtab (StL.StateT s m) where
  getDefTable = lift getDefTable
  withDefTable = lift . withDefTable

instance MonadTrav m => MonadTrav (StL.StateT s m) where
  handleDecl = lift . handleDecl

instance MonadCError m => MonadCError (StL.StateT s m) where
  throwTravError = lift . throwTravError
  catchTravError = StL.liftCatch catchTravError
  recordError = lift . recordError
  getErrors = lift getErrors

instance MonadName m => MonadName (StS.StateT s m) where
  genName = lift genName

instance MonadSymtab m => MonadSymtab (StS.StateT s m) where
  getDefTable = lift getDefTable
  withDefTable = lift . withDefTable

instance MonadTrav m => MonadTrav (StS.StateT s m) where
  handleDecl = lift . handleDecl

instance MonadCError m => MonadCError (StS.StateT s m) where
  throwTravError = lift . throwTravError
  catchTravError = StS.liftCatch catchTravError
  recordError = lift . recordError
  getErrors = lift getErrors

instance MonadName m => MonadName (Ex.ExceptT e m) where
  genName = lift genName

instance MonadSymtab m => MonadSymtab (Ex.ExceptT e m) where
  getDefTable = lift getDefTable
  withDefTable = lift . withDefTable

instance MonadTrav m => MonadTrav (Ex.ExceptT e m) where
  handleDecl = lift . handleDecl

instance MonadCError m => MonadCError (Ex.ExceptT e m) where
  throwTravError = lift . throwTravError
  catchTravError = \comp handler -> Ex.ExceptT (catchTravError (Ex.runExceptT comp) (Ex.runExceptT . handler))
  recordError = lift . recordError
  getErrors = lift getErrors

instance MonadName m => MonadName (UInt.IntBindingT t m) where
  genName = lift genName

instance MonadSymtab m => MonadSymtab (UInt.IntBindingT t m) where
  getDefTable = lift getDefTable
  withDefTable = lift . withDefTable

instance MonadTrav m => MonadTrav (UInt.IntBindingT t m) where
  handleDecl = lift . handleDecl

instance MonadCError m => MonadCError (UInt.IntBindingT t m) where
  throwTravError = lift . throwTravError
  catchTravError = UIntExtras.liftCatch catchTravError
  recordError = lift . recordError
  getErrors = lift getErrors
