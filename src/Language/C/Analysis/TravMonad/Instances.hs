-- | Defines extra instances of the monad classes in "Lanugage.C.Analysis.TravMonad"
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.C.Analysis.TravMonad.Instances () where

import qualified Control.Monad.Trans.Reader as Rd
import qualified Control.Monad.Trans.Writer.Lazy as WrL
import Control.Monad.Trans (MonadTrans (..))

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
