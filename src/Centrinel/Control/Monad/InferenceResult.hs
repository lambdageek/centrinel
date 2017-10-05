-- | A concrete monad transformer that is an instance of
-- 'Centrinel.Control.Monad.Class.RegionResult'
{-# language GeneralizedNewtypeDeriving #-}
module Centrinel.Control.Monad.InferenceResult (
  InferenceResultT (..)
  , runInferenceResultT
  ) where


import Control.Monad.Reader (runReaderT, ReaderT, asks)
import Control.Monad.Trans.Class (MonadTrans(..))

import qualified Data.Map as Map

import qualified Language.C.Data.Ident as C
import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Analysis.TravMonad as CM

import Centrinel.Control.Monad.Class.RegionResult
import Centrinel.Region.Region (RegionScheme(..))
import Centrinel.RegionInferenceResult (RegionInferenceResult)
import Data.Assoc


newtype InferenceResultT m a = InferenceResultT { unInferenceResultT :: ReaderT (Map.Map C.Ident C.TypeDef, RegionInferenceResult) m a }
  deriving (Functor, Applicative, Monad)

instance CM.MonadCError m => CM.MonadCError (InferenceResultT m) where
  throwTravError = InferenceResultT . lift . CM.throwTravError
  catchTravError = error "finish catchTravError for InferenceResultT" -- FIXME: finish me
  recordError = InferenceResultT . lift . CM.recordError
  getErrors = InferenceResultT $ lift $ CM.getErrors
  
instance CM.MonadName m => CM.MonadName (InferenceResultT m) where
  genName = lift CM.genName

instance CM.MonadSymtab m => CM.MonadSymtab (InferenceResultT m) where
  getDefTable = lift CM.getDefTable
  withDefTable = lift . CM.withDefTable

instance Monad m => RegionResultMonad (InferenceResultT m) where
  rrStructTagRegion sr = InferenceResultT $ asks (certain . Map.lookup sr . Data.Assoc.getAssocMap . snd)
    where
      certain Nothing = PolyRS
      certain (Just a) = a
  rrLookupTypedef ident = InferenceResultT $ asks (certain . Map.lookup ident . fst)
    where
      certain Nothing = error "cannot get Nothing  from rrLookupTypedef"
      certain (Just a) = a

instance CM.MonadTrav m => CM.MonadTrav (InferenceResultT m) where
  handleDecl = lift . CM.handleDecl

instance MonadTrans InferenceResultT where
  lift = InferenceResultT . lift

runInferenceResultT :: InferenceResultT m a -> Map.Map C.Ident C.TypeDef -> RegionInferenceResult -> m a
runInferenceResultT comp = curry (runReaderT (unInferenceResultT comp))
