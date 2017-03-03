-- | Monad transformer that provides a view of the region unification results
{-# language GeneralizedNewtypeDeriving #-}
module Centrinel.RegionResultMonad (
  -- * RegionResultMonad type class
  RegionResultMonad(..)
  -- * InferenceResultT monad transformer
  , InferenceResultT
  , runInferenceResultT
  ) where

import Control.Monad.Reader (runReaderT, ReaderT, asks)
import Control.Monad.Writer (WriterT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (MonadTrans(..))

import qualified Data.Map as Map

import qualified Language.C.Data.Ident as C
import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Analysis.TravMonad as CM

import Data.Assoc

import Centrinel.Region
import Centrinel.RegionInferenceResult

class Monad m => RegionResultMonad m where
  rrStructTagRegion :: StructTagRef -> m RegionScheme
  rrLookupTypedef :: C.Ident -> m C.TypeDef

instance (Monoid w, RegionResultMonad m) => RegionResultMonad (WriterT w m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

instance RegionResultMonad m => RegionResultMonad (ReaderT r m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

instance RegionResultMonad m => RegionResultMonad (ExceptT e m) where
  rrStructTagRegion = lift . rrStructTagRegion
  rrLookupTypedef = lift . rrLookupTypedef

newtype InferenceResultT m a = InferenceResultT { unInferenceResultT :: ReaderT (Map.Map C.Ident C.TypeDef, RegionInferenceResult) m a }
  deriving (Functor, Applicative, Monad)

instance CM.MonadCError m => CM.MonadCError (InferenceResultT m) where
  throwTravError = InferenceResultT . lift . CM.throwTravError
  catchTravError = error "finish catchTravError for InferenceResultT" -- FIXME: finish me
  recordError = InferenceResultT . lift . CM.recordError
  getErrors = InferenceResultT $ lift $ CM.getErrors
  

instance Monad m => RegionResultMonad (InferenceResultT m) where
  rrStructTagRegion sr = InferenceResultT $ asks (certain . Map.lookup sr . Data.Assoc.getAssocMap . snd)
    where
      certain Nothing = PolyRS
      certain (Just a) = a
  rrLookupTypedef ident = InferenceResultT $ asks (certain . Map.lookup ident . fst)
    where
      certain Nothing = error "cannot get Nothing  from rrLookupTypedef"
      certain (Just a) = a

instance MonadTrans InferenceResultT where
  lift = InferenceResultT . lift

runInferenceResultT :: Monad m => InferenceResultT m a -> Map.Map C.Ident C.TypeDef -> RegionInferenceResult -> m a
runInferenceResultT comp = curry (runReaderT (unInferenceResultT comp))
