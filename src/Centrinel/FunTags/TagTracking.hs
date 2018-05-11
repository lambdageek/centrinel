-- | Defines the 'TagTrackingT' monad transformer for running the tag gathering
-- analysis.
{-# language GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Centrinel.FunTags.TagTracking (TagTrackingT, runTagTrackingT, TagInferenceResult) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State.Strict (StateT (..), get, gets, modify, evalStateT)

import qualified Data.Map.Strict as M

import qualified Control.Unification as Unif
import qualified Control.Unification.IntVar as Unif
import qualified Control.Unification.Types as Unif

import Language.C.Data.Ident (Ident)

import Centrinel.FunTags.Tag
import Centrinel.FunTags.Class
import Centrinel.FunTags.Unification.Term (TagPreTerm, TagUVar, representTags)

type TaggingFailure = Unif.UFailure TagPreTerm TagUVar

-- | A helper monad that just takes care of the unification.
newtype TaggingT m a = TaggingT { unTaggingT :: ExceptT TaggingFailure (Unif.IntBindingT TagPreTerm m) a }
  deriving (Functor, Applicative, Monad)

freeTaggingVar :: Monad m => TaggingT m TagUVar
freeTaggingVar = TaggingT $ lift $ Unif.freeVar

instance Monad m => TagUnification (TaggingT m) where
  unifyTagTerms t1 t2 = void $ TaggingT $ Unif.unify t1 t2
instance MonadTrans TaggingT where
  lift m = TaggingT (lift $ lift m)
  
newtype TagTrackingT m a = TagTrackingT { unTagTrackingT :: TaggingT (StateT (M.Map Ident Unif.IntVar) m) a }
  deriving (Functor, Applicative, Monad, TagUnification)

instance Monad m => TagAssignment (TagTrackingT m) where
  assignTag ident = TagTrackingT $ do
    m <- lift $ gets $ M.lookup ident
    v <- case m of
      Just v -> return v
      Nothing -> do
        v <- freeTaggingVar
        lift $ modify (M.insert ident v)
        return v
    return (Unif.UVar v)

type TagInferenceResult = M.Map Ident BareTagSet

runTagTrackingT :: forall m a . Monad m => TagTrackingT m a -> m (Either TaggingFailure (a, TagInferenceResult))
runTagTrackingT c = evalStateT (Unif.evalIntBindingT $ runExceptT c') mempty
  where
    c' :: ExceptT TaggingFailure (Unif.IntBindingT TagPreTerm (StateT (M.Map Ident Unif.IntVar) m)) (a, TagInferenceResult)
    c' = do
      x <- unTaggingT $ unTagTrackingT c
      st <- lift $ lift $ gets (fmap Unif.UVar)
      m <- Unif.applyBindingsAll st
      return (x, fmap representTags m)
