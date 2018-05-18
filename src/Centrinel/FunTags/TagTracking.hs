-- | Defines the 'TagTrackingT' monad transformer for running the tag gathering
-- analysis.
{-# language GeneralizedNewtypeDeriving #-}
module Centrinel.FunTags.TagTracking
  ( TagTrackingT
  , runTagTrackingT, evalTagTrackingT
  ,FunTagInferenceResult
  , getFunTagInferenceResult
  ) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State.Strict (StateT (..), gets, modify, evalStateT)

import qualified Data.Map.Strict as M

import qualified Control.Unification as Unif
import qualified Control.Unification.IntVar as Unif
import qualified Control.Unification.Types as Unif

import Language.C.Data.Ident (Ident)
import qualified Language.C.Analysis.TravMonad as AM

import Language.C.Analysis.TravMonad.Instances ()

import qualified Centrinel.Region.Class as R

import Centrinel.FunTags.Tag
import Centrinel.FunTags.Class
import Centrinel.FunTags.Unification.Term (TagPreTerm, TagUVar, representTags)

type TaggingFailure = Unif.UFailure TagPreTerm TagUVar

-- | A helper monad that just takes care of the unification.
newtype TaggingT m a = TaggingT { unTaggingT :: ExceptT TaggingFailure (Unif.IntBindingT TagPreTerm m) a }
  deriving (Functor, Applicative, Monad
           , AM.MonadName, AM.MonadSymtab, AM.MonadCError)

freeTaggingVar :: Monad m => TaggingT m TagUVar
freeTaggingVar = TaggingT $ lift $ Unif.freeVar

instance Monad m => TagUnification (TaggingT m) where
  unifyTagTerms t1 t2 = void $ TaggingT $ Unif.unify t1 t2
instance MonadTrans TaggingT where
  lift = TaggingT . lift . lift
  
-- map C identifiers to their assigned unification variables
type UnifAssignment = M.Map Ident Unif.IntVar

newtype TagTrackingT m a = TagTrackingT { unTagTrackingT :: TaggingT (StateT UnifAssignment m) a }
  deriving (Functor, Applicative, Monad, TagUnification
           , AM.MonadName, AM.MonadSymtab, AM.MonadCError)

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

instance MonadTrans TagTrackingT where
  lift = TagTrackingT . lift . lift

instance R.RegionUnification m => R.RegionUnification (TagTrackingT m) where
  newRegion = lift R.newRegion
  sameRegion = \m1 m2 -> lift $ R.sameRegion m1 m2
  constantRegion = \v r -> lift $ R.constantRegion v r
  regionAddLocation = \v n -> lift $ R.regionAddLocation v n

instance R.RegionAssignment m => R.RegionAssignment (TagTrackingT m) where
  assignRegion = lift . R.assignRegion

type FunTagInferenceResult = M.Map Ident BareTagSet

-- | Run the given computation and return its answer and the final mapping from
-- seen C idents to the tags they were unified with.
runTagTrackingT :: Monad m => TagTrackingT m a -> m (Either TaggingFailure (a, FunTagInferenceResult))
runTagTrackingT c =
  -- n.b. order matters - get tags after unification effects in c
  let c' = (,) <$> c <*> getFunTagInferenceResult
  in evalTagTrackingT c'

-- | Run the given computation and throw away unification state and C ident assignments.
evalTagTrackingT :: Monad m => TagTrackingT m a -> m (Either TaggingFailure a)
evalTagTrackingT = flip evalStateT mempty . evalTaggingT .  unTagTrackingT

evalTaggingT :: Monad m => TaggingT m a -> m (Either TaggingFailure a)
evalTaggingT = Unif.evalIntBindingT . runExceptT . unTaggingT 


-- | Get the current mapping from seen C idents to the tag sets they're currently unified with.
-- Typically this should be called at the end of analysis when there is nothing more to do.
getFunTagInferenceResult :: Monad m => TagTrackingT m FunTagInferenceResult
getFunTagInferenceResult = TagTrackingT $ do
  st <- lift $ gets (fmap Unif.UVar)
  m <- TaggingT $ Unif.applyBindingsAll st
  return (fmap representTags m)
