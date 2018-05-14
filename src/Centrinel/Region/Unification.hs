-- | Region unification monad and term structure
{-# language RankNTypes, FunctionalDependencies, GeneralizedNewtypeDeriving
    , UndecidableInstances
  #-}
module Centrinel.Region.Unification where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Control.Monad.State.Lazy as StL
import qualified Control.Monad.Reader as Rd

import qualified Control.Unification as U
import qualified Control.Unification.IntVar as U

import qualified Control.Unification.IntVar.Extras as ExtraU

import qualified Language.C.Data.Node as C
import qualified Language.C.Analysis.TravMonad as C

import Centrinel.Region.Region (Region, RegionScheme(..))
import Centrinel.Region.Unification.Term
import Centrinel.RegionMismatchError (RegionMismatchError)
 
class Monad m => RegionUnification m where
  newRegion :: m RegionVar
  sameRegion :: RegionVar -> RegionVar -> m ()
  constantRegion :: RegionVar -> Region -> m ()
  -- attach
  regionAddLocation :: C.CNode n => RegionVar -> n -> m ()

class ApplyUnificationState m where
  applyUnificationState :: RegionUnifyTerm -> m RegionUnifyTerm

newtype UnifyRegT m a = UnifyRegT { unUnifyRegT :: U.IntBindingT RegionTerm m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

instance C.MonadName m => C.MonadName (UnifyRegT m) where
  genName = UnifyRegT $ lift $ C.genName

instance C.MonadSymtab m => C.MonadSymtab (UnifyRegT m) where
  getDefTable = UnifyRegT $ lift $ C.getDefTable
  withDefTable = UnifyRegT . lift . C.withDefTable

instance C.MonadCError m => C.MonadCError (UnifyRegT m) where
  throwTravError e = UnifyRegT $ lift $ C.throwTravError e
  catchTravError m handler = liftCatch (C.catchTravError) m handler
  recordError e = UnifyRegT $ lift $ C.recordError e
  getErrors = UnifyRegT $ lift $ C.getErrors

instance Monad m => U.BindingMonad RegionTerm RegionVar (UnifyRegT m) where
  lookupVar = UnifyRegT . fmap (fmap (fmap RegionVar)) . U.lookupVar . unRegionVar
  freeVar = UnifyRegT $ fmap RegionVar $ U.freeVar
  bindVar v t = UnifyRegT $ U.bindVar (unRegionVar v) (fmap unRegionVar t)

instance C.MonadCError m => RegionUnification (UnifyRegT m) where
  newRegion = UnifyRegT (fmap RegionVar U.freeVar)
  sameRegion v1 v2 = do
    e <- unify (regionUnifyVar v1) (regionUnifyVar v2)
    case e of
      Right _uterm -> return ()
      Left err -> C.throwTravError err
  constantRegion v c = do
    e <- unify (regionUnifyVar v) (regionUnifyTerm $ constRegionTerm c)
    case e of
      Right _uterm -> return ()
      Left err -> C.throwTravError err
  regionAddLocation v n = do
    e <- unify (regionUnifyVar v) (regionUnifyTerm $ dummyLocationTerm $ C.nodeInfo n)
    case e of
      Right _uterm -> return ()
      Left _err -> error "unexpected unification failure from regionAddLocation"

instance C.MonadCError m => ApplyUnificationState (UnifyRegT m) where
  applyUnificationState t = do
    ans <- runFailableUnify $ U.applyBindings t
    case ans of
      Left err -> C.throwTravError err
      Right term -> return term

runUnifyRegT :: Monad m => UnifyRegT m a -> m a
runUnifyRegT = U.evalIntBindingT . unUnifyRegT

liftCatch :: Monad m => (forall b . m b -> (e -> m b) -> m b) -> UnifyRegT m a -> (e -> UnifyRegT m a) -> UnifyRegT m a
liftCatch catch (UnifyRegT comp) handler_ =
  UnifyRegT (ExtraU.liftCatch catch comp handler)
  where handler = unUnifyRegT . handler_

-- | Given a region unification term (assumed to already have had unification bindings applied),
-- return the @FixedRS r@ where @r@ is the inferred constant region or @PolyRS@ if the region was an unconstrained variable.
extractRegionScheme :: RegionUnifyTerm -> RegionScheme
extractRegionScheme (U.UVar {}) = PolyRS
extractRegionScheme (U.UTerm (ConstRegionTerm r _l)) = FixedRS r
extractRegionScheme (U.UTerm (DummyLocTerm {})) = PolyRS -- dummy loc term is same as an unconstrained uvar


runFailableUnify :: ExceptT RegionMismatchError m a -> m (Either RegionMismatchError a)
runFailableUnify = runExceptT


unify :: Monad m => RegionUnifyTerm -> RegionUnifyTerm -> UnifyRegT m (Either RegionMismatchError RegionUnifyTerm)
unify m1 m2 = runFailableUnify $ U.unify m1 m2


instance RegionUnification m => RegionUnification (StL.StateT s m) where
  newRegion = lift newRegion
  sameRegion = \v1 v2 -> lift (sameRegion v1 v2)
  constantRegion = \v r -> lift (constantRegion v r)
  regionAddLocation = \v n -> lift (regionAddLocation v n)

instance RegionUnification m => RegionUnification (Rd.ReaderT r m) where
  newRegion = lift newRegion
  sameRegion = \v1 v2 -> lift (sameRegion v1 v2)
  constantRegion = \v r -> lift (constantRegion v r)
  regionAddLocation = \v n -> lift (regionAddLocation v n)

instance (Monad m, ApplyUnificationState m) => ApplyUnificationState (StL.StateT s m) where
  applyUnificationState = lift . applyUnificationState

instance (Monad m, ApplyUnificationState m) => ApplyUnificationState (Rd.ReaderT r m) where
  applyUnificationState = lift . applyUnificationState
