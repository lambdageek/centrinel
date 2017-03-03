-- | Region unification monad and term structure
{-# language RankNTypes, FunctionalDependencies, GeneralizedNewtypeDeriving #-}
module Centrinel.RegionUnification where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Control.Unification as U
import qualified Control.Unification.IntVar as U

import qualified Control.Unification.IntVar.Extras as ExtraU

import qualified Language.C.Data.Node as C
import qualified Language.C.Analysis.TravMonad as C

import Centrinel.Region (Region, RegionScheme(..))
import Centrinel.RegionUnification.Term
import Centrinel.RegionMismatchError (RegionMismatchError)
 
class Monad m => RegionUnification v m | m -> v where
  newRegion :: m v
  sameRegion :: v -> v -> m ()
  constantRegion :: v -> Region -> m ()
  -- attach
  regionAddLocation :: C.CNode n => v -> n -> m ()

class ApplyUnificationState m where
  applyUnificationState :: RegionUnifyTerm -> m RegionUnifyTerm

newtype UnifyRegT m a = UnifyRegT { unUnifyRegT :: U.IntBindingT RegionTerm m a}
  deriving (Functor, Applicative, Monad, MonadTrans)



instance C.MonadCError m => C.MonadCError (UnifyRegT m) where
  throwTravError e = UnifyRegT $ lift $ C.throwTravError e
  catchTravError m handler = liftCatch (C.catchTravError) m handler
  recordError e = UnifyRegT $ lift $ C.recordError e
  getErrors = UnifyRegT $ lift $ C.getErrors

instance Monad m => U.BindingMonad RegionTerm RegionVar (UnifyRegT m) where
  lookupVar = UnifyRegT . fmap (fmap (fmap RegionVar)) . U.lookupVar . unRegionVar
  freeVar = UnifyRegT $ fmap RegionVar $ U.freeVar
  bindVar v t = UnifyRegT $ U.bindVar (unRegionVar v) (fmap unRegionVar t)

instance C.MonadCError m => RegionUnification RegionVar (UnifyRegT m) where
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

