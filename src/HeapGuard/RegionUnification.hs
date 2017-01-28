{-# LANGUAGE RankNTypes #-}
{-# language FunctionalDependencies, GeneralizedNewtypeDeriving #-}
module HeapGuard.RegionUnification where

import Control.Monad.Trans.Class
import qualified Control.Monad.State.Class as St
import Control.Monad.Except

import Data.Traversable (fmapDefault, foldMapDefault)

import qualified Control.Unification as U
import qualified Control.Unification.IntVar as U
import qualified Control.Unification.Types as U

newtype RegionVar = RegionVar U.IntVar

-- trivial unification terms with no structure
data RegionTerm a = RegionTerm

regionTerm :: RegionTerm a
regionTerm = RegionTerm

instance U.Unifiable RegionTerm where
  zipMatch RegionTerm RegionTerm = Just RegionTerm

instance Functor RegionTerm where
  fmap = fmapDefault

instance Foldable RegionTerm where
  foldMap = foldMapDefault

instance Traversable RegionTerm where
  traverse _f RegionTerm = pure RegionTerm
  
class Monad m => RegionUnification v m | m -> v where
  newRegion :: m v
  sameRegion :: v -> v -> m ()

newtype UnifyRegT m a = UnifyRegT { unUnifyRegT :: U.IntBindingT RegionTerm m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => RegionUnification RegionVar (UnifyRegT m) where
  newRegion = UnifyRegT (fmap RegionVar U.freeVar)
  sameRegion (RegionVar v1) (RegionVar v2) = UnifyRegT $ do
    e <- runExceptT $ U.unify (U.UVar v1) (U.UVar v2)
    case e of
      Right _uterm -> return ()
      Left (U.OccursFailure (U.IntVar _) (U.UVar _v'))  -> error "occurs failure cannot happen RegionTerm has no structure"
      Left (U.OccursFailure _v (U.UTerm RegionTerm))  -> error "occurs failure cannot happen RegionTerm has no structure"
      Left (U.MismatchFailure _t1 _t2) -> error "mismatch failure cannot happen RegionTerm has no structure"

runUnifyRegT :: Monad m => UnifyRegT m a -> m a
runUnifyRegT = U.evalIntBindingT . unUnifyRegT

liftCatch :: Monad m => (forall b . m b -> (e -> m b) -> m b) -> UnifyRegT m a -> (e -> UnifyRegT m a) -> UnifyRegT m a
liftCatch catch (UnifyRegT comp) handler_ =
  let handler = unUnifyRegT . handler_
      runWithState :: St.MonadState s m => m a -> s -> m a
      runWithState c st = St.put st >> c
      guardedComp st = U.runIntBindingT (runWithState comp st) `catch` (\e -> U.runIntBindingT (runWithState (handler e) st))
  in UnifyRegT $ do
    initialSt <- St.get
    (ans, finalSt) <- lift $ guardedComp initialSt
    St.put finalSt
    return ans
  
         
      
