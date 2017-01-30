{-# LANGUAGE RankNTypes #-}
{-# language FunctionalDependencies, GeneralizedNewtypeDeriving #-}
module HeapGuard.RegionUnification where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Data.Traversable (fmapDefault, foldMapDefault)

import qualified Control.Unification as U
import qualified Control.Unification.IntVar as U
import qualified Control.Unification.Types as U

import qualified Control.Unification.IntVar.Extras as ExtraU

import HeapGuard.Region (Region)

newtype RegionVar = RegionVar { unRegionVar :: U.IntVar }
  deriving (Show)

-- trivial unification terms with no structure
data RegionTerm a = ConstRegionTerm Region
  deriving (Show)

constRegionTerm :: Region -> RegionTerm a
constRegionTerm = ConstRegionTerm

type RegionUnifyTerm = U.UTerm RegionTerm RegionVar

regionUnifyVar :: RegionVar -> RegionUnifyTerm
regionUnifyVar = U.UVar

regionUnifyTerm :: RegionTerm RegionUnifyTerm -> RegionUnifyTerm
regionUnifyTerm = U.UTerm

instance U.Unifiable RegionTerm where
  zipMatch (ConstRegionTerm r1) (ConstRegionTerm r2) | r1 == r2 = Just (ConstRegionTerm r1)
                                                     | otherwise = Nothing

unify :: Monad m => RegionUnifyTerm -> RegionUnifyTerm -> UnifyRegT m (Either (U.UFailure RegionTerm RegionVar) RegionUnifyTerm)
unify m1 m2 = UnifyRegT $ runExceptT $ fmap to $ withExceptT to $ U.unify (from m1) (from m2)
  where
    to :: Functor f => f U.IntVar -> f RegionVar
    to = fmap RegionVar
    from :: Functor f => f RegionVar -> f U.IntVar
    from = fmap unRegionVar

instance Functor RegionTerm where
  fmap = fmapDefault

instance Foldable RegionTerm where
  foldMap = foldMapDefault

instance Traversable RegionTerm where
  traverse _f (ConstRegionTerm r) = pure (ConstRegionTerm r)
  
class Monad m => RegionUnification v m | m -> v where
  newRegion :: m v
  sameRegion :: v -> v -> m ()

newtype UnifyRegT m a = UnifyRegT { unUnifyRegT :: U.IntBindingT RegionTerm m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => RegionUnification RegionVar (UnifyRegT m) where
  newRegion = UnifyRegT (fmap RegionVar U.freeVar)
  sameRegion v1 v2 = do
    e <- unify (regionUnifyVar v1) (regionUnifyVar v2)
    case e of
      Right _uterm -> return ()
      Left _err -> error "occurs failure and mismatch failure cannot happen for region unification" -- FIXME: region mismatch can totally happen!

runUnifyRegT :: Monad m => UnifyRegT m a -> m a
runUnifyRegT = U.evalIntBindingT . unUnifyRegT

liftCatch :: Monad m => (forall b . m b -> (e -> m b) -> m b) -> UnifyRegT m a -> (e -> UnifyRegT m a) -> UnifyRegT m a
liftCatch catch (UnifyRegT comp) handler_ =
  UnifyRegT (ExtraU.liftCatch catch comp handler)
  where handler = unUnifyRegT . handler_
