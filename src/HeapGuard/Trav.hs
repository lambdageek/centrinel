-- | HeapGuard C language traversal monad
--
-- The @HGTrav@ monad stack has:
-- 1. a collection of analysis hooks that are triggered by traversals of C ASTs,
-- 2. and a unification state of region unification constraints and a map from C types to region unification variables.
--
--
{-# language GeneralizedNewtypeDeriving #-}
module HeapGuard.Trav (
  HGTrav
  , runHGTrav
  , evalHGTrav
  , HGAnalysis
  , withHGAnalysis
  , RegionIdentMap
  ) where

import Control.Monad.Trans.Class 
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as State

import qualified Data.Map as Map

import Language.C.Data.Error (CError)

import Language.C.Analysis.SemRep (DeclEvent)
import qualified Language.C.Analysis.TravMonad as AM

import qualified HeapGuard.RegionIdent as HGId
import qualified HeapGuard.RegionUnification as U
import qualified HeapGuard.RegionUnification.Term as U
import HeapGuard.Warning (hgWarn)

type HGAnalysis s = DeclEvent -> HGTrav s ()

type RegionIdentMap = Map.Map HGId.RegionIdent U.RegionUnifyTerm

newtype HGTrav s a = HGTrav { unHGTrav :: ReaderT (HGAnalysis s) (StateT RegionIdentMap (U.UnifyRegT (AM.Trav s))) a}
  deriving (Functor, Applicative, Monad)

instance AM.MonadName (HGTrav s) where
  genName = HGTrav $ lift $ lift $ lift $ AM.genName

instance AM.MonadSymtab (HGTrav s) where
  getDefTable = HGTrav $ lift $ lift $ lift AM.getDefTable
  withDefTable = HGTrav . lift . lift . lift . AM.withDefTable

instance AM.MonadCError (HGTrav s) where
  throwTravError = HGTrav . lift . lift . lift . AM.throwTravError
  catchTravError (HGTrav c) handler = HGTrav (Reader.liftCatch (State.liftCatch (U.liftCatch AM.catchTravError)) c (unHGTrav . handler))
  recordError = HGTrav . lift . lift . lift . AM.recordError
  getErrors = HGTrav $ lift $ lift $ lift AM.getErrors

instance U.RegionUnification U.RegionVar (HGTrav s) where
  newRegion = HGTrav $ lift $ lift U.newRegion
  sameRegion v = HGTrav . lift . lift . U.sameRegion v
  constantRegion v  = HGTrav . lift . lift . U.constantRegion v
  regionAddLocation v = HGTrav . lift . lift . U.regionAddLocation v

instance U.ApplyUnificationState (HGTrav s) where
  applyUnificationState = HGTrav . lift . lift . U.applyUnificationState

(-:=) :: U.RegionVar -> Maybe U.RegionUnifyTerm -> HGTrav s U.RegionUnifyTerm
v -:= Nothing = return (U.regionUnifyVar v)
v -:= Just r = do
  m <- HGTrav $ lift $ lift $ U.unify (U.regionUnifyVar v) r
  case m of
    Right r' -> return r'
    Left _err -> do
      AM.recordError (hgWarn "failed to unify regions" Nothing) -- TODO: region info
      return (U.regionUnifyVar v)

getRegionIdent :: HGId.RegionIdent -> HGTrav s (Maybe (U.RegionUnifyTerm))
getRegionIdent i = HGTrav $ lift $ State.gets (Map.lookup i)

putRegionIdent :: HGId.RegionIdent -> U.RegionUnifyTerm -> HGTrav s ()
putRegionIdent i m = HGTrav $ lift $ State.modify' (Map.insert i m)

instance HGId.RegionAssignment HGId.RegionIdent U.RegionVar (HGTrav s) where
  assignRegion i = do
    v <- U.newRegion
    r <- getRegionIdent i
    r' <- v -:= r
    putRegionIdent i r'
    return v

instance AM.MonadTrav (HGTrav s) where
  handleDecl ev = do
    handler <- HGTrav Reader.ask
    handler ev

withHGAnalysis :: HGAnalysis s -> HGTrav s a -> HGTrav s a
withHGAnalysis az =
  HGTrav . Reader.local (addAnalysis az) . unHGTrav
  where
    -- new analysis runs last
    addAnalysis m = (>> m)

runHGTrav :: HGTrav () a
          -> Either [CError] ((a, RegionIdentMap), [CError])
-- runHGTrav :: HGAnalysis () -> HGTrav () a -> Either [CError] (a, [CError])
runHGTrav (HGTrav comp) = AM.runTrav_ (U.runUnifyRegT (State.runStateT (Reader.runReaderT comp az) Map.empty))
  where
    az = const (return ())

evalHGTrav :: HGTrav () a
          -> Either [CError] (a, [CError])
evalHGTrav (HGTrav comp) = AM.runTrav_ (U.runUnifyRegT (State.evalStateT (Reader.runReaderT comp az) Map.empty))
  where
    az = const (return ())

