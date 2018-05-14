-- | Centrinel C language traversal monad
--
-- The @HGTrav@ monad stack has:
-- 1. a collection of analysis hooks that are triggered by traversals of C ASTs,
-- 2. and a unification state of region unification constraints and a map from C types to region unification variables.
--
--
{-# language GeneralizedNewtypeDeriving, LambdaCase, ViewPatterns, StandaloneDeriving, RankNTypes #-}
module Centrinel.Trav (
  -- * Extensible analysis monad
  HGTrav
  , runHGTrav
  , evalHGTrav
  , HGAnalysis
  , withHGAnalysis
  -- * Pointer region analysis
  , PointerAnalysisT (..)
  , RegionIdentMap
  , frozenRegionUnificationState
  , hoistPointerAnalysis
  ) where


import Control.Monad.Trans.Class 
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Except (ExceptT (..))
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as State

import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (foldMap)
import qualified Data.Map.Lazy as Map

import Language.C.Data.Ident (SUERef)
import Language.C.Data.Error (CError, fromError)

import Language.C.Analysis.SemRep (DeclEvent)
import qualified Language.C.Analysis.TravMonad as AM
import Language.C.Analysis.TravMonad.Instances ()

import qualified Centrinel.Region.Ident as HGId
import Centrinel.Region.Region (RegionScheme)
import qualified Centrinel.Region.Unification as U
import qualified Centrinel.Region.Unification.Term as U
import Centrinel.Types (CentrinelAnalysisError (..)
                       , CentrinelAnalysisErrors
                       , singleAnalysisError)
import Centrinel.Warning (hgWarn)

type HGAnalysis s = DeclEvent -> HGTrav s ()

type RegionIdentMap = Map.Map HGId.RegionIdent U.RegionUnifyTerm

-- | Monad transformer that adds extensible analyses on top of 'HGTrav'
-- note that 'HGAnalysis' is mutually defined with 'HGTrav', so the monad stack is actually fixed.
type TravT s = ReaderT (HGAnalysis s)

-- | Monad transformer that adds pointer region analysis on top of any underlying monad.
newtype PointerAnalysisT m a = PointerAnalysisT { unPointerAnalysisT :: StateT RegionIdentMap (U.UnifyRegT m) a }
                             deriving (Functor, Applicative, Monad)

instance MonadTrans PointerAnalysisT where
  lift m = PointerAnalysisT (lift $ lift m)

-- | The fixed monad stack for extensible semantic analyses of C programs.
-- It's built on top of the base monad 'AM.Trav' that we build up with
-- analysis-specific state, and then close up with 'TravT' which adds the
-- 'handleDecl' event handling callbacks that are used to consume the C
-- declarations.
newtype HGTrav s a = HGTrav { unHGTrav :: TravT s (PointerAnalysisT (AM.Trav s)) a}
  deriving (Functor, Applicative, Monad)

deriving instance AM.MonadName m => AM.MonadName (PointerAnalysisT m)

deriving instance AM.MonadName (HGTrav s)

deriving instance AM.MonadSymtab m => AM.MonadSymtab (PointerAnalysisT m)

deriving instance AM.MonadSymtab (HGTrav s)

instance AM.MonadCError m => AM.MonadCError (PointerAnalysisT m) where
  throwTravError = lift . AM.throwTravError
  catchTravError (PointerAnalysisT c) handler = PointerAnalysisT (AM.catchTravError c (unPointerAnalysisT . handler))
  recordError = lift . AM.recordError
  getErrors = lift AM.getErrors

instance AM.MonadCError (HGTrav s) where
  throwTravError = HGTrav . AM.throwTravError
  catchTravError (HGTrav c) handler = HGTrav (AM.catchTravError c (unHGTrav . handler))
  recordError = HGTrav . AM.recordError
  getErrors = HGTrav $ AM.getErrors

deriving instance AM.MonadCError m => U.RegionUnification (PointerAnalysisT m)
  
deriving instance U.RegionUnification (HGTrav s)

deriving instance AM.MonadCError m => U.ApplyUnificationState (PointerAnalysisT m)

deriving instance U.ApplyUnificationState (HGTrav s)

(-:=) :: (AM.MonadCError m, U.RegionUnification m) => U.RegionVar -> Maybe U.RegionUnifyTerm -> m U.RegionUnifyTerm
v -:= Nothing = return (U.regionUnifyVar v)
v -:= Just r = do
  AM.catchTravError (U.sameRegion (U.regionUnifyVar v) r)
    (\_err -> do
        AM.recordError (hgWarn "failed to unify regions" Nothing) -- TODO: region info
        return (U.regionUnifyVar v))

getRegionIdent :: Monad m => HGId.RegionIdent -> PointerAnalysisT m (Maybe (U.RegionUnifyTerm))
getRegionIdent i = PointerAnalysisT $ State.gets (Map.lookup i)

putRegionIdent :: Monad m => HGId.RegionIdent -> U.RegionUnifyTerm -> PointerAnalysisT m ()
putRegionIdent i m = PointerAnalysisT $ State.modify' (Map.insert i m)

-- | Gets a mapping of the region identifiers that have been noted by
-- unification to their 'RegionScheme' as implied by the constraints available
-- at the time of the call.
frozenRegionUnificationState :: AM.MonadCError m => PointerAnalysisT m (Map.Map SUERef RegionScheme)
frozenRegionUnificationState = do
  sueRegions <- PointerAnalysisT $ State.gets munge
  traverse (fmap U.extractRegionScheme . U.applyUnificationState) sueRegions
  where
    munge :: Map.Map HGId.RegionIdent U.RegionUnifyTerm -> Map.Map SUERef U.RegionUnifyTerm
    munge = Map.mapKeysMonotonic onlySUERef . Map.filterWithKey (\k -> const (isStructTag k))
    onlySUERef :: HGId.RegionIdent -> SUERef
    onlySUERef (HGId.StructTagId sue) = sue
    onlySUERef (HGId.TypedefId {}) = error "unexpected TypedefId in onlySUERef"
    isStructTag :: HGId.RegionIdent -> Bool
    isStructTag (HGId.StructTagId {}) = True
    isStructTag (HGId.TypedefId {}) = False

instance AM.MonadCError m => HGId.RegionAssignment HGId.RegionIdent U.RegionVar (PointerAnalysisT m) where
  assignRegion i = do
    v <- U.newRegion
    r <- getRegionIdent i
    r' <- v -:= r
    putRegionIdent i r'
    return v

deriving instance HGId.RegionAssignment HGId.RegionIdent U.RegionVar (HGTrav s)

instance AM.MonadTrav (HGTrav s) where
  handleDecl ev = do
    handler <- HGTrav Reader.ask
    handler ev

hoistPointerAnalysis :: (forall m . AM.MonadCError m => PointerAnalysisT m a) -> HGTrav s a
hoistPointerAnalysis comp = HGTrav $ lift comp

withHGAnalysis :: HGAnalysis s -> HGTrav s a -> HGTrav s a
withHGAnalysis az =
  HGTrav . Reader.local (addAnalysis az) . unHGTrav
  where
    -- new analysis runs last
    addAnalysis m = (>> m)

runHGTrav :: Monad m
          => HGTrav () a
          -> ExceptT CentrinelAnalysisErrors m ((a, RegionIdentMap), CentrinelAnalysisErrors)
runHGTrav = helper State.runStateT

evalHGTrav :: Monad m
           => HGTrav () a
          -> ExceptT CentrinelAnalysisErrors m (a, CentrinelAnalysisErrors)
evalHGTrav = helper State.evalStateT

helper :: Monad m
       => (StateT RegionIdentMap (U.UnifyRegT (AM.Trav t)) a
           -> Map.Map k b
           -> U.UnifyRegT (AM.Trav ()) r)
       -> HGTrav t a
       -> ExceptT CentrinelAnalysisErrors m (r, CentrinelAnalysisErrors)
helper destructState (HGTrav comp) =
  ExceptT $ return . fixupErrors $ AM.runTrav_ $ U.runUnifyRegT (destructState (unPointerAnalysisT (Reader.runReaderT comp az)) Map.empty)
  where
    az = const (return ())
    -- change errors and warnings from one form to another
    fixupFatalNonFatal :: (e1 -> e2) -> (w1 -> w2)
                       -> Either e1 (a, w1) -> Either e2 (a, w2)
    fixupFatalNonFatal fErr fWarn = bimap fErr (fmap fWarn)
    -- refine 'CError' errors and warnings to 'CentrinelAnalysisError'
    fixupErrors :: Either [CError] (a, [CError])
                -> Either CentrinelAnalysisErrors (a, CentrinelAnalysisErrors)
    fixupErrors = fixupFatalNonFatal (foldMap centrinelAnalysisError) (foldMap centrinelAnalysisError)


-- | Refine an existentially-packed 'CError' into one of the well-known Centrinel
-- analysis errors, or a purely C syntax or semantics error from the "language-c" package.
centrinelAnalysisError :: CError -> CentrinelAnalysisErrors
centrinelAnalysisError = singleAnalysisError . matchError
  where
    matchError = \case
      e | Just regError <- fromError e -> CARegionMismatchError regError
        | Just nakedPtrError <- fromError e -> CANakedPointerError nakedPtrError
        | otherwise -> CACError e
