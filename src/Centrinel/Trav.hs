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
  , evalHGTrav
  -- * Analysis composition
  , HGAnalysis
  , singleHGAnalysis
  , withHGAnalysis
  -- * Pointer region analysis
  , hoistPointerRegionAnalysis
  -- * Function tag tracking analysis
  , hoistTagTracking
  ) where


import Control.Applicative ((*>))
import Control.Monad.Trans.Class 
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Except (ExceptT(..))
import qualified Control.Monad.Trans.Reader as Reader

import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (foldMap)
import qualified Data.Semigroup

import Language.C.Data.Error (CError, fromError)

import Language.C.Analysis.SemRep (DeclEvent)
import qualified Language.C.Analysis.TravMonad as AM
import Language.C.Analysis.TravMonad.Instances ()

import qualified Centrinel.Region.Class as U
import Centrinel.FunTags.Class as TF 
import Centrinel.Types (CentrinelAnalysisError (..)
                       , CentrinelAnalysisErrors
                       , singleAnalysisError
                       , CentrinelFatalError(CentAbortedAnalysisError))

import Centrinel.PointerRegionAnalysis (PointerRegionAnalysisT, evalPointerRegionAnalysisT)
import Centrinel.FunTags.TagTracking (TagTrackingT, evalTagTrackingT)

-- | An 'HGAnalysis' can perform some stateful action based on the incoming 'DeclEvent'.
-- The analyses form a 'Monoid' that just executes them in sequence
newtype HGAnalysis s = HGAnalysis { execHGAnalysis :: DeclEvent -> HGTrav s () }

-- | Make a singleton analysis
singleHGAnalysis :: (DeclEvent -> HGTrav s ()) -> HGAnalysis s
singleHGAnalysis = HGAnalysis

instance Data.Semigroup.Semigroup (HGAnalysis s) where
  (HGAnalysis a1) <> (HGAnalysis a2) = HGAnalysis (a1 *> a2)

instance Monoid (HGAnalysis s) where
  mempty = HGAnalysis $ const $ return ()
  mappend = (Data.Semigroup.<>)

-- | Monad transformer that adds extensible analyses on top of 'HGTrav'.
--
-- Note that 'HGAnalysis' is mutually defined with 'HGTrav', so the monad stack
-- is actually fixed.
type TravT s = ReaderT (HGAnalysis s)

-- | The fixed monad stack for extensible semantic analyses of C programs.
-- It's built on top of the base monad 'AM.Trav' that we build up with
-- analysis-specific state, and then close up with 'TravT' which adds the
-- 'handleDecl' event handling callbacks that are used to consume the C
-- declarations.
newtype HGTrav s a = HGTrav { unHGTrav :: TravT s (TagTrackingT (PointerRegionAnalysisT (AM.Trav s))) a}
  deriving (Functor, Applicative, Monad
           , AM.MonadName, AM.MonadSymtab, AM.MonadCError
           , U.RegionUnification, U.RegionAssignment
           , TF.TagUnification, TF.TagAssignment)

instance AM.MonadTrav (HGTrav s) where
  handleDecl ev = do
    handler <- HGTrav Reader.ask
    execHGAnalysis handler ev

-- | Run a pointer region analysis computation in the C traversal monad
hoistPointerRegionAnalysis :: (forall m . AM.MonadCError m => PointerRegionAnalysisT m a) -> HGTrav s a
hoistPointerRegionAnalysis = HGTrav . lift . lift

-- | Run a function tag tracking analysis in the C traversal monad
hoistTagTracking :: (forall m . AM.MonadCError m => TagTrackingT m a) -> HGTrav s a
hoistTagTracking = HGTrav . lift 

withHGAnalysis :: HGAnalysis s -> HGTrav s a -> HGTrav s a
withHGAnalysis az =
  HGTrav . Reader.local (Data.Semigroup.<> az) . unHGTrav

evalHGTrav :: Monad m
           => HGTrav () a
           -> ExceptT CentrinelFatalError m CentrinelAnalysisErrors
evalHGTrav (HGTrav comp) =
  ExceptT $ return $ fixupErrors $ evalTrav $ evalPointerRegionAnalysisT $ evalTagTrackingT $ Reader.runReaderT comp mempty
  where
    -- refine normal 'CError' errors and warnings to 'CentrinelAnalysisError'
    -- and abnormal ones to 'CentrinelFatalError'
    fixupErrors :: Either [CError] [CError]
                -> Either CentrinelFatalError CentrinelAnalysisErrors
    fixupErrors = bimap (CentAbortedAnalysisError . foldMap centrinelAnalysisError) (foldMap centrinelAnalysisError)

    evalTrav :: AM.Trav () a -> Either [CError] [CError]
    evalTrav = fmap snd . AM.runTrav_


-- | Refine an existentially-packed 'CError' into one of the well-known Centrinel
-- analysis errors, or a purely C syntax or semantics error from the "language-c" package.
centrinelAnalysisError :: CError -> CentrinelAnalysisErrors
centrinelAnalysisError = singleAnalysisError . matchError
  where
    matchError = \case
      e | Just regError <- fromError e -> CARegionMismatchError regError
        | Just nakedPtrError <- fromError e -> CANakedPointerError nakedPtrError
        | otherwise -> CACError e

