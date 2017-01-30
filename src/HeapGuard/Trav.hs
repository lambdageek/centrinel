{-# language GeneralizedNewtypeDeriving #-}
module HeapGuard.Trav (
  HGTrav
  , runHGTrav
  , HGAnalysis
  ) where

import Control.Monad.Trans.Class 
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader

import Language.C.Data.Error (CError)

import Language.C.Analysis.SemRep (DeclEvent)
import qualified Language.C.Analysis.TravMonad as AM

import qualified HeapGuard.RegionUnification as U

type HGAnalysis s = DeclEvent -> HGTrav s ()

newtype HGTrav s a = HGTrav { unHGTrav :: ReaderT (HGAnalysis s) (U.UnifyRegT (AM.Trav s)) a}
  deriving (Functor, Applicative, Monad)

instance AM.MonadName (HGTrav s) where
  genName = HGTrav $ lift $ lift AM.genName

instance AM.MonadSymtab (HGTrav s) where
  getDefTable = HGTrav $ lift $ lift AM.getDefTable
  withDefTable = HGTrav . lift . lift . AM.withDefTable

instance AM.MonadCError (HGTrav s) where
  throwTravError = HGTrav . lift . lift . AM.throwTravError
  catchTravError (HGTrav c) handler = HGTrav (Reader.liftCatch (U.liftCatch AM.catchTravError) c (unHGTrav . handler))
  recordError = HGTrav . lift . lift . AM.recordError
  getErrors = HGTrav $ lift $ lift AM.getErrors

instance U.RegionUnification U.RegionVar (HGTrav s) where
  newRegion = HGTrav $ lift U.newRegion
  sameRegion v1 v2 = HGTrav $ lift $ U.sameRegion v1 v2

instance AM.MonadTrav (HGTrav s) where
  handleDecl ev = do
    handler <- HGTrav Reader.ask
    handler ev

runHGTrav :: HGAnalysis () -> HGTrav () a -> Either [CError] (a, [CError])
runHGTrav az (HGTrav comp) = AM.runTrav_ (U.runUnifyRegT (Reader.runReaderT comp az))

