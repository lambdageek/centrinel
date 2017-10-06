-- | Utilities for "Centrinel.NakedPointer.InDeclarations" and "Centrinel.NakedPointer.InDefinitions"
module Centrinel.NakedPointer.Utils (
  tellWhenManaged
  ) where

import Control.Monad.Reader.Class
import Control.Monad.Writer.Class

import ZeptoLens

import qualified Language.C.Analysis.SemRep as C

import Centrinel.Region.Region (isManagedRegion)

import Centrinel.Control.Monad.Class.RegionResult

import Centrinel.NakedPointer.Env
import Centrinel.NakedPointer.PointerRegionScheme 
import Centrinel.NakedPointerError (NPEVictim(..), NPEVictims)

-- | Tell when the given type is a naked pointer to the managed heap
tellWhenManaged :: (MonadReader AnalysisEnv m, MonadWriter NPEVictims m, RegionResultMonad m)
                => C.Type -> m ()
tellWhenManaged ty = do
  x <- pointerRegionScheme ty
  case x of
    Just rs | isManagedRegion rs -> tellNPE ty
    _ -> return ()

tellNPE :: (MonadReader AnalysisEnv m, MonadWriter NPEVictims m) => C.Type -> m ()
tellNPE ty = view analysisPosn >>= \npe -> tell [NPEVictim ty npe]

