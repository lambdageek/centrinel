{-# language GeneralizedNewtypeDeriving #-}
module HeapGuard.RegionUnification.Loc where

import Data.Monoid (Monoid(..))

import qualified Data.Set as Set

import qualified Language.C.Data.Node as C

newtype LocTerm = LocTerm {locTermNodes :: Set.Set C.NodeInfo}
  deriving (Monoid, Show)

-- Not really the best. For now the heuristic is whatever comes later in the file is probably most relevant.
--
-- TODO: richer LocTerm structure that knows how unification constraints arise.
bestNodeLocTerm :: LocTerm -> (C.NodeInfo, [C.NodeInfo])
bestNodeLocTerm (LocTerm s) =
  case Set.maxView s of
    Nothing -> (C.undefNode, [])
    Just (n, s') -> (n, Set.toList s')

locSingleton :: C.NodeInfo -> LocTerm
locSingleton = LocTerm . Set.singleton
