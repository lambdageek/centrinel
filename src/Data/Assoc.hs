{-# language GeneralizedNewtypeDeriving #-}
-- | Association list data
-- This is is just a thin wrapper around 'Map.Map' with a 'PP.Pretty' instance
module Data.Assoc where

import qualified Data.Map as Map

import qualified HeapGuard.PrettyPrint as PP
import qualified Language.C.Analysis.Debug as DP  

newtype Assoc k a = Assoc { getAssocMap :: Map.Map k a }
  deriving (Show, Eq, Ord, Functor, Foldable, Monoid)

instance Traversable (Assoc k) where
  traverse f = fmap Assoc . traverse f . getAssocMap

instance (PP.Pretty k, PP.Pretty a) => PP.Pretty (Assoc k a) where
  pretty = DP.prettyAssocs "Assoc" . Map.toList . getAssocMap 
