-- | Unification terms for tag collection
module Centrinel.FunTags.Unification.Term (TagUVar, TagPreTerm, TagTerm, presentTags, representTags) where

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import qualified Data.Set as S
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)

import qualified Control.Unification as Unif
import qualified Control.Unification.IntVar as Unif

import Centrinel.FunTags.Tag

-- | Terms in the unification for tag sets.
--
-- This is kind of an unsatisfying encoding of what we actually want (just
-- union together tags as we come across them).  Make it look like a
-- unification problem so we can outsource state management.
-- 
-- The terms can be a finite map from tags to terms, or the symbol Present.
--
-- We will only have a single level of maps (ie we map each tag either to a
-- unification variable or to Present), but we want to allow unification to succeed like this:
--
-- [A -> Present, B -> U1] =:= [A -> U2, C -> U3]
-- which will unify give the common term
-- [A -> Present, B -> U1, C -> U3] and unify U2 with Present
--
-- We should never construct [A -> [B -> ...]] or anything that requires us to unify
-- Present with a TagPreTerm.  (This is not checked)
data TagPreTerm a = TagPreTerm (M.Map BareTag a)
                  | Present
                  deriving (Show)
type TagUVar = Unif.IntVar
type TagTerm = Unif.UTerm TagPreTerm TagUVar

instance Functor TagPreTerm where
  fmap = fmapDefault

instance Foldable TagPreTerm where
  foldMap = foldMapDefault

instance Traversable TagPreTerm where
  traverse _ Present = pure Present
  traverse f (TagPreTerm m) = TagPreTerm <$> traverse f m

instance Unif.Unifiable TagPreTerm where
  zipMatch Present Present = Just Present
  zipMatch (TagPreTerm m1) (TagPreTerm m2) =
    let
      -- if a tag is missing in m1 or in m2, keep it, and use Left to signal to
      -- unification that the corresponding entry on the other side was like a
      -- fresh unification variable
      keep = Left <$> M.preserveMissing
      -- when a tag is in both m2 and m2, keep it and tell unification to unify
      -- what the left and right maps map it to (which had better be Present or
      -- a unification variable).
      combine = M.zipWithMatched (\_k l r -> Right (l,r))
    in Just $ TagPreTerm $ M.merge keep keep combine m1 m2
  zipMatch Present (TagPreTerm {}) =
    -- shouldn't happen unless we made a mistake in generating unification problems
    Nothing
  zipMatch (TagPreTerm {}) Present =
    -- shouldn't happen unless we made a mistake in generating unificaiton problems
    Nothing

-- | Given a set of tags T1...Tk, make the term
-- @[T1 -> Present, ..., Tk -> Present]@
presentTags :: S.Set BareTag -> TagTerm
presentTags = Unif.UTerm . TagPreTerm . M.fromSet (const $ Unif.UTerm Present)

-- | Given a (unified) 'TagTerm', get the set of tags (ie the keys of the
-- 'TagPreTerm').  If the unification term is an uninstantiated variable (which
-- means we haven't seen any concrete tags for this term), or 'Present' (which
-- shouldn't happen), return the empty set.
representTags :: TagTerm -> BareTagSet
representTags (Unif.UVar {}) = mempty
representTags (Unif.UTerm Present) = mempty
representTags (Unif.UTerm (TagPreTerm tagMap)) = M.keysSet tagMap
