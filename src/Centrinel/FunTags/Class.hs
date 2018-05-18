module Centrinel.FunTags.Class where

import Control.Monad.Reader as Rd
import Control.Monad.State.Strict as StS
import Control.Monad.Trans (MonadTrans (..))
import Language.C.Data.Ident (Ident)

import Centrinel.FunTags.Unification.Term (TagTerm)

-- | Class of monads that can assign/lookup tags for C identifiers
class Monad m => TagAssignment m where
  -- | Get the tag unification term for a given identifier.  If we haven't seen
  -- this identifier before, makes a fresh unification variable for it and
  -- returns that.
  assignTag :: Ident -> m TagTerm

-- | Class of monads that can perform unification of tag terms
class Monad m => TagUnification m where
  -- | Given two tag unification terms, try to unify them
  -- In general if the terms are S1 ∪ ρ1 and S2 ∪ ρ2 the
  -- unifier will be (S1∪S2)∪ρ3 with ρ1 := S2 ∪ ρ3 and ρ2 := S1 ∪ ρ3 where ρ3 is fresh.
  unifyTagTerms :: TagTerm -> TagTerm -> m ()

instance TagUnification m => TagUnification (StS.StateT s m) where
  unifyTagTerms = \t1 t2 -> lift (unifyTagTerms t1 t2)

instance TagUnification m => TagUnification (Rd.ReaderT r m) where
  unifyTagTerms = \t1 t2 -> lift (unifyTagTerms t1 t2)

instance TagAssignment m => TagAssignment (Rd.ReaderT r m) where
  assignTag = lift . assignTag
