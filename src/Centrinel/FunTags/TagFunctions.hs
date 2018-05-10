-- | A semantic analysis that collects sets of tags applied to function declarations and definitions
{-# language GeneralizedNewtypeDeriving #-}
module Centrinel.FunTags.TagFunctions (inferDeclEvent, TagTrackingT, runEveryThing) where

import Control.Monad (void)
import Control.Monad.State.Strict (StateT (..), gets, modify)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans (MonadTrans (..))

import Data.Semigroup (Option(..))
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import qualified Data.Set as S
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)

-- data
import Language.C.Data.Ident (Ident, identToString{-, internalIdent-})
import Language.C.Data.Node (NodeInfo)

-- syntax
import qualified Language.C.Syntax.AST as Syn
import qualified Language.C.Syntax.Constants as Syn
-- Semantics
import Language.C.Analysis.SemRep as A

import qualified Control.Unification as Unif
import qualified Control.Unification.Types as Unif
import qualified Control.Unification.IntVar as Unif

-- | A single tag
newtype BareTag = BareTag String
  deriving (Eq, Ord, Show)

singleBareTag :: String -> S.Set BareTag
singleBareTag = S.singleton . BareTag

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
type TagTerm = Unif.UTerm TagPreTerm Unif.IntVar

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

class Monad m => TagUnification m where
  -- | Given two tag unification terms, try to unify them
  -- In general if the terms are S1 ∪ ρ1 and S2 ∪ ρ2 the
  -- unifier will be (S1∪S2)∪ρ3 with ρ1 := S2 ∪ ρ3 and ρ2 := S1 ∪ ρ3 where ρ3 is fresh.
  unifyTagTerms :: TagTerm -> TagTerm -> m ()

class Monad m => TagAssignment m where
  assignTag :: Ident -> m TagTerm


type TaggingFailure = Unif.UFailure TagPreTerm Unif.IntVar

newtype TaggingT m a = TaggingT { unTaggingT :: ExceptT TaggingFailure (Unif.IntBindingT TagPreTerm m) a }
  deriving (Functor, Applicative, Monad)

freeTaggingVar :: Monad m => TaggingT m Unif.IntVar
freeTaggingVar = TaggingT $ lift $ Unif.freeVar

instance Monad m => TagUnification (TaggingT m) where
  unifyTagTerms t1 t2 = void $ TaggingT $ Unif.unify t1 t2
instance MonadTrans TaggingT where
  lift m = TaggingT (lift $ lift m)
  
instance TagUnification m => TagUnification (StateT r m) where
  unifyTagTerms t1 t2 = lift (unifyTagTerms t1 t2)

newtype TagTrackingT m a = TagTrackingT { unTagTrackingT :: TaggingT (StateT (M.Map Ident Unif.IntVar) m) a }
  deriving (Functor, Applicative, Monad, TagUnification)

instance Monad m => TagAssignment (TagTrackingT m) where
  assignTag ident = TagTrackingT $ do
    m <- lift $ gets $ M.lookup ident
    v <- case m of
      Just v -> return v
      Nothing -> do
        v <- freeTaggingVar
        lift $ modify (M.insert ident v)
        return v
    return (Unif.UVar v)

runEveryThing :: TagTrackingT m a -> m ((Either TaggingFailure a, Unif.IntBindingState TagPreTerm), M.Map Ident Unif.IntVar)
runEveryThing c = runStateT (Unif.runIntBindingT $ runExceptT $ unTaggingT $ unTagTrackingT c) mempty

-- cleanup :: Monad m => TagTerm -> TagTrackingT m TagTerm
-- cleanup = TagTrackingT . TaggingT . Unif.applyBindings


-- example1 :: (TagAssignment m, TagUnification m) => m TagTerm
-- example1 = do
--   v <- assignTag (internalIdent "f")
--   unifyTagTerms v (presentTags $ S.fromList $ map BareTag $ ["foo", "bar"])
--   unifyTagTerms v (presentTags $ S.fromList $ map BareTag $ ["bar"])
--   return v

-- example2 :: (TagAssignment m, TagUnification m) => m TagTerm
-- example2 = do
--   v1 <- assignTag (internalIdent "f")
--   unifyTagTerms v1 (presentTags $ S.fromList $ map BareTag $ ["foo", "bar"])
--   v2 <- assignTag (internalIdent "g")
--   unifyTagTerms v2 (presentTags $ S.fromList $ map BareTag $ ["baz"])
--   unifyTagTerms v1 v2
--   return v1

inferDeclEvent :: (TagAssignment m, TagUnification m) => A.DeclEvent -> m ()
inferDeclEvent e =
  case e of
    A.DeclEvent identDecl -> inferIdentDecl identDecl
    A.TypeDefEvent td -> inferTypeDef td
    _ -> return ()

inferIdentDecl :: (TagAssignment m, TagUnification m) => A.IdentDecl -> m ()
inferIdentDecl i =
  case i of
    A.Declaration d -> inferDecl d
    A.FunctionDef fn -> inferFunDef fn
    _ -> return ()


inferDecl :: (TagAssignment m, TagUnification m) => A.Decl -> m ()
inferDecl d =
  case d of
    A.Decl v ni -> inferVarDecl v ni

inferVarDecl :: (TagAssignment m, TagUnification m) => A.VarDecl -> NodeInfo -> m  ()
inferVarDecl vd _ni =
  case vd of
    A.VarDecl v (A.DeclAttrs _funattrs _storage attrs) _ty -> do
      mt <- assignTagVarName v
      case mt of
        Nothing -> return ()
        Just t -> unifyWithAttrs t attrs -- TODO: record the NodeInfo

assignTagVarName :: (TagAssignment m, TagUnification m) => A.VarName -> m (Maybe TagTerm)
assignTagVarName A.NoName = return Nothing
assignTagVarName (A.VarName ident _) = Just <$> assignTag ident
      
inferFunDef :: (TagAssignment m, TagUnification m) => A.FunDef -> m ()
inferFunDef fd =
  case fd of
    A.FunDef vd _body ni -> inferVarDecl vd ni
    

inferTypeDef :: Monad m => A.TypeDef -> m ()
inferTypeDef _ = return () -- nothing

unifyWithAttrs :: (TagAssignment m, TagUnification m) => TagTerm -> A.Attributes -> m ()
unifyWithAttrs t1 attrs =
  case tagTermFromAttrs attrs of
    Option Nothing -> return ()
    Option (Just t2) -> unifyTagTerms t1 t2

tagTermFromAttrs :: A.Attributes -> Option TagTerm
tagTermFromAttrs = fmap presentTags . foldMap tagsFromAttr
      

tagsFromAttr :: A.Attr -> Option (S.Set BareTag)
tagsFromAttr (A.Attr ident [Syn.CConst (Syn.CStrConst (Syn.CString litStr _wide) _)] _ni) =
  -- TODO: might as well allow __tags("tag1", "tag2", ...)
  if identToString ident == "__tags"
  then Option $ Just $ singleBareTag litStr
  else Option Nothing
tagsFromAttr _ = Option Nothing

                                 

