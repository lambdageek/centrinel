-- | A semantic analysis that collects sets of tags applied to function declarations and definitions
{-# language GeneralizedNewtypeDeriving #-}
module Centrinel.FunTags.TagFunctions (inferDeclEvent, TagTrackingT, runEveryThing) where

import Control.Monad (void)
import Control.Monad.State.Strict (StateT (..), gets, modify)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans (MonadTrans (..))

import Data.Semigroup (Option(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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

import Centrinel.FunTags.Tag
import Centrinel.FunTags.Unification.Term

class Monad m => TagUnification m where
  -- | Given two tag unification terms, try to unify them
  -- In general if the terms are S1 ∪ ρ1 and S2 ∪ ρ2 the
  -- unifier will be (S1∪S2)∪ρ3 with ρ1 := S2 ∪ ρ3 and ρ2 := S1 ∪ ρ3 where ρ3 is fresh.
  unifyTagTerms :: TagTerm -> TagTerm -> m ()

class Monad m => TagAssignment m where
  assignTag :: Ident -> m TagTerm


type TaggingFailure = Unif.UFailure TagPreTerm TagUVar

newtype TaggingT m a = TaggingT { unTaggingT :: ExceptT TaggingFailure (Unif.IntBindingT TagPreTerm m) a }
  deriving (Functor, Applicative, Monad)

freeTaggingVar :: Monad m => TaggingT m TagUVar
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

                                 

