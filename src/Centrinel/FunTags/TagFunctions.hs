-- | A semantic analysis that collects sets of tags applied to function declarations and definitions
{-# language GeneralizedNewtypeDeriving #-}
module Centrinel.FunTags.TagFunctions (inferDeclEvent, TagTrackingT, runTagTrackingT, TagInferenceResult) where

import Data.Semigroup (Option(..))
import qualified Data.Set as S

-- data
import Language.C.Data.Ident (identToString)
import Language.C.Data.Node (NodeInfo)

-- syntax
import qualified Language.C.Syntax.AST as Syn
import qualified Language.C.Syntax.Constants as Syn
-- Semantics
import Language.C.Analysis.SemRep as A

import Centrinel.FunTags.Tag
import Centrinel.FunTags.Class
import Centrinel.FunTags.Unification.Term
import Centrinel.FunTags.TagTracking

-- | A "language-c" semantic analysis event handler that collects a mapping
-- from C identifier declarations and definition to tags.  Tags are attached
-- using the @\_\_tags@ custom attribute. For example:
-- @\_\_attribute\_\_((\_\_tags("whatever")))@
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

                                 

