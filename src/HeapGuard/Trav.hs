module HeapGuard.Trav where

import Data.Monoid (First(..))

-- data
import qualified Language.C.Data.Ident as Id

-- syntax
import qualified Language.C.Syntax.AST as Syn
import qualified Language.C.Syntax.Constants as Syn

-- semantics
import qualified Language.C.Analysis.TravMonad as AM
import qualified Language.C.Analysis.SemRep as A

-- utils
import qualified Language.C.Pretty as P

import Language.C.Analysis.Debug () -- P.Pretty instances 

import HeapGuard.Warning (hgWarn)

extraDecl :: AM.MonadTrav m => A.DeclEvent -> m ()
extraDecl e =
  case e of
    A.TagEvent (A.CompDef structTy@(A.CompType _ A.StructTag (_:_) _ _)) -> thinkStruct structTy
    _ -> return ()

thinkStruct :: AM.MonadTrav m => A.CompType -> m ()
thinkStruct (A.CompType suref A.StructTag (_:_) attrs ni) =
  case hasRegionAttr attrs of
    Just r ->
      AM.warn (hgWarn ("Found struct " ++  show (P.pretty suref) ++ " with at least one member and region " ++ show r) ni)
    Nothing -> return ()
thinkStruct _ = fail "can't happen"

newtype Region = Region Int
  deriving (Show, Eq)

hasRegionAttr :: A.Attributes -> Maybe Region
hasRegionAttr = getFirst . foldMap (First . from)
  where
    from (A.Attr ident [Syn.CConst (Syn.CIntConst r _)] _ni) | Id.identToString ident == "__region" = Just (Region $ fromInteger $ Syn.getCInteger r)
    from _ = Nothing
