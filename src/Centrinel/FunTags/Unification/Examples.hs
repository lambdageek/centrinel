module Centrinel.FunTags.Unification.Examples (example1, example2, runTagTrackingT) where

import qualified Data.Set as S

import Language.C.Data.Ident (internalIdent)

import Centrinel.FunTags.Class
import Centrinel.FunTags.TagFunctions
import Centrinel.FunTags.Tag
import Centrinel.FunTags.Unification.Term

-- > runTagTrackingT example1
-- Right ((),fromList [(Ident "f" 102 (OnlyPos <internal> (<no file>,-1)),fromList [BareTag "bar",BareTag "foo"])])
example1 :: (TagAssignment m, TagUnification m) => m ()
example1 = do
  v <- assignTag (internalIdent "f")
  unifyTagTerms v (presentTags $ S.fromList $ map BareTag $ ["foo", "bar"])
  unifyTagTerms v (presentTags $ S.fromList $ map BareTag $ ["bar"])
  return ()

example2 :: (TagAssignment m, TagUnification m) => m ()
example2 = do
  v1 <- assignTag (internalIdent "f")
  unifyTagTerms v1 (presentTags $ S.fromList $ map BareTag $ ["foo", "bar"])
  v2 <- assignTag (internalIdent "g")
  unifyTagTerms v2 (presentTags $ S.fromList $ map BareTag $ ["baz"])
  unifyTagTerms v1 v2
  return ()
