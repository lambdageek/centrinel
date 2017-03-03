module Centrinel.PrettyPrint (
  -- * Pretty printing typeclass
  Pretty (..)
  -- * Specialized pretty printing for C ASTs
  , prettyUsingInclude
  -- * Re-export Text.PrettyPrint combinators
  , module Text.PrettyPrint
  -- * Additional pretty printing utilities
  , parenPrec
  , prettyPos
  ) where

import qualified Language.C.Data.Position as C

import Language.C.Pretty (Pretty (..), prettyUsingInclude)
import Text.PrettyPrint

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec prec prec2 t = if prec <= prec2 then t else parens t

prettyPos :: C.Pos p => p -> Doc
prettyPos = prettyPosition . C.posOf
  where
    prettyPosition :: C.Position -> Doc
    prettyPosition p | C.isSourcePos p = text (C.posFile p) <> text ":" <> int (C.posRow p) <> text ":" <+>
                                         parens (text "column" <+> int (C.posColumn p))
                     | otherwise = text (show p) <+> text "::"
