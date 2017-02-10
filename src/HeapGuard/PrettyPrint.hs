module HeapGuard.PrettyPrint (
  Pretty (..)
  , prettyUsingInclude
  , module Text.PrettyPrint
  , parenPrec
  ) where

import Language.C.Pretty (Pretty (..), prettyUsingInclude)
import Text.PrettyPrint

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec prec prec2 t = if prec <= prec2 then t else parens t
