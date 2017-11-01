-- | Find @__attribute__((__suppress(b)))@ in a givne attribute list
-- 
module Centrinel.NakedPointer.FindSuppressAttribute where

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Alt(..))

import qualified Language.C.Data.Ident as C
import qualified Language.C.Syntax.AST as CStx
import qualified Language.C.Syntax.Constants as CStx
import qualified Language.C.Analysis.SemRep as CSem

-- | Returns @pure b@ iff the given attribute list includes @__suppress(c)@.
-- If @c@ is a literal constant 0, @b@ is 'False', if it is a literal constant
-- 1, @b@ is 'True'. If the attribute is not found, returns 'empty'
findSuppressAttribute :: Alternative f => C.Ident -> [CStx.CExpr] -> f Bool
findSuppressAttribute ident attrArgs = do
  case attrArgs of
    [CStx.CConst (CStx.CIntConst i _constNi)]
      | isSuppress ident ->
          case CStx.getCInteger i of
            0 -> pure False
            1 -> pure True
            _ -> empty
    _ -> empty

-- | Look in the given list of attributes for a @__suppress(c)@ pragma.
-- Returns @pure (c == 1)@ where c is the argument value from the first
-- attribute with a literal constant 0 or 1 as the argument (That is: if it
-- finds several @_suppress(x)@ attributes that don't have a 0 or 1 literal for
-- @x@, it'll skip them and keep going).
findSuppressInCAttrList :: (Foldable f, Alternative g) => f CStx.CAttr -> g Bool
findSuppressInCAttrList = getAlt . foldMap (Alt . findSuppressCAttr)
  where
    findSuppressCAttr (CStx.CAttr ident attrArgs _ni) = findSuppressAttribute ident attrArgs
{-# Specialize findSuppressInCAttrList :: [CStx.CAttr] -> Maybe Bool #-}

-- | Same as 'findSuppressInCAttrList' but for 'CSem.Attr' as the attribute type.
findSuppressInSemAttrList :: (Foldable f, Alternative g) => f CSem.Attr -> g Bool
findSuppressInSemAttrList = getAlt . foldMap (Alt . findSuppressSemAttr)
  where
    findSuppressSemAttr (CSem.Attr ident attrArgs _ni) = findSuppressAttribute ident attrArgs
{-# Specialize findSuppressInSemAttrList :: [CSem.Attr] -> Maybe Bool #-}

isSuppress:: C.Ident -> Bool
isSuppress ident = C.identToString ident == "__suppress"
