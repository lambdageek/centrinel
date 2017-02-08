module HeapGuard.Region where

import qualified HeapGuard.PrettyPrint as PP
import HeapGuard.PrettyPrint ((<+>))

-- A literal region attribute __region (n) that requires a struct to be located
-- in region n.
newtype Region = Region Int
  deriving (Show, Eq)

-- A region scheme is either region polymorphic (can be in any region) or a single fixed region
data RegionScheme = PolyRS | FixedRS Region
  deriving (Show, Eq)

instance PP.Pretty Region where
  prettyPrec p (Region r) = PP.parenPrec p 10 $ PP.text "Region" <+> PP.int r

instance PP.Pretty RegionScheme where
  prettyPrec p rs = case rs of
    PolyRS -> PP.text "PolyRS"
    FixedRS r -> PP.parenPrec p 10 $ PP.text "FixedRS" <+> PP.prettyPrec 11 r
