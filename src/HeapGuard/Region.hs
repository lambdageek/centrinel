module HeapGuard.Region where

-- A literal region attribute __region (n) that requires a struct to be located
-- in region n.
newtype Region = Region Int
  deriving (Show, Eq)

-- A region scheme is either region polymorphic (can be in any region) or a single fixed region
data RegionScheme = PolyRS | FixedRS Region
  deriving (Show, Eq)
