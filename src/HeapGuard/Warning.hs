module HeapGuard.Warning where

import Language.C.Data.Error as Err
import Language.C.Data.Node (NodeInfo, undefNode)

newtype Warning = Warning Err.ErrorInfo

instance Err.Error Warning where
  errorInfo (Warning e) = e
instance Show Warning where
  showsPrec p (Warning e) = showsPrec p e

hgWarn :: String -> Maybe NodeInfo -> Warning
hgWarn msg ni = Warning $ Err.mkErrorInfo Err.LevelWarn msg (maybe undefNode id ni)

