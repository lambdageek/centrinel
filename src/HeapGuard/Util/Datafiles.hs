-- | Find the Cabal-managed data files
module HeapGuard.Util.Datafiles where

import Paths_use_c (getDataFileName)

data Datafiles = Datafiles { datafileHeapguardHeader :: !FilePath }

getDatafiles :: IO Datafiles
getDatafiles = do
  header <- getDataFileName "include/heapguard.h"
  return $ Datafiles header
