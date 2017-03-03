-- | Find the Cabal-managed data files
module Centrinel.Util.Datafiles where

import Paths_centrinel (getDataFileName)

data Datafiles = Datafiles { datafileCentrinelHeader :: !FilePath }

getDatafiles :: IO Datafiles
getDatafiles = do
  header <- getDataFileName "include/centrinel.h"
  return $ Datafiles header
