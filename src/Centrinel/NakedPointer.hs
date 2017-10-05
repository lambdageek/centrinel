-- | Analyze function signatures and bodies to identify naked pointers into the
-- managed heap.
{-# language GeneralizedNewtypeDeriving #-}
module Centrinel.NakedPointer (analyze, AnalysisOpts(..)) where

import Control.Monad (forM_, (<=<))

import Data.Foldable (traverse_)
import qualified Data.Map as Map

import Centrinel.NakedPointerError

import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Data.Ident as C
import qualified Language.C.Data.Position as C

import qualified Language.C.Analysis.TravMonad as CM

import Centrinel.Control.Monad.Class.RegionResult

import Centrinel.NakedPointer.InDeclarations (nakedPtrCheckDecl)
import Centrinel.NakedPointer.InDefinitions (nakedPtrCheckDefn)


-- | Find uses of naked pointers to the managed region in function declarations
-- and in their definitions
analyze :: (RegionResultMonad m, CM.MonadTrav m) => AnalysisOpts -> Map.Map C.Ident C.IdentDecl -> m ()
analyze opts m = do
  let (fnDecls, (_globalDefns, _enumDefns, fnDefns)) = C.splitIdentDecls True m
  forM_ (optFilterDecls opts fnDecls) (recordNPE <=< nakedPtrCheckDecl)
  forM_ fnDefns (recordNPE <=< nakedPtrCheckDefn)
  where
    recordNPE :: (CM.MonadCError m) => Maybe NakedPointerError -> m ()
    recordNPE = traverse_ CM.recordError
      

-- | Options guiding the naked pointer analysis
data AnalysisOpts =
  AnalysisOpts
  { analysisOptFilterPath :: Maybe FilePath -- ^ @Just fp@ if we should only
                                            -- consider declarations from the
                                            -- given path, otherwise consider
                                            -- every declaration.
  }

-- | Given @optFilterDecls opts@, filter out declarations that are not in @fp@
-- where @analysisOptFilterPath opts == Just fp@
optFilterDecls :: AnalysisOpts -> Map.Map C.Ident a -> Map.Map C.Ident a
optFilterDecls opts =
  case analysisOptFilterPath opts of
    Just fp -> Map.filterWithKey $ \k _v ->
      let pos = C.posOf k
      in C.isSourcePos pos && fp == C.posFile pos
    Nothing -> id
