-- | Process Clang's JSON Compilation Database for centrinel's purposes.
{-# language NamedFieldPuns, OverloadedStrings #-}
module Centrinel.Util.CompilationDatabase (parseCompilationDatabase
                                          , RunLikeCC (..)
                                          , makeStandaloneRunLikeCC
                                          , Invoke (..)
                                          , combineDuplicateRuns
                                          , divideRunLikeCC) where

import Data.Monoid (Monoid(..), (<>))
import Data.Text (Text)
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode')
import qualified Clang.CompilationDatabase as CDB

import Centrinel.System.RunLikeCC (RunLikeCC(..))

parseCompilationDatabase :: B.ByteString -> Either String [RunLikeCC Invoke]
parseCompilationDatabase = fmap (fmap commandObjectRunLikeCC) . eitherDecode'

commandObjectRunLikeCC :: CDB.CommandObject -> RunLikeCC Invoke
commandObjectRunLikeCC cmd =
  case cmd of
    CDB.CommandObject {CDB.command = Just command, CDB.file = file, CDB.directory = workingDirectory} ->
      RunLikeCC { file, workingDirectory, artifact = Invoke (tail $ T.words command) }
    CDB.CommandObject {CDB.arguments = Just args, CDB.file = file, CDB.directory = workingDirectory} ->
      RunLikeCC { file, workingDirectory, artifact = Invoke (tail args) }
    CDB.CommandObject {CDB.file = file, CDB.command = Nothing, CDB.arguments = Nothing} ->
      error $ "impossible: command object for " ++ show file ++ "with no command and no arguments"

-- | The list of arguments that were passed to the compiler.
-- Double quotes and backslashes are escaped with a backslash.
-- Note that the name of the compiler is /not/ part of 'invokeArguments'.
newtype Invoke = Invoke { invokeArguments :: [Text] }
  deriving (Show)

makeStandaloneRunLikeCC :: [String] -> RunLikeCC Invoke
makeStandaloneRunLikeCC = RunLikeCC mempty "." . Invoke . fmap T.pack

mergeRunLikeCC :: Monoid a => RunLikeCC a -> RunLikeCC a -> RunLikeCC a
mergeRunLikeCC i1 i2 = i1 { artifact = artifact i1 <> artifact i2 }


combineDuplicateRuns :: [RunLikeCC Invoke] -> [RunLikeCC [Invoke]]
combineDuplicateRuns = M.elems . M.fromListWith mergeRunLikeCC . map (\i -> (dirFilePair i, singletonize i))
  where
    dirFilePair :: RunLikeCC a -> (Text, Text)
    dirFilePair i = (workingDirectory i, file i)

    singletonize :: RunLikeCC a -> RunLikeCC [a]
    singletonize i = i { artifact = [artifact i] }

divideRunLikeCC :: RunLikeCC [a] -> [RunLikeCC a]
divideRunLikeCC i = map (\a -> i { artifact = a }) (artifact i)
{-# INLINEABLE divideRunLikeCC #-}
