-- | Process Clang's JSON Compilation Database for centrinel's purposes.
{-# language NamedFieldPuns, OverloadedStrings #-}
module Centrinel.Util.CompilationDatabase (parseCompilationDatabase
                                          , RunLikeCC (..)
                                          , Invoke (..)) where

import Data.Text (Text)
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


