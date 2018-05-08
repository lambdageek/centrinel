-- | Run the "language-c" parser on the given input with the given flags.
module Centrinel.System.ParseCFile (parseCFile) where

import Control.Monad.Except (ExceptT (..), withExceptT)

import Language.C.Data.Position (initPos)
import Language.C.Parser (parseC)
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.System.GCC (GCC)
import qualified Language.C.System.Preprocess as CPP

import Centrinel.Types (CentrinelFatalError(..))

parseCFile :: CPP.Preprocessor cpp => cpp -> CPP.CppArgs -> ExceptT CentrinelFatalError IO CTranslUnit
{-# specialize parseCFile :: GCC -> CPP.CppArgs -> ExceptT CentrinelFatalError IO CTranslUnit #-}
parseCFile cpp cppArgs = do
    inputStream <- withExceptT CentCPPError $ ExceptT $ CPP.runPreprocessor cpp cppArgs
    withExceptT CentParseError $ ExceptT $ return $ parseC inputStream (initPos $ CPP.inputFile cppArgs)

