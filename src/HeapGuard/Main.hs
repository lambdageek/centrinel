module HeapGuard.Main where

import Control.Monad (void)
import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName)

import HeapGuard (inp, think')

main :: IO ()
main = do
  fp <- processArgs
  ast <- do
    x <- inp fp
    case x of
      Left err -> do
        print err
        exitFailure
      Right ast -> return ast
  void $ think' ast
      
processArgs :: IO FilePath
processArgs = do
  a <- getArgs
  case a of
    [fp] -> return fp
    _ -> do
      usage
      exitFailure

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " FILENAME"
