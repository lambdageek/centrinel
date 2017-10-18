module Main where

import Data.Monoid 
import Options.Applicative
import qualified Centrinel.Main

main :: IO ()
main = execParser theParser >>= Centrinel.Main.main

theParser :: ParserInfo Centrinel.Main.CentrinelCmd
theParser =
  let
    commonOptions = pure Centrinel.Main.CentrinelOptions

    runOne :: Parser Centrinel.Main.CentrinelCmd
    runOne = Centrinel.Main.RunOneCentrinelCmd <$> commonOptions <*> some (strArgument $ metavar "-- CFLAGS... CFILE" <> help "Run the analysis on a single C input file with the given flags")
    parser = helper <*> runOne
  in info parser (fullDesc <> (progDesc "centrinel is a C static analyzer"))
