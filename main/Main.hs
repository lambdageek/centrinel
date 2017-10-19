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

    runProject :: Parser Centrinel.Main.CentrinelCmd
    runProject = Centrinel.Main.RunProjectCentrinelCmd <$> commonOptions  <*> strOption (long "project" <> help "Run the analysis on every file in the compilation database JSON_FILE" <> metavar "JSON_FILE")
    runOne :: Parser Centrinel.Main.CentrinelCmd
    runOne = Centrinel.Main.RunOneCentrinelCmd <$> commonOptions <*> some (strArgument $ metavar "-- CFLAGS... CFILE" <> help "Run the analysis on a single C input file with the given flags")
    parser = helper <*> (runProject <|> runOne)
  in info parser (fullDesc <> (progDesc "centrinel is a C static analyzer"))
