module Main where

import Data.Monoid 
import Options.Applicative
import qualified Centrinel.Main

main :: IO ()
main = execParser theParser >>= Centrinel.Main.main

theParser :: ParserInfo Centrinel.Main.CentrinelCmd
theParser =
  let
    commonOptions = Centrinel.Main.CentrinelOptions <$> useCCOption

    useCCOption = optional $ strOption (long "use-cc" <> metavar "CC" <> help "Use the given C compiler for preprocessing (Default: CC environment value or 'cc' if unset)")

    runProject :: Parser (Centrinel.Main.CentrinelOptions -> Centrinel.Main.CentrinelCmd)
    runProject = flip Centrinel.Main.RunProjectCentrinelCmd <$> strOption (long "project" <> help "Run the analysis on every file in the compilation database JSON_FILE" <> metavar "JSON_FILE")
    runOne :: Parser (Centrinel.Main.CentrinelOptions -> Centrinel.Main.CentrinelCmd)
    runOne = flip Centrinel.Main.RunOneCentrinelCmd <$> some (strArgument $ metavar "-- CFLAGS... CFILE" <> help "Run the analysis on a single C input file with the given flags")
    parser = helper <*> commonOptions <**> (runProject <|> runOne)
  in info parser (fullDesc <> (progDesc "centrinel is a C static analyzer"))
