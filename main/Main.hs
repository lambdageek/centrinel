module Main where

import Data.Monoid 
import Options.Applicative
import qualified Centrinel.Main
import qualified Centrinel.Report

main :: IO ()
main = execParser theParser >>= Centrinel.Main.main

theParser :: ParserInfo Centrinel.Main.CentrinelCmd
theParser =
  let
    commonOptions :: Parser Centrinel.Main.CentrinelOptions
    commonOptions = Centrinel.Main.CentrinelOptions <$> useCCOption <*> outputMethodOptions

    useCCOption :: Parser (Maybe FilePath)
    useCCOption = optional $ strOption (long "use-cc" <> metavar "CC" <> help "Use the given C compiler for preprocessing (Default: CC environment value or 'cc' if unset)")

    runProject :: Parser (Centrinel.Main.CentrinelOptions -> Centrinel.Main.CentrinelCmd)
    runProject = flip Centrinel.Main.RunProjectCentrinelCmd <$> strOption (long "project" <> help "Run the analysis on every file in the compilation database JSON_FILE" <> metavar "JSON_FILE")
    runOne :: Parser (Centrinel.Main.CentrinelOptions -> Centrinel.Main.CentrinelCmd)
    runOne = flip Centrinel.Main.RunOneCentrinelCmd <$> some (strArgument $ metavar "-- CFLAGS... CFILE" <> help "Run the analysis on a single C input file with the given flags")
    parser = helper <*> commonOptions <**> (runProject <|> runOne)
  in info parser (fullDesc <> (progDesc "centrinel is a C static analyzer"))


outputMethodOptions :: Parser Centrinel.Report.OutputMethod
outputMethodOptions = Centrinel.Report.OutputMethod <$> outputDestinationOption <*> outputFormatOption
  where
    outputDestinationOption :: Parser Centrinel.Report.OutputDestination
    outputDestinationOption =
      (Centrinel.Report.FilePathOutputDestination <$> strOption (long "output" <> help "destination file for the output"
                                                               <> metavar "FILE" ))
      <|> pure Centrinel.Report.StdOutOutputDestination 

    outputFormatOption :: Parser Centrinel.Report.OutputFormat
    outputFormatOption =
      (option outputFormatReader (long "format" <> help "output format: text or json (default is text)"
                                  <> metavar "FMT"))
      <|> pure Centrinel.Report.PlainTextOutputFormat

    outputFormatReader :: ReadM Centrinel.Report.OutputFormat
    outputFormatReader = maybeReader $ \s ->
      case s of
        "text" -> pure Centrinel.Report.PlainTextOutputFormat
        "json" -> pure Centrinel.Report.JSONOutputFormat
        _ -> empty
