{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse
    ( getInstructions
    , Instructions(..)
    , Settings(..)
    ) where

import Import

import System.Environment (getArgs)
import System.Exit

import Options.Applicative

import Smos.Report.Config
import Smos.Report.OptParse.Types

getInstructions :: SmosReportConfig -> IO Instructions
getInstructions conf = do
    args <- getArguments
    config <- getConfiguration args
    combineToInstructions conf args config

combineToInstructions ::
       SmosReportConfig -> Arguments -> Configuration -> IO Instructions
combineToInstructions SmosReportConfig {..} (Arguments name Flags) Configuration =
    case lookup name reportConfigReports of
        Nothing ->
            die $
            unlines
                [ "Unknown report name: " ++ name
                , "known options are: " ++ unwords (map fst reportConfigReports)
                ]
        Just r -> pure $ Instructions r Settings

getConfiguration :: Arguments -> IO Configuration
getConfiguration _ = pure Configuration

getArguments :: IO Arguments
getArguments = runArgumentsParser <$> getArgs >>= handleParseResult

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
        ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Smos reports"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseReportName <*> parseFlags

parseReportName :: Parser String
parseReportName =
    strArgument (mconcat [metavar "REPORT", help "the report to make"])

parseFlags :: Parser Flags
parseFlags = pure Flags
