{-# LANGUAGE RecordWildCards #-}

module Smos.OptParse
    ( getInstructions
    , Instructions
    , Dispatch(..)
    , Settings(..)
    ) where

import Import

import System.Environment (getArgs)
import System.Exit

import Options.Applicative

import Smos.OptParse.Types
import Smos.Types

getInstructions :: SmosConfig e -> IO Instructions
getInstructions conf = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions conf cmd flags config

combineToInstructions ::
       SmosConfig e -> Command -> Flags -> Configuration -> IO Instructions
combineToInstructions SmosConfig {..} cmd Flags Configuration =
    case cmd of
        CommandEdit fp -> do
            p <- resolveFile' fp
            pure (DispatchEdit p, Settings)
        CommandReport name ->
            case find ((== name) . reportName) configReports of
                Nothing ->
                    die $
                    unlines
                        [ "Unknown report name: " ++ name
                        , "known options are: " ++
                          unwords (map reportName configReports)
                        ]
                Just r -> pure (DispatchReport r, Settings)

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

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
    description = "Smos editor"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
    hsubparser
        (mconcat
             [ command "edit" parseCommandEdit
             , command "report" parseCommandReport
             ])

parseCommandEdit :: ParserInfo Command
parseCommandEdit = info parser modifier
  where
    parser = editParser
    modifier = fullDesc <> progDesc "Edit a smos file."

editParser :: Parser Command
editParser =
    CommandEdit <$>
    strArgument (mconcat [metavar "FILE", help "the file to edit"])

parseCommandReport :: ParserInfo Command
parseCommandReport = info parser modifier
  where
    parser =
        CommandReport <$>
        strArgument (mconcat [metavar "REPORT", help "the report to make"])
    modifier = fullDesc <> progDesc "Make a report."

parseFlags :: Parser Flags
parseFlags = pure Flags
