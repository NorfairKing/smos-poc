module Smos.OptParse
    ( getInstructions
    , Instructions
    , Dispatch(..)
    , Settings(..)
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

import Smos.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions (CommandEdit fp) Flags Configuration = do
    p <- resolveFile' fp
    pure (DispatchEdit p, Settings)

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
    hsubparser (mconcat [command "edit" parseCommandEdit]) <|> editParser

parseCommandEdit :: ParserInfo Command
parseCommandEdit = info parser modifier
  where
    parser = editParser
    modifier = fullDesc <> progDesc "Edit a smos file."

editParser :: Parser Command
editParser =
    CommandEdit <$>
    strArgument (mconcat [metavar "FILE", help "the file to edit"])

parseFlags :: Parser Flags
parseFlags = pure Flags
