{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Convert.OptParse
    ( module Convert.OptParse
    , module Convert.OptParse.Types
    ) where

import Import hiding (lookup)

import Convert.OptParse.Types

import Options.Applicative
import System.Environment

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    dispatch <- getDispatch cmd flags
    settings <- getSettings flags
    pure (dispatch, settings)

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

getSettings :: Flags -> IO Settings
getSettings _ = pure Settings

getSettingsFromConfig :: Flags -> Configuration -> IO Settings
getSettingsFromConfig _ _ = pure Settings

getDispatch :: Command -> Flags -> IO Dispatch
getDispatch (ConvertFile ConvertFileArgs {..}) _ =
    DispatchConvertFile . DispatchConvertFileArgs <$> parseAbsFile orgfile

getConfig :: Flags -> IO Configuration
getConfig _ = pure Configuration

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
    description = "Smos converter"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseOrgFilePath :: Parser FilePath
parseOrgFilePath =
    strArgument (mconcat [metavar "FILE", help "Orgfile to convert"])

parseConvertFileArgs :: Parser ConvertFileArgs
parseConvertFileArgs = ConvertFileArgs <$> parseOrgFilePath

parseConvertFile :: ParserInfo Command
parseConvertFile = info parser modifier
  where
    modifier = fullDesc <> progDesc "Write smosfile corresponding to an orgfile"
    parser = ConvertFile <$> parseConvertFileArgs

parseCommand :: Parser Command
parseCommand = hsubparser $ command "convert" parseConvertFile

parseFlags :: Parser Flags
parseFlags = pure Flags
