{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Convert.OptParse
    ( module Smos.Convert.OptParse
    , module Smos.Convert.OptParse.Types
    ) where

import Import hiding (lookup)

import Smos.Convert.OptParse.Types

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
    DispatchConvertFile . DispatchConvertFileArgs <$>
    sequence (parseAbsFile <$> convertArgsFiles)

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

parseCommand :: Parser Command
parseCommand = ConvertFile . ConvertFileArgs <$> some parseOrgFilePath

parseFlags :: Parser Flags
parseFlags = pure Flags
