module Smos.OrgToSmos
    ( convert
    ) where

import Import

import Data.Semigroup ((<>))
import Options.Applicative

import Smos.OrgToSmos.Types

convert :: IO ()
convert = putStrLn "converting..."

opts :: ParserInfo Options
opts =
    info
        optionsParser
        (fullDesc <> progDesc "Convert SOURCE org-mode file to TARGET smos file")

optionsParser :: Parser Options
optionsParser = Options <$> sourceParser <*> targetParser <*> verbosityParser

sourceParser :: Parser String
sourceParser = argument str (metavar "SOURCE")

targetParser :: Parser String
targetParser = argument str (metavar "TARGET")

verbosityParser :: Parser Verbosity
verbosityParser =
    flag
        Normal
        Verbose
        (long "verbose" <> short 'v' <> help "Enable verbose mode")
