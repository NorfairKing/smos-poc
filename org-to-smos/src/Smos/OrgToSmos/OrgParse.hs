{-# LANGUAGE OverloadedStrings #-}

module Smos.OrgToSmos.OrgParse where

import Import

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import Smos.OrgToSmos.Types

parseOrgDoc :: T.Text -> Either ParseError OrgDoc
parseOrgDoc = parse orgDocParser ""

orgDocParser :: Parser OrgDoc
orgDocParser = OrgDoc <$> metadataParser <*> headlinesParser

metadataParser :: Parser [Metadata]
metadataParser =
    (fmap . fmap) Metadata (try metadataContent `sepEndBy` endOfLine)
  where
    metadataContent = do
        _headlinePrefix <- try $ lookAhead $ noneOf "*"
      -- Don't know why the the manyTill isn't working, it only parses the first line.
      -- content <- anyChar `manyTill` endOfLine
        content <- many $ noneOf "\n"
        pure $ T.pack content

headlinesParser :: Parser [Headline]
headlinesParser = try headline `sepEndBy` endOfLine
  where
    headline = Headline <$> (prefix *> tier') <*> status <*> content
    prefix = try $ lookAhead $ oneOf "*"
    tier' = (Tier . length) <$> many (oneOf "*")
    status = pure $ Status "Todo"
    content = T.pack <$> many (noneOf "\n")
