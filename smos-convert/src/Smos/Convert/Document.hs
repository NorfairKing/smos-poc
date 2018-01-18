{-# LANGUAGE OverloadedStrings #-}

module Smos.Convert.Document where

import Import

import Data.Attoparsec.Text
import qualified Data.OrgMode.Parse.Attoparsec.Document as ORG
import Data.OrgMode.Types

parseDocument :: Parser Document
parseDocument =
    ORG.parseDocument
        ["TODO", "WAITING", "SCHEDULED", "READY", "NEXT", "CANCELLED"]

getDocument :: Text -> Either String Document
getDocument = parseOnly parseDocument
