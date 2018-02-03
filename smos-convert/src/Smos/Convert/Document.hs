{-# LANGUAGE OverloadedStrings #-}

module Smos.Convert.Document where

import Import

import Data.Attoparsec.Text
import qualified Data.OrgMode.Parse.Attoparsec.Document as ORG
import Data.OrgMode.Types

getDocument :: [Text] -> Text -> Either String Document
getDocument statekeywords =
    parseOnly $ ORG.parseDocument (statekeywords ++ standardStatekeywords)

standardStatekeywords :: [Text]
standardStatekeywords =
    ["TODO", "WAITING", "SCHEDULED", "READY", "NEXT", "CANCELLED"]
