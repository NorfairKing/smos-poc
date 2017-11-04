{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Data.Types
    ( SmosFile(..)
    , Forest
    , Tree(..)
    , Entry(..)
    , newEntry
    , TodoState(..)
    , Header(..)
    , Contents(..)
    , Tag(..)
    , Logbook(..)
    , TimestampName(..)
    ) where

import Import

import Data.Aeson as JSON
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Tree

import Control.Applicative

newtype SmosFile = SmosFile
    { smosFileForest :: Forest Entry
    } deriving (Show, Eq, Generic)

instance Validity SmosFile

instance FromJSON SmosFile where
    parseJSON v = SmosFile <$> parseJSON v

instance ToJSON SmosFile where
    toJSON = toJSON . smosFileForest

newtype ForYaml a = ForYaml
    { unForYaml :: a
    }

instance FromJSON (ForYaml (Forest Entry)) where
    parseJSON v = ForYaml <$> parseJSON v

instance ToJSON (ForYaml (Forest Entry)) where
    toJSON = toJSON . unForYaml

instance FromJSON (ForYaml (Tree Entry)) where
    parseJSON v =
        ForYaml <$>
        ((Node <$> parseJSON v <*> pure []) <|>
         (withObject "Tree Entry" $ \o ->
              Node <$> o .: "entry" <*> o .:? "forest" .!= [])
             v)

instance ToJSON (ForYaml (Tree Entry)) where
    toJSON (ForYaml Node {..}) =
        if null subForest
            then toJSON rootLabel
            else object $
                 ("entry" .= rootLabel) :
                 ["forest" .= subForest | not (null subForest)]

data Entry = Entry
    { entryHeader :: Header
    , entryContents :: Maybe Contents
    , entryTimestamps :: HashMap TimestampName UTCTime -- SCHEDULED, DEADLINE, etc.
    , entryState :: Maybe TodoState -- TODO, DONE, etc.
    , entryTags :: [Tag] -- '@home', 'toast', etc.
    , entryLogbook :: Logbook
    } deriving (Show, Eq, Generic)

newEntry :: Header -> Entry
newEntry h =
    Entry
    { entryHeader = h
    , entryContents = Nothing
    , entryTimestamps = HM.empty
    , entryState = Nothing
    , entryTags = []
    , entryLogbook = LogEnd
    }

instance Validity Entry

instance FromJSON Entry where
    parseJSON v =
        (do h <- parseJSON v
            pure $ newEntry h) <|>
        (withObject "Entry" $ \o ->
             Entry <$> o .: "header" <*> o .:? "contents" <*>
             o .:? "timestamps" .!= HM.empty <*>
             o .:? "state" <*>
             o .:? "tags" .!= [] <*>
             o .:? "logbook" .!= LogEnd)
            v

instance ToJSON Entry where
    toJSON Entry {..} =
        if and [ isNothing entryContents
               , HM.null entryTimestamps
               , isNothing entryState
               , null entryTags
               , entryLogbook == LogEnd
               ]
            then toJSON entryHeader
            else object $
                 ["header" .= entryHeader] ++
                 ["contents" .= entryContents | isJust entryContents] ++
                 [ "timestamps" .= entryTimestamps
                 | not $ HM.null entryTimestamps
                 ] ++
                 ["state" .= entryState | isJust entryState] ++
                 ["tags" .= entryTags | not $ null entryTags] ++
                 ["logbook" .= entryLogbook | entryLogbook /= LogEnd]

newtype Header = Header
    { headerText :: Text
    } deriving (Show, Eq, Generic, IsString, FromJSON, ToJSON)

instance Validity Header

newtype Contents = Contents
    { contentsText :: Text
    } deriving (Show, Eq, Generic, IsString, FromJSON, ToJSON)

instance Validity Contents

newtype TimestampName = TimestampName
    { timestampNameText :: Text
    } deriving ( Show
               , Eq
               , Generic
               , IsString
               , FromJSON
               , ToJSON
               , FromJSONKey
               , ToJSONKey
               , Hashable
               )

instance Validity TimestampName

newtype TodoState = TodoState
    { todoStateText :: Text
    } deriving (Show, Eq, Generic, IsString, FromJSON, ToJSON)

instance Validity TodoState

newtype Tag = Tag
    { tagText :: Text
    } deriving (Show, Eq, Generic, IsString, FromJSON, ToJSON)

instance Validity Tag

data Logbook
    = LogEnd
    | LogEntry UTCTime
               UTCTime
               Logbook
    | LogOpenEntry UTCTime
                   Logbook
    deriving (Show, Eq, Generic)

instance Validity Logbook

instance FromJSON Logbook

instance ToJSON Logbook
