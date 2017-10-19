{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Data.Types where

import Import

import Data.Aeson
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Time

newtype SmosFile = SmosFile
    { smosFileForest :: SmosForest
    } deriving (Show, Eq, Generic)

instance Validity SmosFile

instance FromJSON SmosFile where
    parseJSON v = SmosFile <$> parseJSON v

instance ToJSON SmosFile where
    toJSON = toJSON . smosFileForest

newtype SmosForest = SmosForest
    { smosTrees :: [SmosTree]
    } deriving (Show, Eq, Generic)

instance Validity SmosForest

instance FromJSON SmosForest where
    parseJSON v = SmosForest <$> parseJSON v

instance ToJSON SmosForest where
    toJSON = toJSON . smosTrees

data SmosTree = SmosTree
    { treeEntry :: Entry
    , treeForest :: SmosForest
    } deriving (Show, Eq, Generic)

instance Validity SmosTree

instance FromJSON SmosTree where
    parseJSON =
        withObject "SmosTree" $ \o ->
            SmosTree <$> o .: "entry" <*> o .:? "forest" .!= SmosForest []

instance ToJSON SmosTree where
    toJSON SmosTree {..} = object ["entry" .= treeEntry, "forest" .= treeForest]

data Entry = Entry
    { entryHeader :: Header
    , entryContents :: Maybe Contents
    , entryTimestamps :: HashMap TimestampName UTCTime -- SCHEDULED, DEADLINE, etc.
    , entryState :: Maybe TodoState -- TODO, DONE, etc.
    , entryTags :: [Tag] -- '@home', 'toast', etc.
    , entryLogbook :: Logbook
    } deriving (Show, Eq, Generic)

instance Validity Entry

instance FromJSON Entry where
    parseJSON =
        withObject "Entry" $ \o ->
            Entry <$> o .: "header" <*> o .:? "contents" <*>
            o .:? "timestamps" .!= HM.empty <*>
            o .:? "state" <*>
            o .:? "tags" .!= [] <*>
            o .:? "logbook" .!= LogEnd

instance ToJSON Entry where
    toJSON Entry {..} =
        object
            [ "header" .= entryHeader
            , "contents" .= entryContents
            , "timestamps" .= entryTimestamps
            , "state" .= entryState
            , "tags" .= entryTags
            , "logbook" .= entryLogbook
            ]

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
