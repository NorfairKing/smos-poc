{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Data.Types where

import Import

import Data.Aeson as JSON
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Time

import Control.Applicative

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
    parseJSON v =
        (SmosTree <$> parseJSON v <*> pure (SmosForest [])) <|>
        (withObject "SmosTree" $ \o ->
             SmosTree <$> o .: "entry" <*> o .:? "forest" .!= SmosForest [])
            v

instance ToJSON SmosTree where
    toJSON SmosTree {..} =
        if treeForest == SmosForest []
            then toJSON treeEntry
            else object $
                 ("entry" .= treeEntry) :
                 ["forest" .= treeForest | treeForest /= SmosForest []]

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
