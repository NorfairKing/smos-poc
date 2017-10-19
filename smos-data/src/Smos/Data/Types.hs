{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Data.Types where

import Import

import Data.Aeson
import Data.HashMap.Lazy (HashMap)
import Data.Hashable
import Data.Text (Text)
import Data.Time
import Data.Tree

newtype SmosFile = SmosFile
    { smosFileForrest :: Forest Entry
    } deriving (Show, Eq, Generic)

instance Validity SmosFile

instance FromJSON SmosFile

instance ToJSON SmosFile

data Entry = Entry
    { entryContents :: Contents
    , entryTimestamps :: HashMap TimestampName UTCTime -- SCHEDULED, DEADLINE, etc.
    , entryState :: TodoState -- TODO, DONE, etc.
    , entryTags :: [Tag] -- '@home', 'toast', etc.
    , entryLogbook :: Logbook
    } deriving (Show, Eq, Generic)

instance Validity Entry

instance FromJSON Entry

instance ToJSON Entry

newtype Contents =
    Contents Text
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity Contents

newtype TimestampName =
    TimestampName Text
    deriving ( Show
             , Eq
             , Generic
             , FromJSON
             , ToJSON
             , FromJSONKey
             , ToJSONKey
             , Hashable
             )

instance Validity TimestampName

newtype TodoState =
    TodoState Text
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity TodoState

newtype Tag =
    Tag Text
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

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
