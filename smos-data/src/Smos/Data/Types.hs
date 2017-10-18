{-# LANGUAGE DeriveGeneric #-}

module Smos.Data.Types where

import Import

import Data.HashMap.Lazy (HashMap)
import Data.Text (Text)
import Data.Time
import Data.Tree

data SmosFile = SmosFile
    { smosFileForrest :: Forest Entry
    } deriving (Show, Eq, Generic)

data Entry = Entry
    { entryContents :: Contents
    , entryTimestamps :: HashMap TimestampName UTCTime -- SCHEDULED, DEADLINE, etc.
    , entryState :: TodoState -- TODO, DONE, etc.
    , entryTags :: [Tag] -- '@home', 'toast', etc.
    , entryLogbook :: Logbook
    } deriving (Show, Eq, Generic)

newtype Contents =
    Contents Text
    deriving (Show, Eq, Generic)

newtype TimestampName =
    TimestampName Text
    deriving (Show, Eq, Generic)

newtype TodoState =
    TodoState Text
    deriving (Show, Eq, Generic)

newtype Tag =
    Tag Text
    deriving (Show, Eq, Generic)

data Logbook
    = LogEnd
    | LogEntry UTCTime
               UTCTime
               Logbook
    | LogOpenEntry UTCTime
                   Logbook
    deriving (Show, Eq, Generic)
