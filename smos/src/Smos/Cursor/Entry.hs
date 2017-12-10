{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.Entry
    ( EntryCursor(..)
    , makeEntryCursor
    , entryCursor
    , entryCursorHeaderL
    , entryCursorContentsL
    , entryCursorStateL
    , entryCursorTagsL
    , entryCursorTimestampsL
    , entryCursorTimestampsMapL
    , entryCursorLogbookL
    , entryCursorPropertiesL
    , entryCursorClockIn
    , entryCursorContentsML
    ) where

import Import

import Data.Time

import Cursor.Class
import Cursor.Tree

import Smos.Data

import Smos.Cursor.Contents
import Smos.Cursor.Entry.Contents
import Smos.Cursor.Entry.Header
import Smos.Cursor.Entry.Logbook
import Smos.Cursor.Entry.Properties
import Smos.Cursor.Entry.State
import Smos.Cursor.Entry.Tags
import Smos.Cursor.Entry.Timestamps
import Smos.Cursor.Types

makeEntryCursor :: TreeCursor EntryCursor -> Entry -> EntryCursor
makeEntryCursor par e = entryCursor par $ view e

entryCursorClockIn :: UTCTime -> EntryCursor -> Maybe EntryCursor
entryCursorClockIn now = entryCursorLogbookL $ clockInAt now
