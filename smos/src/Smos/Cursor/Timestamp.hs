{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.Timestamp
    ( TimestampCursor(..)
    , timestampCursorResolve
    , timestampCursorNull
    , timestampCursorIndex
    , timestampCursorLeft
    , timestampCursorRight
    , timestampCursorStart
    , timestampCursorEnd
    , timestampCursorInsert
    , timestampCursorAppend
    , timestampCursorRemove
    , timestampCursorDelete
    ) where

import Import

import Lens.Micro

import qualified Data.Text as T
import Data.Time
import Text.Megaparsec

import Data.FuzzyTime
import Smos.Data

import Cursor.Class
import Cursor.Text

import Smos.Cursor.Types

timestampCursorResolve :: ZonedTime -> TimestampCursor -> Timestamp
timestampCursorResolve now TimestampCursor {..} =
    fromMaybe timestampCursorCurrent $
    (TimestampTime . resolveDateTime now) <$>
    parseMaybe fuzzyDateTimeP (T.unpack $ source $ rebuild timestampCursorText)

timestampCursorTextCursorL ::
       Functor f
    => (TextCursor -> f TextCursor)
    -> TimestampCursor
    -> f TimestampCursor
timestampCursorTextCursorL =
    lens timestampCursorText (\ftc tc -> ftc {timestampCursorText = tc})

timestampCursorNull :: TimestampCursor -> Bool
timestampCursorNull = textCursorNull . timestampCursorText

timestampCursorIndex :: TimestampCursor -> Int
timestampCursorIndex = textCursorIndex . timestampCursorText

timestampCursorLeft :: TimestampCursor -> Maybe TimestampCursor
timestampCursorLeft = timestampCursorTextCursorL textCursorSelectPrev

timestampCursorRight :: TimestampCursor -> Maybe TimestampCursor
timestampCursorRight = timestampCursorTextCursorL textCursorSelectNext

timestampCursorStart :: TimestampCursor -> TimestampCursor
timestampCursorStart = timestampCursorTextCursorL %~ textCursorSelectStart

timestampCursorEnd :: TimestampCursor -> TimestampCursor
timestampCursorEnd = timestampCursorTextCursorL %~ textCursorSelectEnd

timestampCursorInsert :: Char -> TimestampCursor -> TimestampCursor
timestampCursorInsert c = timestampCursorTextCursorL %~ textCursorInsert c

timestampCursorAppend :: Char -> TimestampCursor -> TimestampCursor
timestampCursorAppend c = timestampCursorTextCursorL %~ textCursorAppend c

timestampCursorRemove :: TimestampCursor -> Maybe TimestampCursor
timestampCursorRemove = timestampCursorTextCursorL textCursorRemove

timestampCursorDelete :: TimestampCursor -> Maybe TimestampCursor
timestampCursorDelete = timestampCursorTextCursorL textCursorDelete
