{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.Timestamp
    ( TimestampCursor(..)
    , timestampCursorResolve
    , timestampCursorIndex
    , timestampCursorSelectPrev
    , timestampCursorSelectNext
    , timestampCursorSelectStart
    , timestampCursorSelectEnd
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

timestampCursorIndex :: TimestampCursor -> Int
timestampCursorIndex = textCursorIndex . timestampCursorText

timestampCursorSelectPrev :: TimestampCursor -> Maybe TimestampCursor
timestampCursorSelectPrev = timestampCursorTextCursorL textCursorSelectPrev

timestampCursorSelectNext :: TimestampCursor -> Maybe TimestampCursor
timestampCursorSelectNext = timestampCursorTextCursorL textCursorSelectNext

timestampCursorSelectStart :: TimestampCursor -> TimestampCursor
timestampCursorSelectStart = timestampCursorTextCursorL %~ textCursorSelectStart

timestampCursorSelectEnd :: TimestampCursor -> TimestampCursor
timestampCursorSelectEnd = timestampCursorTextCursorL %~ textCursorSelectEnd

timestampCursorInsert :: Char -> TimestampCursor -> TimestampCursor
timestampCursorInsert c = timestampCursorTextCursorL %~ textCursorInsert c

timestampCursorAppend :: Char -> TimestampCursor -> TimestampCursor
timestampCursorAppend c = timestampCursorTextCursorL %~ textCursorAppend c

timestampCursorRemove :: TimestampCursor -> Maybe TimestampCursor
timestampCursorRemove = timestampCursorTextCursorL textCursorRemove

timestampCursorDelete :: TimestampCursor -> Maybe TimestampCursor
timestampCursorDelete = timestampCursorTextCursorL textCursorDelete
