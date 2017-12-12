{-# LANGUAGE OverloadedStrings #-}

module Smos.Cursor.TimestampName
    ( TimestampNameCursor(..)
    , timestampNameTextCursorL
    , timestampNameCursorNull
    , timestampNameCursorInsert
    , timestampNameCursorAppend
    , timestampNameCursorRemove
    , timestampNameCursorDelete
    , timestampNameCursorLeft
    , timestampNameCursorRight
    , timestampNameCursorStart
    , timestampNameCursorEnd
    ) where

import Import

import Lens.Micro

import Cursor.Text

import Smos.Cursor.Types

timestampNameTextCursorL :: Lens' TimestampNameCursor TextCursor
timestampNameTextCursorL =
    lens timestampNameCursorTimestampName $ \tsn tc ->
        tsn {timestampNameCursorTimestampName = tc}

timestampNameCursorNull :: TimestampNameCursor -> Bool
timestampNameCursorNull = textCursorNull . timestampNameCursorTimestampName

timestampNameCursorInsert :: Char -> TimestampNameCursor -> TimestampNameCursor
timestampNameCursorInsert c = timestampNameTextCursorL %~ textCursorInsert c

timestampNameCursorAppend :: Char -> TimestampNameCursor -> TimestampNameCursor
timestampNameCursorAppend c = timestampNameTextCursorL %~ textCursorAppend c

timestampNameCursorRemove :: TimestampNameCursor -> Maybe TimestampNameCursor
timestampNameCursorRemove = timestampNameTextCursorL textCursorRemove

timestampNameCursorDelete :: TimestampNameCursor -> Maybe TimestampNameCursor
timestampNameCursorDelete = timestampNameTextCursorL textCursorDelete

timestampNameCursorLeft :: TimestampNameCursor -> Maybe TimestampNameCursor
timestampNameCursorLeft = timestampNameTextCursorL textCursorSelectPrev

timestampNameCursorRight :: TimestampNameCursor -> Maybe TimestampNameCursor
timestampNameCursorRight = timestampNameTextCursorL textCursorSelectNext

timestampNameCursorStart :: TimestampNameCursor -> TimestampNameCursor
timestampNameCursorStart = timestampNameTextCursorL %~ textCursorSelectStart

timestampNameCursorEnd :: TimestampNameCursor -> TimestampNameCursor
timestampNameCursorEnd = timestampNameTextCursorL %~ textCursorSelectEnd
