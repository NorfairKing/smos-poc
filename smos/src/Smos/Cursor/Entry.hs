{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.Entry
    ( EntryCursor
    , makeEntryCursor
    , entryCursor
    , entryCursorParent
    , entryCursorHeader
    , entryCursorContents
    , entryCursorState
    , entryCursorTags
    , entryCursorTimestamps
    , entryCursorHeaderL
    , entryCursorContentsL
    , entryCursorStateL
    , entryCursorTagsL
    , entryCursorTimestampsL
    , entryCursorLogbookL
    , entryCursorPropertiesL
    , entryCursorClockIn
    , entryCursorContentsML
    , makeHeaderCursor
    , headerCursor
    , headerCursorParent
    , headerCursorHeader
    , headerCursorTextCursorL
    , headerCursorSetHeader
    , headerCursorHeaderL
    , headerCursorInsert
    , headerCursorAppend
    , headerCursorRemove
    , headerCursorDelete
    , headerCursorLeft
    , headerCursorRight
    , headerCursorStart
    , headerCursorEnd
    , emptyContentsCursor
    , makeContentsCursor
    , contentsCursor
    , contentsCursorParent
    , contentsCursorContents
    , contentsCursorContentsL
    , contentsCursorSetContents
    , contentsCursorTextFieldL
    , contentsCursorInsert
    , contentsCursorAppend
    , contentsCursorNewline
    , contentsCursorRemove
    , contentsCursorDelete
    , contentsCursorLeft
    , contentsCursorRight
    , contentsCursorUp
    , contentsCursorDown
    , contentsCursorStart
    , contentsCursorEnd
    , makeStateCursor
    , stateCursor
    , stateCursorParent
    , stateCursorStateHistory
    , stateCursorClear
    , stateCursorSetState
    , makeTagsCursor
    , tagsCursor
    , tagsCursorParent
    , tagsCursorTags
    , tagsCursorTagsL
    , tagsCursorSelectFirst
    , tagsCursorSelectLast
    , tagsCursorSetTags
    , tagsCursorInsertAt
    , tagsCursorInsertAtStart
    , tagsCursorAppendAtEnd
    , tagCursorParent
    , tagCursorIndex
    , tagCursorPrevElemens
    , tagCursorNextElemens
    , tagCursorTag
    , tagCursorTextCursorL
    , tagCursorModify
    , tagCursorInsert
    , tagCursorAppend
    , tagCursorRemove
    , tagCursorDelete
    , tagCursorLeft
    , tagCursorRight
    , tagCursorStart
    , tagCursorEnd
    , tagCursorSelectPrev
    , tagCursorSelectNext
    , makeTimestampsCursor
    , timestampsCursor
    , timestampsCursorParent
    , timestampsCursorTimestamps
    , timestampsCursorSetTimestamps
    , timestampsCursorTimestampsL
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import Data.Time

import Lens.Micro

import Cursor.Class
import Cursor.Tree

import Smos.Cursor.Contents
import Smos.Cursor.Entry.Contents
import Smos.Cursor.Entry.Header
import Smos.Cursor.Entry.Logbook
import Smos.Cursor.Entry.State
import Smos.Cursor.Entry.Tags
import Smos.Cursor.Entry.Timestamps
import Smos.Cursor.Header
import Smos.Cursor.State
import Smos.Cursor.Tags
import Smos.Cursor.Timestamps
import Smos.Cursor.Types
import Smos.Data

makeEntryCursor :: TreeCursor EntryCursor -> Entry -> EntryCursor
makeEntryCursor par e = entryCursor par $ view e

entryCursorPropertiesL ::
       Functor f
    => (HashMap PropertyName PropertyValue -> f (HashMap PropertyName PropertyValue))
    -> EntryCursor
    -> f EntryCursor
entryCursorPropertiesL = lens getter setter
  where
    getter = entryCursorProperties
    setter ec ps = ec'
      where
        ec' =
            ec
            { entryCursorParent = entryCursorParent ec & treeCursorValueL .~ ec'
            , entryCursorState = (entryCursorState ec) {stateCursorParent = ec'}
            , entryCursorHeader =
                  (entryCursorHeader ec) {headerCursorParent = ec'}
            , entryCursorContents =
                  (\ec_ -> ec_ {contentsCursorParent = ec'}) <$>
                  entryCursorContents ec
            , entryCursorTags = (entryCursorTags ec) {tagsCursorParent = ec'}
            , entryCursorTimestamps =
                  (entryCursorTimestamps ec) {timestampsCursorParent = ec'}
            , entryCursorProperties = ps
            }

entryCursorClockIn :: UTCTime -> EntryCursor -> Maybe EntryCursor
entryCursorClockIn now = entryCursorLogbookL $ clockInAt now
