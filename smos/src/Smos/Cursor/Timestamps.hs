{-# LANGUAGE OverloadedStrings #-}

module Smos.Cursor.Timestamps
    ( TimestampsCursor(..)
    , newTimestampsCursor
    , makeNewTimestampsCursor
    , makeTimestampsCursor
    , timestampsCursorSetTimestamps
    , timestampsCursorTimestampsL
    , timestampsCursorMapL
    , timestampsCursorSelectedL
    , timestampsCursorSelectPrev
    , timestampsCursorSelectNext
    , timestampsCursorSelectFirst
    , timestampsCursorSelectLast
    , timestampsCursorInsert
    , timestampsCursorAppend
    , timestampsCursorInsertAndSelect
    , timestampsCursorAppendAndSelect
    , timestampsCursorRemoveElemAndSelectPrev
    , timestampsCursorDeleteElemAndSelectNext
    , timestampsCursorRemoveElem
    , timestampsCursorDeleteElem
    , TimestampNameCursor(..)
    ) where

import Import

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time

import Lens.Micro

import Cursor.Class
import Cursor.Map
import Cursor.Select

import Smos.Data

import Smos.Cursor.Entry.Timestamps
import Smos.Cursor.Types

newTimestampsCursor :: EntryCursor -> UTCTime -> TimestampsCursor
newTimestampsCursor ec now =
    makeTimestampsCursor ec $ (TimestampName "", TimestampTime now) :| []

makeNewTimestampsCursor ::
       EntryCursor -> TimestampName -> Timestamp -> TimestampsCursor
makeNewTimestampsCursor ec tsn ts = makeTimestampsCursor ec $ (tsn, ts) :| []

makeTimestampsCursor ::
       EntryCursor -> NonEmpty (TimestampName, Timestamp) -> TimestampsCursor
makeTimestampsCursor ec = timestampsCursor ec . view

timestampsCursorTimestampsL ::
       Lens' TimestampsCursor (NonEmpty (TimestampName, Timestamp))
timestampsCursorTimestampsL =
    lens (source . selectValue . build) (flip timestampsCursorSetTimestamps)

timestampsCursorSetTimestamps ::
       NonEmpty (TimestampName, Timestamp)
    -> TimestampsCursor
    -> TimestampsCursor
timestampsCursorSetTimestamps tss tsc = tsc'
  where
    ec' = timestampsCursorParent tsc & entryCursorTimestampsL .~ Just tsc'
    tsc' = timestampsCursor ec' $ view tss

timestampsCursorMapL ::
       Lens' TimestampsCursor (MapCursor TimestampNameCursor Timestamp)
timestampsCursorMapL = lens getter setter
  where
    getter = timestampsCursorTimestamps
    setter tsc l = tsc {timestampsCursorTimestamps = l}

timestampsCursorSelectedL ::
       Lens' TimestampsCursor (KeyValueCursor TimestampNameCursor Timestamp)
timestampsCursorSelectedL = timestampsCursorMapL . mapCursorSelectedL

timestampsCursorSelectPrev :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectPrev = timestampsCursorMapL mapCursorSelectPrev

timestampsCursorSelectNext :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectNext = timestampsCursorMapL mapCursorSelectNext

timestampsCursorSelectFirst :: TimestampsCursor -> TimestampsCursor
timestampsCursorSelectFirst = timestampsCursorMapL %~ mapCursorSelectFirst

timestampsCursorSelectLast :: TimestampsCursor -> TimestampsCursor
timestampsCursorSelectLast = timestampsCursorMapL %~ mapCursorSelectLast

timestampsCursorInsert ::
       TimestampNameCursor -> Timestamp -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsert n ts = timestampsCursorMapL %~ mapCursorInsert n ts

timestampsCursorAppend ::
       TimestampNameCursor -> Timestamp -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppend n ts = timestampsCursorMapL %~ mapCursorAppend n ts

timestampsCursorInsertAndSelect ::
       TimestampNameCursor -> Timestamp -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsertAndSelect n ts =
    timestampsCursorMapL %~ mapCursorInsertAndSelect n ts

timestampsCursorAppendAndSelect ::
       TimestampNameCursor -> Timestamp -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendAndSelect n ts =
    timestampsCursorMapL %~ mapCursorInsertAndSelect n ts

timestampsCursorRemoveElemAndSelectPrev ::
       TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorRemoveElemAndSelectPrev =
    timestampsCursorMapL mapCursorRemoveElemAndSelectPrev

timestampsCursorDeleteElemAndSelectNext ::
       TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorDeleteElemAndSelectNext =
    timestampsCursorMapL mapCursorDeleteElemAndSelectNext

timestampsCursorRemoveElem :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorRemoveElem = timestampsCursorMapL mapCursorRemoveElem

timestampsCursorDeleteElem :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorDeleteElem = timestampsCursorMapL mapCursorDeleteElem
