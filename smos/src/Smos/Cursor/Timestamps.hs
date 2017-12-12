{-# LANGUAGE OverloadedStrings #-}

module Smos.Cursor.Timestamps
    ( TimestampsCursor(..)
    , newTimestampsCursor
    , makeTimestampsCursor
    , timestampsCursorSetTimestamps
    , timestampsCursorTimestampsL
    , timestampsCursorListL
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
    ) where

import Import

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time

import Lens.Micro

import Cursor.Class
import Cursor.Map

import Smos.Data

import Smos.Cursor.Entry.Timestamps
import Smos.Cursor.Types
import Smos.View

newTimestampsCursor :: EntryCursor -> UTCTime -> TimestampsCursor
newTimestampsCursor ec now =
    makeTimestampsCursor ec $ (TimestampName "", TimestampTime now) :| []

makeTimestampsCursor ::
       EntryCursor -> NonEmpty (TimestampName, Timestamp) -> TimestampsCursor
makeTimestampsCursor ec = timestampsCursor ec . TimestampsView . view

timestampsCursorSetTimestamps ::
       NonEmpty (TimestampName, Timestamp)
    -> TimestampsCursor
    -> TimestampsCursor
timestampsCursorSetTimestamps ts = timestampsCursorTimestampsL .~ ts

timestampsCursorTimestampsL ::
       Lens' TimestampsCursor (NonEmpty (TimestampName, Timestamp))
timestampsCursorTimestampsL = lens getter setter
  where
    getter = source . rebuild . timestampsCursorTimestamps
    setter ::
           TimestampsCursor
        -> NonEmpty (TimestampName, Timestamp)
        -> TimestampsCursor
    setter tsc tss = tsc'
      where
        ec' = timestampsCursorParent tsc & entryCursorTimestampsL .~ Just tsc'
        tsc' = timestampsCursor ec' $ TimestampsView $ view tss

timestampsCursorListL ::
       Lens' TimestampsCursor (MapCursor TimestampName Timestamp)
timestampsCursorListL = lens getter setter
  where
    getter = timestampsCursorTimestamps
    setter tsc l = tsc {timestampsCursorTimestamps = l}

timestampsCursorSelectedL ::
       Lens' TimestampsCursor (KeyValueCursor TimestampName Timestamp)
timestampsCursorSelectedL = timestampsCursorListL . mapCursorSelectedL

timestampsCursorSelectPrev :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectPrev = timestampsCursorListL mapCursorSelectPrev

timestampsCursorSelectNext :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectNext = timestampsCursorListL mapCursorSelectNext

timestampsCursorSelectFirst :: TimestampsCursor -> TimestampsCursor
timestampsCursorSelectFirst = timestampsCursorListL %~ mapCursorSelectFirst

timestampsCursorSelectLast :: TimestampsCursor -> TimestampsCursor
timestampsCursorSelectLast = timestampsCursorListL %~ mapCursorSelectLast

timestampsCursorInsert ::
       TimestampName -> Timestamp -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsert n ts = timestampsCursorListL %~ mapCursorInsert n ts

timestampsCursorAppend ::
       TimestampName -> Timestamp -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppend n ts = timestampsCursorListL %~ mapCursorAppend n ts

timestampsCursorInsertAndSelect ::
       TimestampName -> Timestamp -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsertAndSelect n ts =
    timestampsCursorListL %~ mapCursorInsertAndSelect n ts

timestampsCursorAppendAndSelect ::
       TimestampName -> Timestamp -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendAndSelect n ts =
    timestampsCursorListL %~ mapCursorInsertAndSelect n ts

timestampsCursorRemoveElemAndSelectPrev ::
       TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorRemoveElemAndSelectPrev =
    timestampsCursorListL mapCursorRemoveElemAndSelectPrev

timestampsCursorDeleteElemAndSelectNext ::
       TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorDeleteElemAndSelectNext =
    timestampsCursorListL mapCursorDeleteElemAndSelectNext

timestampsCursorRemoveElem :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorRemoveElem = timestampsCursorListL mapCursorRemoveElem

timestampsCursorDeleteElem :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorDeleteElem = timestampsCursorListL mapCursorDeleteElem
