module Smos.Cursor.Timestamps
    ( makeTimestampsCursor
    , timestampsCursorSetTimestamps
    , timestampsCursorTimestampsL
    ) where

import Import

import Data.List.NonEmpty (NonEmpty(..))

import Lens.Micro

import Cursor.Class

import Smos.Data

import Smos.Cursor.Entry.Timestamps
import Smos.Cursor.Types
import Smos.View

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
