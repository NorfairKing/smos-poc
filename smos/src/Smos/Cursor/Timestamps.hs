module Smos.Cursor.Timestamps
    ( makeTimestampsCursor
    , timestampsCursorSetTimestamps
    , timestampsCursorTimestampsL
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import Data.Time

import Lens.Micro

import Cursor.Class

import Smos.Data

import Smos.Cursor.Entry.Timestamps
import Smos.Cursor.Types

makeTimestampsCursor ::
       EntryCursor -> HashMap TimestampName UTCTime -> TimestampsCursor
makeTimestampsCursor ec hm = timestampsCursor ec $ view hm

timestampsCursorSetTimestamps ::
       HashMap TimestampName UTCTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorSetTimestamps ts = timestampsCursorTimestampsL .~ ts

timestampsCursorTimestampsL ::
       Functor f
    => (HashMap TimestampName UTCTime -> f (HashMap TimestampName UTCTime))
    -> TimestampsCursor
    -> f TimestampsCursor
timestampsCursorTimestampsL = lens getter setter
  where
    getter = timestampsCursorTimestamps
    setter tsc tss = tsc'
      where
        ec' = timestampsCursorParent tsc & entryCursorTimestampsL .~ tsc'
        tsc' =
            tsc {timestampsCursorParent = ec', timestampsCursorTimestamps = tss}
