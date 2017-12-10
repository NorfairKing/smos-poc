module Smos.Cursor.Entry.Timestamps
    ( entryCursorTimestampsL
    , entryCursorTimestampsIndex
    ) where

import Import

import Lens.Micro

import Cursor.Tree

import Smos.Cursor.Types

entryCursorTimestampsL ::
       Functor f
    => (TimestampsCursor -> f TimestampsCursor)
    -> EntryCursor
    -> f EntryCursor
entryCursorTimestampsL = lens getter setter
  where
    getter = entryCursorTimestamps
    setter ec ts = ec'
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
            , entryCursorTags =
                  (\ec_ -> ec_ {tagsCursorParent = ec'}) <$> entryCursorTags ec
            , entryCursorTimestamps = ts
            }

entryCursorTimestampsIndex :: Int
entryCursorTimestampsIndex = 4
