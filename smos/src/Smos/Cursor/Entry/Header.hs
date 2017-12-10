module Smos.Cursor.Entry.Header
    ( entryCursorHeaderL
    , entryCursorHeaderIndex
    ) where

import Import

import Lens.Micro

import Cursor.Tree

import Smos.Cursor.Types

entryCursorHeaderL ::
       Functor f
    => (HeaderCursor -> f HeaderCursor)
    -> EntryCursor
    -> f EntryCursor
entryCursorHeaderL = lens getter setter
  where
    getter = entryCursorHeader
    setter ec hc = ec'
      where
        ec' =
            ec
            { entryCursorParent = entryCursorParent ec & treeCursorValueL .~ ec'
            , entryCursorHeader = hc
            , entryCursorContents =
                  (\ec_ -> ec_ {contentsCursorParent = ec'}) <$>
                  entryCursorContents ec
            , entryCursorState = (entryCursorState ec) {stateCursorParent = ec'}
            , entryCursorTags = (entryCursorTags ec) {tagsCursorParent = ec'}
            , entryCursorTimestamps =
                  (\ec_ -> ec_ {timestampsCursorParent = ec'}) <$>
                  entryCursorTimestamps ec
            }

entryCursorHeaderIndex :: Int
entryCursorHeaderIndex = 2
