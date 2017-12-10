module Smos.Cursor.Entry.Tags
    ( entryCursorTagsL
    , entryCursorTagsIndex
    ) where

import Import

import Lens.Micro

import Cursor.Tree

import Smos.Cursor.Types

entryCursorTagsL ::
       Functor f => (TagsCursor -> f TagsCursor) -> EntryCursor -> f EntryCursor
entryCursorTagsL = lens getter setter
  where
    getter = entryCursorTags
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
            , entryCursorTags = ts
            , entryCursorTimestamps =
                  (\ec_ -> ec_ {timestampsCursorParent = ec'}) <$>
                  entryCursorTimestamps ec
            }

entryCursorTagsIndex :: Int
entryCursorTagsIndex = 3
