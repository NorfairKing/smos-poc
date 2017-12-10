module Smos.Cursor.Entry.Contents
    ( entryCursorContentsL
    , entryCursorContentsIndex
    ) where

import Import

import Lens.Micro

import Cursor.Tree

import Smos.Cursor.Types

entryCursorContentsL :: Lens' EntryCursor (Maybe ContentsCursor)
entryCursorContentsL = lens getter setter
  where
    getter = entryCursorContents
    setter ec mcc = ec'
      where
        ec' =
            ec
            { entryCursorParent = entryCursorParent ec & treeCursorValueL .~ ec'
            , entryCursorState = (entryCursorState ec) {stateCursorParent = ec'}
            , entryCursorHeader =
                  (entryCursorHeader ec) {headerCursorParent = ec'}
            , entryCursorContents = mcc
            , entryCursorTags = (entryCursorTags ec) {tagsCursorParent = ec'}
            , entryCursorTimestamps =
                  (\ec_ -> ec_ {timestampsCursorParent = ec'}) <$>
                  entryCursorTimestamps ec
            }

entryCursorContentsIndex :: Int
entryCursorContentsIndex = 5
