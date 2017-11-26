module Smos.Cursor.Entry.Logbook
    ( entryCursorLogbookL
    , entryCursorLogbookIndex
    ) where

import Import

import Lens.Micro

import Cursor.Tree

import Smos.Data

import Smos.Cursor.Types

entryCursorLogbookL ::
       Functor f => (Logbook -> f Logbook) -> EntryCursor -> f EntryCursor
entryCursorLogbookL = lens getter setter
  where
    getter = entryCursorLogbook
    setter ec lb = ec'
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
            , entryCursorTimestamps =
                  (entryCursorTimestamps ec) {timestampsCursorParent = ec'}
            , entryCursorLogbook = lb
            }

entryCursorLogbookIndex :: Int
entryCursorLogbookIndex = 6
