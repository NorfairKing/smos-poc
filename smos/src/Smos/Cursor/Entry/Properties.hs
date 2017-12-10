module Smos.Cursor.Entry.Properties
    ( entryCursorPropertiesL
    , entryCursorPropertiesIndex
    ) where

import Import

import Lens.Micro

import Data.HashMap.Lazy (HashMap)

import Cursor.Tree

import Smos.Data

import Smos.Cursor.Types

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
            , entryCursorTags =
                  (\ec_ -> ec_ {tagsCursorParent = ec'}) <$> entryCursorTags ec
            , entryCursorTimestamps =
                  (entryCursorTimestamps ec) {timestampsCursorParent = ec'}
            , entryCursorProperties = ps
            }

entryCursorPropertiesIndex :: Int
entryCursorPropertiesIndex = 3
