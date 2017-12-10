{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.Entry.Tags
    ( entryCursorTagsL
    , entryCursorTagsListL
    , entryCursorTagsIndex
    ) where

import Import

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Lens.Micro

import Cursor.Class
import Cursor.Select
import Cursor.Tree

import Smos.Data

import Smos.Cursor.Types

entryCursorTagsL :: Lens' EntryCursor (Maybe TagsCursor)
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

entryCursorTagsListL :: Lens' EntryCursor [Tag]
entryCursorTagsListL = lens getter setter
  where
    getter ec =
        maybe [] (NE.toList . source . selectValue . build) $
        ec ^. entryCursorTagsL
    setter ec [] = ec & entryCursorTagsL .~ Nothing
    setter ec (t:ts) =
        ec & entryCursorTagsL .~ Just (tagsCursor ec $ view (t :| ts))

entryCursorTagsIndex :: Int
entryCursorTagsIndex = 3
