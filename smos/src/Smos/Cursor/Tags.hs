{-# LANGUAGE OverloadedStrings #-}

module Smos.Cursor.Tags
    ( TagsCursor(..)
    , newTagsCursor
    , makeNewTagsCursor
    , makeTagsCursor
    , tagsCursorTagCursorsL
    , tagsCursorTagsL
    , tagsCursorSetTags
    , tagsCursorSelectedL
    , tagsCursorSelectPrev
    , tagsCursorSelectNext
    , tagsCursorSelectFirst
    , tagsCursorSelectLast
    , tagsCursorInsertAndSelect
    , tagsCursorAppendAndSelect
    , tagsCursorSet
    , tagsCursorUnset
    , tagsCursorToggle
    ) where

import Import

import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE

import Lens.Micro

import Cursor.Class
import Cursor.ListElem
import Cursor.Select

import Smos.Data

import Smos.Cursor.Entry.Tags
import Smos.Cursor.Tags.Tag
import Smos.Cursor.Types

newTagsCursor :: EntryCursor -> TagsCursor
newTagsCursor ec = makeTagsCursor ec $ Tag "" :| []

makeNewTagsCursor :: EntryCursor -> Tag -> TagsCursor
makeNewTagsCursor ec tag = tagsCursor ec $ view (tag :| [])

makeTagsCursor :: EntryCursor -> NonEmpty Tag -> TagsCursor
makeTagsCursor ec tags = tagsCursor ec $ view tags

tagsCursorTagsL :: Lens' TagsCursor (NonEmpty Tag)
tagsCursorTagsL = lens (source . selectValue . build) (flip tagsCursorSetTags)

tagsCursorSetTags :: NonEmpty Tag -> TagsCursor -> TagsCursor
tagsCursorSetTags tgs tc = tc'
  where
    ec' = tagsCursorParent tc & entryCursorTagsL .~ Just tc'
    tc' = tagsCursor ec' $ view tgs

tagsCursorSelectedL :: Lens' TagsCursor TagCursor
tagsCursorSelectedL = tagsCursorTagCursorsL . listElemCursorElemL

tagsCursorSelectNext :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectNext = tagsCursorTagCursorsL listElemCursorSelectNext

tagsCursorSelectPrev :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectPrev = tagsCursorTagCursorsL listElemCursorSelectPrev

tagsCursorSelectFirst :: TagsCursor -> TagsCursor
tagsCursorSelectFirst = tagsCursorTagCursorsL %~ listElemCursorSelectFirst

tagsCursorSelectLast :: TagsCursor -> TagsCursor
tagsCursorSelectLast = tagsCursorTagCursorsL %~ listElemCursorSelectLast

tagsCursorInsertAndSelect :: Text -> TagsCursor -> TagsCursor
tagsCursorInsertAndSelect t =
    tagsCursorTagCursorsL %~ listElemCursorInsertAndSelect (tagCursor $ view t)

tagsCursorAppendAndSelect :: Text -> TagsCursor -> TagsCursor
tagsCursorAppendAndSelect t =
    tagsCursorTagCursorsL %~ listElemCursorAppendAndSelect (tagCursor $ view t)

tagsCursorSet :: Tag -> TagsCursor -> TagsCursor
tagsCursorSet t =
    tagsCursorTagsL %~
    (\ts ->
         if t `elem` ts
             then ts
             else t <| ts)

tagsCursorUnset :: Tag -> TagsCursor -> Maybe TagsCursor
tagsCursorUnset t tc =
    if t `elem` (tc ^. tagsCursorTagsL)
        then (\ts -> tc & tagsCursorTagsL .~ ts) <$>
             NE.nonEmpty (NE.filter (/= t) (tc ^. tagsCursorTagsL))
        else Just tc

tagsCursorToggle :: Tag -> TagsCursor -> Maybe TagsCursor
tagsCursorToggle t tc =
    if t `elem` (tc ^. tagsCursorTagsL)
        then tagsCursorUnset t tc
        else Just $ tagsCursorSet t tc
