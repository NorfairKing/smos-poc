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
    , tagsCursorRemoveElemAndSelectPrev
    , tagsCursorDeleteElemAndSelectNext
    , tagsCursorRemoveElem
    , tagsCursorDeleteElem
    , tagsCursorRemove
    , tagsCursorDelete
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
import Smos.Cursor.Tag
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
tagsCursorSelectNext tc =
    if tc ^. tagsCursorSelectedL . to tagCursorNull
        then tagsCursorDeleteElem tc
        else tagsCursorTagCursorsL listElemCursorSelectNext tc

tagsCursorSelectPrev :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectPrev tc =
    if tc ^. tagsCursorSelectedL . to tagCursorNull
        then tagsCursorRemoveElem tc
        else tagsCursorTagCursorsL listElemCursorSelectPrev tc

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

tagsCursorRemoveElemAndSelectPrev :: TagsCursor -> Maybe TagsCursor
tagsCursorRemoveElemAndSelectPrev =
    tagsCursorTagCursorsL listElemCursorRemoveElemAndSelectPrev

tagsCursorDeleteElemAndSelectNext :: TagsCursor -> Maybe TagsCursor
tagsCursorDeleteElemAndSelectNext =
    tagsCursorTagCursorsL listElemCursorDeleteElemAndSelectNext

tagsCursorRemoveElem :: TagsCursor -> Maybe TagsCursor
tagsCursorRemoveElem = tagsCursorTagCursorsL listElemCursorRemoveElem

tagsCursorDeleteElem :: TagsCursor -> Maybe TagsCursor
tagsCursorDeleteElem = tagsCursorTagCursorsL listElemCursorDeleteElem

tagsCursorRemove :: TagsCursor -> NOUOD TagsCursor
tagsCursorRemove tgsc =
    if tgsc ^. tagsCursorSelectedL . to tagCursorNull
        then case tagsCursorRemoveElem tgsc of
                 Nothing -> Deleted
                 Just t -> New t
        else case tgsc & tagsCursorSelectedL tagCursorRemove of
                 Nothing -> Unchanged
                 Just t -> New t

tagsCursorDelete :: TagsCursor -> NOUOD TagsCursor
tagsCursorDelete tgsc =
    if tgsc ^. tagsCursorSelectedL . to tagCursorNull
        then case tagsCursorDeleteElem tgsc of
                 Nothing -> Deleted
                 Just t -> New t
        else case tgsc & tagsCursorSelectedL tagCursorDelete of
                 Nothing -> Unchanged
                 Just t -> New t

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
