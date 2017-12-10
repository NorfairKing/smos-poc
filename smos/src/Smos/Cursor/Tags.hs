module Smos.Cursor.Tags
    ( TagsCursor(..)
    , makeTagsCursor
    , tagsCursorTagCursorsL
    , tagsCursorTagsL
    , tagsCursorSelectFirst
    , tagsCursorSelectLast
    , tagsCursorSetTags
    , tagsCursorInsertAt
    , tagsCursorInsertAtStart
    , tagsCursorAppendAtEnd
    ) where

import Import

import Lens.Micro

import Cursor.Class
import Cursor.Select

import Smos.Data

import Smos.Cursor.Entry.Tags
import Smos.Cursor.Tags.Tag
import Smos.Cursor.Types
import Smos.View

makeTagsCursor :: EntryCursor -> [Tag] -> TagsCursor
makeTagsCursor ec tags = tagsCursor ec $ view tags

tagsCursorTagsL :: Lens' TagsCursor [Tag]
tagsCursorTagsL = lens (source . selectValue . build) setter
  where
    setter tc tgs = tc'
      where
        ec' = tagsCursorParent tc & entryCursorTagsL .~ tc'
        tc' = tagsCursor ec' $ view tgs

tagsCursorSelectFirst :: TagsCursor -> Maybe TagCursor
tagsCursorSelectFirst tsc =
    case tagsCursorTags tsc of
        [] -> Nothing
        (tc:_) -> Just tc

tagsCursorSelectLast :: TagsCursor -> Maybe TagCursor
tagsCursorSelectLast tsc =
    case reverse $ tagsCursorTags tsc of
        [] -> Nothing
        (tc:_) -> Just tc

tagsCursorSetTags :: [Tag] -> TagsCursor -> TagsCursor
tagsCursorSetTags tgs tsc = tsc'
  where
    tsc' = tsc {tagsCursorTags = tagElems tsc' $ map tagView tgs}

tagsCursorInsertAt :: Int -> Tag -> TagsCursor -> TagsCursor
tagsCursorInsertAt ix_ newTag tsc = tsc'
  where
    tsc' =
        tsc & tagsCursorTagCursorsL %~
        (\els ->
             tagElems tsc' $
             concat
                 [ map (selectValue . build) (prevs els)
                 , [tagView newTag]
                 , map (selectValue . build) (nexts els)
                 ])
    ffilter rel = filter ((`rel` ix_) . tagCursorIndex)
    prevs = ffilter (<)
    nexts = ffilter (>=)

tagsCursorInsertAtStart :: Tag -> TagsCursor -> TagsCursor
tagsCursorInsertAtStart = tagsCursorInsertAt 0

tagsCursorAppendAtEnd :: Tag -> TagsCursor -> TagsCursor
tagsCursorAppendAtEnd t fc =
    tagsCursorInsertAt (length $ tagsCursorTags fc) t fc
