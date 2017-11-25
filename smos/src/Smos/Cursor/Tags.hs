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
    , TagCursor(..)
    , tagCursorTextCursorL
    , tagCursorModify
    , tagCursorInsert
    , tagCursorAppend
    , tagCursorRemove
    , tagCursorDelete
    , tagCursorLeft
    , tagCursorRight
    , tagCursorStart
    , tagCursorEnd
    , tagCursorSelectPrev
    , tagCursorSelectNext
    ) where

import Import

import Lens.Micro

import Cursor.Class
import Cursor.Select
import Cursor.Text

import Smos.Data

import Smos.Cursor.Entry.Tags
import Smos.Cursor.Types
import Smos.View

makeTagsCursor :: EntryCursor -> [Tag] -> TagsCursor
makeTagsCursor ec tags = tagsCursor ec $ view tags

tagsCursorTagCursorsL ::
       Functor f => ([TagCursor] -> f [TagCursor]) -> TagsCursor -> f TagsCursor
tagsCursorTagCursorsL = lens getter setter
  where
    getter = tagsCursorTags
    setter cc ts = cc'
      where
        ec' = tagsCursorParent cc & entryCursorTagsL .~ cc'
        cc' = cc {tagsCursorParent = ec', tagsCursorTags = ts}

tagsCursorTagsL :: Functor f => ([Tag] -> f [Tag]) -> TagsCursor -> f TagsCursor
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

tagCursorTextCursorL ::
       Functor f => (TextCursor -> f TextCursor) -> TagCursor -> f TagCursor
tagCursorTextCursorL = lens getter setter
  where
    getter = tagCursorTag
    setter tc textC = tagCursorModify (const textC) tc

tagCursorModify :: (TextCursor -> TextCursor) -> TagCursor -> TagCursor
tagCursorModify tfunc tc = tc'''
  where
    tct' = tfunc $ tagCursorTag tc
    tc' = tc {tagCursorTag = tct'}
    tcs =
        reverse (tagCursorPrevElemens tc') ++ [tc'] ++ tagCursorNextElemens tc'
    tags = map build tcs
    fc = tagCursorParent tc' & tagsCursorTagCursorsL .~ els
    els = tagElems fc $ map selectValue tags
    tc'' = els !! tagCursorIndex tc'
    tc''' = tc'' {tagCursorTag = tagCursorTag tc'' `reselectLike` tct'}

tagCursorInsert :: Char -> TagCursor -> TagCursor
tagCursorInsert c = tagCursorTextCursorL %~ textCursorInsert c

tagCursorAppend :: Char -> TagCursor -> TagCursor
tagCursorAppend c = tagCursorTextCursorL %~ textCursorAppend c

tagCursorRemove :: TagCursor -> Maybe TagCursor
tagCursorRemove = tagCursorTextCursorL textCursorRemove

tagCursorDelete :: TagCursor -> Maybe TagCursor
tagCursorDelete = tagCursorTextCursorL textCursorDelete

tagCursorLeft :: TagCursor -> Maybe TagCursor
tagCursorLeft = tagCursorTextCursorL textCursorSelectPrev

tagCursorRight :: TagCursor -> Maybe TagCursor
tagCursorRight = tagCursorTextCursorL textCursorSelectNext

tagCursorStart :: TagCursor -> TagCursor
tagCursorStart = tagCursorTextCursorL %~ textCursorSelectStart

tagCursorEnd :: TagCursor -> TagCursor
tagCursorEnd = tagCursorTextCursorL %~ textCursorSelectEnd

tagCursorSelectPrev :: TagCursor -> Maybe TagCursor
tagCursorSelectPrev tc =
    case tagCursorPrevElemens tc of
        [] -> Nothing
        (tc':_) -> Just tc'

tagCursorSelectNext :: TagCursor -> Maybe TagCursor
tagCursorSelectNext tc =
    case tagCursorNextElemens tc of
        [] -> Nothing
        (tc':_) -> Just tc'
