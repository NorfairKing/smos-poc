{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.Entry
    ( EntryCursor
    , makeEntryCursor
    , entryCursor
    , entryCursorParent
    , entryCursorHeader
    , entryCursorContents
    , entryCursorState
    , entryCursorTags
    , entryCursorTimestamps
    , entryCursorHeaderL
    , entryCursorContentsL
    , entryCursorStateL
    , entryCursorTagsL
    , entryCursorTimestampsL
    , entryCursorLogbookL
    , entryCursorPropertiesL
    , entryCursorClockIn
    , entryCursorContentsML
    , makeHeaderCursor
    , headerCursor
    , headerCursorParent
    , headerCursorHeader
    , headerCursorTextCursorL
    , headerCursorSetHeader
    , headerCursorHeaderL
    , headerCursorInsert
    , headerCursorAppend
    , headerCursorRemove
    , headerCursorDelete
    , headerCursorLeft
    , headerCursorRight
    , headerCursorStart
    , headerCursorEnd
    , emptyContentsCursor
    , makeContentsCursor
    , contentsCursor
    , contentsCursorParent
    , contentsCursorContents
    , contentsCursorContentsL
    , contentsCursorSetContents
    , contentsCursorTextFieldL
    , contentsCursorInsert
    , contentsCursorAppend
    , contentsCursorNewline
    , contentsCursorRemove
    , contentsCursorDelete
    , contentsCursorLeft
    , contentsCursorRight
    , contentsCursorUp
    , contentsCursorDown
    , contentsCursorStart
    , contentsCursorEnd
    , makeStateCursor
    , stateCursor
    , stateCursorParent
    , stateCursorStateHistory
    , stateCursorClear
    , stateCursorSetState
    , makeTagsCursor
    , tagsCursor
    , tagsCursorParent
    , tagsCursorTags
    , tagsCursorTagsL
    , tagsCursorSelectFirst
    , tagsCursorSelectLast
    , tagsCursorSetTags
    , tagsCursorInsertAt
    , tagsCursorInsertAtStart
    , tagsCursorAppendAtEnd
    , tagCursorParent
    , tagCursorIndex
    , tagCursorPrevElemens
    , tagCursorNextElemens
    , tagCursorTag
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
    , makeTimestampsCursor
    , timestampsCursor
    , timestampsCursorParent
    , timestampsCursorTimestamps
    , timestampsCursorSetTimestamps
    , timestampsCursorTimestampsL
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import qualified Data.Text as T
import Data.Time

import Lens.Micro

import Cursor.Class
import Cursor.Select
import Cursor.Text
import Cursor.TextField
import Cursor.Tree

import Smos.Cursor.Entry.Header
import Smos.Cursor.Header
import Smos.Cursor.Types
import Smos.Data
import Smos.View

makeEntryCursor :: TreeCursor EntryCursor -> Entry -> EntryCursor
makeEntryCursor par e = entryCursor par $ view e

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
                  (entryCursorTimestamps ec) {timestampsCursorParent = ec'}
            }

entryCursorStateL ::
       Functor f
    => (StateCursor -> f StateCursor)
    -> EntryCursor
    -> f EntryCursor
entryCursorStateL = lens getter setter
  where
    getter = entryCursorState
    setter ec hc = ec'
      where
        ec' =
            ec
            { entryCursorParent = entryCursorParent ec & treeCursorValueL .~ ec'
            , entryCursorState = hc
            , entryCursorHeader =
                  (entryCursorHeader ec) {headerCursorParent = ec'}
            , entryCursorContents =
                  (\ec_ -> ec_ {contentsCursorParent = ec'}) <$>
                  entryCursorContents ec
            , entryCursorTags = (entryCursorTags ec) {tagsCursorParent = ec'}
            , entryCursorTimestamps =
                  (entryCursorTimestamps ec) {timestampsCursorParent = ec'}
            }

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
                  (entryCursorTimestamps ec) {timestampsCursorParent = ec'}
            }

entryCursorTimestampsL ::
       Functor f
    => (TimestampsCursor -> f TimestampsCursor)
    -> EntryCursor
    -> f EntryCursor
entryCursorTimestampsL = lens getter setter
  where
    getter = entryCursorTimestamps
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
            , entryCursorTags = (entryCursorTags ec) {tagsCursorParent = ec'}
            , entryCursorTimestamps = ts
            }

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
            , entryCursorTags = (entryCursorTags ec) {tagsCursorParent = ec'}
            , entryCursorTimestamps =
                  (entryCursorTimestamps ec) {timestampsCursorParent = ec'}
            , entryCursorLogbook = lb
            }

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
            , entryCursorTags = (entryCursorTags ec) {tagsCursorParent = ec'}
            , entryCursorTimestamps =
                  (entryCursorTimestamps ec) {timestampsCursorParent = ec'}
            , entryCursorProperties = ps
            }

entryCursorClockIn :: UTCTime -> EntryCursor -> Maybe EntryCursor
entryCursorClockIn now = entryCursorLogbookL $ clockInAt now

entryCursorContentsML :: Lens' EntryCursor (Maybe Contents)
entryCursorContentsML =
    lens (fmap (source . selectValue . build) . entryCursorContents) setter
  where
    setter ec mc = ec'
      where
        ec' = ec & entryCursorContentsL .~ ((contentsCursor ec' . view) <$> mc)

emptyContentsCursor :: EntryCursor -> ContentsCursor
emptyContentsCursor ec = makeContentsCursor ec $ Contents T.empty

makeContentsCursor :: EntryCursor -> Contents -> ContentsCursor
makeContentsCursor ec cts = contentsCursor ec $ view cts

contentsCursorTextFieldL ::
       Functor f
    => (TextFieldCursor -> f TextFieldCursor)
    -> ContentsCursor
    -> f ContentsCursor
contentsCursorTextFieldL = lens getter setter
  where
    getter = contentsCursorContents
    setter cc tfc = cc'
      where
        ec' = contentsCursorParent cc & entryCursorContentsL .~ Just cc'
        cc' = cc {contentsCursorParent = ec', contentsCursorContents = tfc}

contentsCursorSetContents :: Contents -> ContentsCursor -> ContentsCursor
contentsCursorSetContents cs =
    contentsCursorTextFieldL .~ makeTextFieldCursor (contentsText cs)

contentsCursorContentsL :: Lens' ContentsCursor Contents
contentsCursorContentsL =
    lens (source . selectValue . build) $ flip contentsCursorSetContents

contentsCursorInsert :: Char -> ContentsCursor -> ContentsCursor
contentsCursorInsert c = contentsCursorTextFieldL %~ textFieldCursorInsert c

contentsCursorAppend :: Char -> ContentsCursor -> ContentsCursor
contentsCursorAppend c = contentsCursorTextFieldL %~ textFieldCursorAppend c

contentsCursorNewline :: ContentsCursor -> ContentsCursor
contentsCursorNewline = contentsCursorTextFieldL %~ textFieldCursorNewline

contentsCursorRemove :: ContentsCursor -> Maybe ContentsCursor
contentsCursorRemove = contentsCursorTextFieldL textFieldCursorRemove

contentsCursorDelete :: ContentsCursor -> Maybe ContentsCursor
contentsCursorDelete = contentsCursorTextFieldL textFieldCursorDelete

contentsCursorLeft :: ContentsCursor -> Maybe ContentsCursor
contentsCursorLeft = contentsCursorTextFieldL textFieldCursorSelectPrev

contentsCursorRight :: ContentsCursor -> Maybe ContentsCursor
contentsCursorRight = contentsCursorTextFieldL textFieldCursorSelectNext

contentsCursorUp :: ContentsCursor -> Maybe ContentsCursor
contentsCursorUp = contentsCursorTextFieldL textFieldCursorSelectUp

contentsCursorDown :: ContentsCursor -> Maybe ContentsCursor
contentsCursorDown = contentsCursorTextFieldL textFieldCursorSelectDown

contentsCursorStart :: ContentsCursor -> ContentsCursor
contentsCursorStart = contentsCursorTextFieldL %~ textFieldCursorSelectStart

contentsCursorEnd :: ContentsCursor -> ContentsCursor
contentsCursorEnd = contentsCursorTextFieldL %~ textFieldCursorSelectEnd

makeStateCursor :: EntryCursor -> StateHistory -> StateCursor
makeStateCursor ec sh = stateCursor ec $ view sh

stateCursorStateL ::
       Functor f
    => UTCTime
    -> (Maybe TodoState -> f (Maybe TodoState))
    -> StateCursor
    -> f StateCursor
stateCursorStateL now = lens getter setter
  where
    getter = stateHistoryState . stateCursorStateHistory
    setter sc mts = sc'
      where
        sc' =
            StateCursor
            { stateCursorParent =
                  stateCursorParent sc & entryCursorStateL .~ sc'
            , stateCursorStateHistory =
                  stateHistorySetState now mts $ stateCursorStateHistory sc
            }

stateCursorClear :: UTCTime -> StateCursor -> StateCursor
stateCursorClear now sc = sc & stateCursorStateL now .~ Nothing

stateCursorSetState :: UTCTime -> TodoState -> StateCursor -> StateCursor
stateCursorSetState now ts sc = sc & stateCursorStateL now .~ Just ts

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

makeTimestampsCursor ::
       EntryCursor -> HashMap TimestampName UTCTime -> TimestampsCursor
makeTimestampsCursor ec hm = timestampsCursor ec $ view hm

timestampsCursorSetTimestamps ::
       HashMap TimestampName UTCTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorSetTimestamps ts = timestampsCursorTimestampsL .~ ts

timestampsCursorTimestampsL ::
       Functor f
    => (HashMap TimestampName UTCTime -> f (HashMap TimestampName UTCTime))
    -> TimestampsCursor
    -> f TimestampsCursor
timestampsCursorTimestampsL = lens getter setter
  where
    getter = timestampsCursorTimestamps
    setter tsc tss = tsc'
      where
        ec' = timestampsCursorParent tsc & entryCursorTimestampsL .~ tsc'
        tsc' =
            tsc {timestampsCursorParent = ec', timestampsCursorTimestamps = tss}
