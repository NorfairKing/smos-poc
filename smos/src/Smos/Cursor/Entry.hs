{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.Entry
    ( EntryCursor
    , makeEntryCursor
    , entryCursor
    , foldEntrySel
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
    , HeaderCursor
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
    , ContentsCursor
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
    , StateCursor
    , makeStateCursor
    , stateCursor
    , stateCursorParent
    , stateCursorStateHistory
    , stateCursorClear
    , stateCursorSetState
    , TagsCursor
    , makeTagsCursor
    , tagsCursor
    , foldTagsSel
    , tagsCursorParent
    , tagsCursorTags
    , tagsCursorTagsL
    , tagsCursorSelectFirst
    , tagsCursorSelectLast
    , tagsCursorSetTags
    , tagsCursorInsertAt
    , tagsCursorInsertAtStart
    , tagsCursorAppendAtEnd
    , TagCursor
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
    , TimestampsCursor
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
import Cursor.Text
import Cursor.TextField
import Cursor.Tree

import Smos.Data
import Smos.View

data EntryCursor = EntryCursor
    { entryCursorParent :: TreeCursor EntryCursor
    , entryCursorHeader :: HeaderCursor
    , entryCursorContents :: Maybe ContentsCursor
    , entryCursorTimestamps :: TimestampsCursor
    , entryCursorProperties :: HashMap PropertyName PropertyValue
    , entryCursorState :: StateCursor
    , entryCursorTags :: TagsCursor
    , entryCursorLogbook :: Logbook
    }

instance Validity EntryCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show EntryCursor where
    show EntryCursor {..} =
        unlines
            ("[Tree]" :
             map
                 (" |- " ++)
                 [ "[Header]: " ++ show (build entryCursorHeader)
                 , "[Contents]: " ++ show (build <$> entryCursorContents)
                 , "[Timestamps]: " ++ show (build entryCursorTimestamps)
                 , show entryCursorProperties
                 , "[State]: " ++ show (build entryCursorState)
                 , "[Tags]: " ++ show (build entryCursorTags)
                 , show entryCursorLogbook
                 ])

instance Eq EntryCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild EntryCursor where
    type ReBuilding EntryCursor = ForestView EntryView
    rebuild = rebuild . entryCursorParent
    selection EntryCursor {..} = 0 : selection entryCursorParent

instance Build EntryCursor where
    type Building EntryCursor = EntryView
    build EntryCursor {..} =
        EntryView
        { entryViewHeader = build entryCursorHeader
        , entryViewContents = build <$> entryCursorContents
        , entryViewTimestamps = build entryCursorTimestamps
        , entryViewProperties = view entryCursorProperties
        , entryViewTodostate = build entryCursorState
        , entryViewTags = build entryCursorTags
        , entryViewLogbook = view entryCursorLogbook
        }

instance BuiltFrom EntryCursor EntryView where
    type Parent EntryCursor = TreeCursor EntryCursor
    makeWith = entryCursor

makeEntryCursor :: TreeCursor EntryCursor -> Entry -> EntryCursor
makeEntryCursor par e = entryCursor par $ view e

entryCursor :: TreeCursor EntryCursor -> EntryView -> EntryCursor
entryCursor par EntryView {..} = ec
  where
    ec =
        EntryCursor
        { entryCursorParent = par
        , entryCursorHeader = headerCursor ec entryViewHeader
        , entryCursorContents = contentsCursor ec <$> entryViewContents
        , entryCursorTimestamps = timestampsCursor ec entryViewTimestamps
        , entryCursorProperties = source entryViewProperties
        , entryCursorState = stateCursor ec entryViewTodostate
        , entryCursorTags = tagsCursor ec entryViewTags
        , entryCursorLogbook = source entryViewLogbook
        }

foldEntrySel ::
       (Maybe [Int] -> TodostateView -> a)
    -> (Maybe [Int] -> HeaderView -> b)
    -> (Maybe [Int] -> TagsView -> c)
    -> (Maybe [Int] -> TimestampsView -> d)
    -> (Maybe [Int] -> PropertiesView -> e)
    -> (Maybe [Int] -> ContentsView -> f)
    -> (Maybe [Int] -> LogbookView -> g)
    -> (a -> b -> c -> d -> e -> Maybe f -> g -> r)
    -> Maybe [Int]
    -> EntryView
    -> r
foldEntrySel tsFunc hFunc tgsFunc tssFunc psFunc cFunc lFunc combFunc msel EntryView {..} =
    combFunc
        (tsFunc (drillSel msel 0) entryViewTodostate)
        (hFunc (drillSel msel 1) entryViewHeader)
        (tgsFunc (drillSel msel 2) entryViewTags)
        (tssFunc (drillSel msel 3) entryViewTimestamps)
        (psFunc (drillSel msel 4) entryViewProperties)
        (cFunc (drillSel msel 5) <$> entryViewContents)
        (lFunc (drillSel msel 6) entryViewLogbook)

entryCursorHeaderL ::
       Functor f
    => (HeaderCursor -> f HeaderCursor)
    -> EntryCursor
    -> f EntryCursor
entryCursorHeaderL = lens getter setter
  where
    getter = entryCursorHeader
    setter ec hc = ec'
      where
        ec' =
            ec
            { entryCursorParent = entryCursorParent ec & treeCursorValueL .~ ec'
            , entryCursorHeader = hc
            , entryCursorContents =
                  (\ec_ -> ec_ {contentsCursorParent = ec'}) <$>
                  entryCursorContents ec
            , entryCursorState = (entryCursorState ec) {stateCursorParent = ec'}
            , entryCursorTags = (entryCursorTags ec) {tagsCursorParent = ec'}
            , entryCursorTimestamps =
                  (entryCursorTimestamps ec) {timestampsCursorParent = ec'}
            }

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
    lens (fmap (source . build) . entryCursorContents) setter
  where
    setter ec mc = ec'
      where
        ec' = ec & entryCursorContentsL .~ ((contentsCursor ec' . view) <$> mc)

data HeaderCursor = HeaderCursor
    { headerCursorParent :: EntryCursor
    , headerCursorHeader :: TextCursor
    }

instance Validity HeaderCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show HeaderCursor where
    show HeaderCursor {..} =
        unlines ["[Entry]", " |-" ++ show (rebuild headerCursorHeader)]

instance Eq HeaderCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild HeaderCursor where
    type ReBuilding HeaderCursor = ForestView EntryView
    rebuild = rebuild . headerCursorParent
    selection HeaderCursor {..} =
        selection headerCursorHeader ++ [1] ++ selection headerCursorParent

instance Build HeaderCursor where
    type Building HeaderCursor = HeaderView
    build = HeaderView . Header . rebuild . headerCursorHeader

makeHeaderCursor :: EntryCursor -> Header -> HeaderCursor
makeHeaderCursor par h = headerCursor par $ view h

headerCursor :: EntryCursor -> HeaderView -> HeaderCursor
headerCursor par h =
    HeaderCursor
    { headerCursorParent = par
    , headerCursorHeader = makeTextCursor $ headerText $ headerViewHeader h
    }

headerCursorTextCursorL ::
       Functor f
    => (TextCursor -> f TextCursor)
    -> HeaderCursor
    -> f HeaderCursor
headerCursorTextCursorL = lens getter setter
  where
    getter = headerCursorHeader
    setter hc tc = hc'
      where
        ec' = headerCursorParent hc & entryCursorHeaderL .~ hc'
        hc' = HeaderCursor {headerCursorParent = ec', headerCursorHeader = tc}

headerCursorSetHeader :: Header -> HeaderCursor -> HeaderCursor
headerCursorSetHeader h hc =
    hc & headerCursorTextCursorL .~ makeTextCursor (headerText h)

headerCursorHeaderL ::
       Functor f => (Header -> f Header) -> HeaderCursor -> f HeaderCursor
headerCursorHeaderL = lens (source . build) $ flip headerCursorSetHeader

headerCursorInsert :: Char -> HeaderCursor -> HeaderCursor
headerCursorInsert c = headerCursorTextCursorL %~ textCursorInsert c

headerCursorAppend :: Char -> HeaderCursor -> HeaderCursor
headerCursorAppend c = headerCursorTextCursorL %~ textCursorAppend c

headerCursorRemove :: HeaderCursor -> Maybe HeaderCursor
headerCursorRemove = headerCursorTextCursorL textCursorRemove

headerCursorDelete :: HeaderCursor -> Maybe HeaderCursor
headerCursorDelete = headerCursorTextCursorL textCursorDelete

headerCursorLeft :: HeaderCursor -> Maybe HeaderCursor
headerCursorLeft = headerCursorTextCursorL textCursorSelectPrev

headerCursorRight :: HeaderCursor -> Maybe HeaderCursor
headerCursorRight = headerCursorTextCursorL textCursorSelectNext

headerCursorStart :: HeaderCursor -> HeaderCursor
headerCursorStart = headerCursorTextCursorL %~ textCursorSelectStart

headerCursorEnd :: HeaderCursor -> HeaderCursor
headerCursorEnd = headerCursorTextCursorL %~ textCursorSelectEnd

data ContentsCursor = ContentsCursor
    { contentsCursorParent :: EntryCursor
    , contentsCursorContents :: TextFieldCursor
    }

instance Validity ContentsCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show ContentsCursor where
    show ContentsCursor {..} =
        unlines ["[Entry]", " |-" ++ show contentsCursorContents]

instance Eq ContentsCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild ContentsCursor where
    type ReBuilding ContentsCursor = ForestView EntryView
    rebuild = rebuild . contentsCursorParent
    selection ContentsCursor {..} =
        selection contentsCursorContents ++
        [5] ++ selection contentsCursorParent

instance Build ContentsCursor where
    type Building ContentsCursor = ContentsView
    build = ContentsView . Contents . rebuild . contentsCursorContents

emptyContentsCursor :: EntryCursor -> ContentsCursor
emptyContentsCursor ec = makeContentsCursor ec $ Contents T.empty

makeContentsCursor :: EntryCursor -> Contents -> ContentsCursor
makeContentsCursor ec cts = contentsCursor ec $ view cts

contentsCursor :: EntryCursor -> ContentsView -> ContentsCursor
contentsCursor ec ContentsView {..} =
    ContentsCursor
    { contentsCursorParent = ec
    , contentsCursorContents =
          makeTextFieldCursor $ contentsText contentsViewContents
    }

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
contentsCursorContentsL = lens (source . build) $ flip contentsCursorSetContents

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

data StateCursor = StateCursor
    { stateCursorParent :: EntryCursor
    , stateCursorStateHistory :: StateHistory
    }

instance Validity StateCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show StateCursor where
    show StateCursor {..} =
        unlines ["[Entry]", " |-" ++ show stateCursorStateHistory]

instance Eq StateCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild StateCursor where
    type ReBuilding StateCursor = ForestView EntryView
    rebuild = rebuild . stateCursorParent
    selection StateCursor {..} = 0 : selection stateCursorParent

instance Build StateCursor where
    type Building StateCursor = TodostateView
    build = TodostateView . stateCursorStateHistory

makeStateCursor :: EntryCursor -> StateHistory -> StateCursor
makeStateCursor ec sh = stateCursor ec $ view sh

stateCursor :: EntryCursor -> TodostateView -> StateCursor
stateCursor ec tsv =
    StateCursor {stateCursorParent = ec, stateCursorStateHistory = source tsv}

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

(&&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(&&&) op1 op2 a b = op1 a b && op2 a b

data TagsCursor = TagsCursor
    { tagsCursorParent :: EntryCursor
    , tagsCursorTags :: [TagCursor]
    }

instance Validity TagsCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show TagsCursor where
    show TagsCursor {..} = unlines ["[Entry]", " |-" ++ show tagsCursorTags]

instance Eq TagsCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild TagsCursor where
    type ReBuilding TagsCursor = ForestView EntryView
    rebuild = rebuild . tagsCursorParent
    selection TagsCursor {..} = 2 : selection tagsCursorParent

instance Build TagsCursor where
    type Building TagsCursor = TagsView
    build = TagsView . map build . tagsCursorTags

makeTagsCursor :: EntryCursor -> [Tag] -> TagsCursor
makeTagsCursor ec tags = tagsCursor ec $ view tags

tagsCursor :: EntryCursor -> TagsView -> TagsCursor
tagsCursor ec tags = tsc
  where
    tsc =
        TagsCursor
        { tagsCursorParent = ec
        , tagsCursorTags = tagElems tsc $ tagsViewTags tags
        }

foldTagsSel ::
       (Maybe [Int] -> TagView -> q)
    -> ([(Int, q)] -> r)
    -> Maybe [Int]
    -> TagsView
    -> r
foldTagsSel tFunc combFunc msel tgs =
    combFunc $
    flip map (zip [0 ..] $ tagsViewTags tgs) $ \(ix_, t) ->
        (ix_, tFunc (drillSel msel ix_) t)

tagElems :: TagsCursor -> [TagView] -> [TagCursor]
tagElems tsc sts = tcs
  where
    tcs = zipWith tc [0 ..] sts
    tc i t = cur
      where
        cur =
            TagCursor
            { tagCursorParent = tsc
            , tagCursorPrevElemens =
                  reverse $ filter ((< i) . tagCursorIndex) tcs
            , tagCursorNextElemens = filter ((> i) . tagCursorIndex) tcs
            , tagCursorIndex = i
            , tagCursorTag = makeTextCursor $ tagViewText t
            }

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
tagsCursorTagsL = lens (source . build) setter
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
             map build (prevs els) ++ [tagView newTag] ++ map build (nexts els))
    ffilter rel = filter ((`rel` ix_) . tagCursorIndex)
    prevs = ffilter (<)
    nexts = ffilter (>=)

tagsCursorInsertAtStart :: Tag -> TagsCursor -> TagsCursor
tagsCursorInsertAtStart = tagsCursorInsertAt 0

tagsCursorAppendAtEnd :: Tag -> TagsCursor -> TagsCursor
tagsCursorAppendAtEnd t fc =
    tagsCursorInsertAt (length $ tagsCursorTags fc) t fc

data TagCursor = TagCursor
    { tagCursorParent :: TagsCursor
    , tagCursorPrevElemens :: [TagCursor]
    , tagCursorNextElemens :: [TagCursor]
    , tagCursorIndex :: Int
    , tagCursorTag :: TextCursor
    }

instance Validity TagCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show TagCursor where
    show TagCursor {..} = unlines ["[Tags]", " |-" ++ show tagCursorTag]

instance Eq TagCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild TagCursor where
    type ReBuilding TagCursor = ForestView EntryView
    rebuild = rebuild . tagCursorParent
    selection TagCursor {..} =
        selection tagCursorTag ++
        [length tagCursorPrevElemens] ++ selection tagCursorParent

instance Build TagCursor where
    type Building TagCursor = TagView
    build TagCursor {..} = TagView {tagViewText = rebuild tagCursorTag}

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
    els = tagElems fc tags
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

data TimestampsCursor = TimestampsCursor
    { timestampsCursorParent :: EntryCursor
    , timestampsCursorTimestamps :: HashMap TimestampName UTCTime
    }

instance Validity TimestampsCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show TimestampsCursor where
    show TimestampsCursor {..} =
        unlines ["[Timestamps]", " |-" ++ show timestampsCursorTimestamps]

instance Eq TimestampsCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild TimestampsCursor where
    type ReBuilding TimestampsCursor = ForestView EntryView
    rebuild = rebuild . timestampsCursorParent
    selection TimestampsCursor {..} = 3 : selection timestampsCursorParent

instance Build TimestampsCursor where
    type Building TimestampsCursor = TimestampsView
    build = TimestampsView . timestampsCursorTimestamps

makeTimestampsCursor ::
       EntryCursor -> HashMap TimestampName UTCTime -> TimestampsCursor
makeTimestampsCursor ec hm = timestampsCursor ec $ view hm

timestampsCursor :: EntryCursor -> TimestampsView -> TimestampsCursor
timestampsCursor ec tsv =
    TimestampsCursor
    {timestampsCursorParent = ec, timestampsCursorTimestamps = source tsv}

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
