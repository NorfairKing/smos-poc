{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.Types
    ( EntryCursor(..)
    , entryCursor
    , HeaderCursor(..)
    , headerCursor
    , ContentsCursor(..)
    , contentsCursor
    , StateCursor(..)
    , stateCursor
    , TagsCursor(..)
    , tagsCursor
    , TagCursor(..)
    , tagCursor
    , TimestampsCursor(..)
    , timestampsCursor
    , KeyCursor(..)
    , ValueCursor(..)
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import Data.Time

import Cursor.Class
import Cursor.ListElem
import Cursor.Select
import Cursor.Text
import Cursor.TextField
import Cursor.Tree

import Smos.Cursor.FuzzyTime

import Smos.Data
import Smos.View

data EntryCursor = EntryCursor
    { entryCursorParent :: TreeCursor EntryCursor
    , entryCursorHeader :: HeaderCursor
    , entryCursorContents :: Maybe ContentsCursor
    , entryCursorTimestamps :: TimestampsCursor
    , entryCursorProperties :: HashMap PropertyName PropertyValue
    , entryCursorState :: StateCursor
    , entryCursorTags :: Maybe TagsCursor
    , entryCursorLogbook :: Logbook
    } deriving (Generic)

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
                 , "[Tags]: " ++ show (build <$> entryCursorTags)
                 , show entryCursorLogbook
                 ])

instance Eq EntryCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild EntryCursor where
    type ReBuilding EntryCursor = Select (ForestView EntryView)
    rebuild = rebuild . entryCursorParent
    selection EntryCursor {..} = 0 : selection entryCursorParent

instance Build EntryCursor where
    type Building EntryCursor = EntryView
    build EntryCursor {..} =
        EntryView
        { entryViewHeader = build entryCursorHeader
        , entryViewContents = build <$> entryCursorContents
        , entryViewTimestamps = build entryCursorTimestamps
        , entryViewProperties = select $ view entryCursorProperties
        , entryViewTodostate = build entryCursorState
        , entryViewTags = build <$> entryCursorTags
        , entryViewLogbook = select $ view entryCursorLogbook
        }

instance BuiltFrom EntryCursor EntryView where
    type Parent EntryCursor = TreeCursor EntryCursor
    makeWith = entryCursor

entryCursor :: TreeCursor EntryCursor -> EntryView -> EntryCursor
entryCursor par EntryView {..} = ec
  where
    ec =
        EntryCursor
        { entryCursorParent = par
        , entryCursorHeader = headerCursor ec $ selectValue entryViewHeader
        , entryCursorContents =
              (contentsCursor ec . selectValue) <$> entryViewContents
        , entryCursorTimestamps =
              timestampsCursor ec $ selectValue entryViewTimestamps
        , entryCursorProperties = source $ selectValue entryViewProperties
        , entryCursorState = stateCursor ec $ selectValue entryViewTodostate
        , entryCursorTags = (tagsCursor ec . selectValue) <$> entryViewTags
        , entryCursorLogbook = source $ selectValue entryViewLogbook
        }

data HeaderCursor = HeaderCursor
    { headerCursorParent :: EntryCursor
    , headerCursorHeader :: TextCursor
    } deriving (Generic)

instance Validity HeaderCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show HeaderCursor where
    show HeaderCursor {..} =
        unlines ["[Entry]", " |-" ++ show (rebuild headerCursorHeader)]

instance Eq HeaderCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild HeaderCursor where
    type ReBuilding HeaderCursor = Select (ForestView EntryView)
    rebuild = rebuild . headerCursorParent
    selection HeaderCursor {..} =
        selection headerCursorHeader ++ [1] ++ selection headerCursorParent

instance Build HeaderCursor where
    type Building HeaderCursor = Select HeaderView
    build = select . HeaderView . rebuild . headerCursorHeader

headerCursor :: EntryCursor -> HeaderView -> HeaderCursor
headerCursor par h =
    HeaderCursor
    { headerCursorParent = par
    , headerCursorHeader = makeTextCursor $ headerText $ source h
    }

data ContentsCursor = ContentsCursor
    { contentsCursorParent :: EntryCursor
    , contentsCursorContents :: TextFieldCursor
    } deriving (Generic)

instance Validity ContentsCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show ContentsCursor where
    show ContentsCursor {..} =
        unlines ["[Entry]", " |-" ++ show contentsCursorContents]

instance Eq ContentsCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild ContentsCursor where
    type ReBuilding ContentsCursor = Select (ForestView EntryView)
    rebuild = rebuild . contentsCursorParent
    selection ContentsCursor {..} =
        selection contentsCursorContents ++
        [5] ++ selection contentsCursorParent

instance Build ContentsCursor where
    type Building ContentsCursor = Select ContentsView
    build = select . ContentsView . rebuild . contentsCursorContents

contentsCursor :: EntryCursor -> ContentsView -> ContentsCursor
contentsCursor ec ContentsView {..} =
    ContentsCursor
    { contentsCursorParent = ec
    , contentsCursorContents = makeTextFieldCursor $ source contentsViewContents
    }

data StateCursor = StateCursor
    { stateCursorParent :: EntryCursor
    , stateCursorStateHistory :: StateHistory
    } deriving (Generic)

instance Validity StateCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show StateCursor where
    show StateCursor {..} =
        unlines ["[Entry]", " |-" ++ show stateCursorStateHistory]

instance Eq StateCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild StateCursor where
    type ReBuilding StateCursor = Select (ForestView EntryView)
    rebuild = rebuild . stateCursorParent
    selection StateCursor {..} = 0 : selection stateCursorParent

instance Build StateCursor where
    type Building StateCursor = Select TodostateView
    build = select . TodostateView . stateCursorStateHistory

stateCursor :: EntryCursor -> TodostateView -> StateCursor
stateCursor ec tsv =
    StateCursor {stateCursorParent = ec, stateCursorStateHistory = source tsv}

data TagsCursor = TagsCursor
    { tagsCursorParent :: EntryCursor
    , tagsCursorTags :: ListElemCursor TagCursor
    } deriving (Generic)

instance Validity TagsCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show TagsCursor where
    show TagsCursor {..} = unlines ["[Entry]", " |-" ++ show tagsCursorTags]

instance Eq TagsCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild TagsCursor where
    type ReBuilding TagsCursor = Select (ForestView EntryView)
    rebuild = rebuild . tagsCursorParent
    selection TagsCursor {..} =
        selection tagsCursorTags ++ 2 : selection tagsCursorParent

instance Build TagsCursor where
    type Building TagsCursor = Select TagsView
    build = select . TagsView . fmap build . rebuild . tagsCursorTags

tagsCursor :: EntryCursor -> TagsView -> TagsCursor
tagsCursor ec tags = tsc
  where
    tsc =
        TagsCursor
        { tagsCursorParent = ec
        , tagsCursorTags =
              makeListElemCursor $ fmap tagCursor $ source $ tagsViewTags tags
        }

newtype TagCursor = TagCursor
    { tagCursorTag :: TextCursor
    } deriving (Generic)

instance Validity TagCursor where
    isValid a = isValid (build a) -- && isValid (rebuild a)
    validate a = build a <?!> "build" -- <> (rebuild a <?!> "rebuild")

instance Show TagCursor where
    show TagCursor {..} = unlines ["[Tags]", " |-" ++ show tagCursorTag]

instance Eq TagCursor where
    (==) = (==) `on` build -- &&& ((==) `on` rebuild)

instance Rebuild TagCursor where
    type ReBuilding TagCursor = TagView
    rebuild = TagView . rebuild . tagCursorTag
    selection TagCursor {..} = selection tagCursorTag

instance Build TagCursor where
    type Building TagCursor = TagView
    build TagCursor {..} = TagView {tagViewText = rebuild tagCursorTag}

tagCursor :: TagView -> TagCursor
tagCursor = TagCursor . makeTextCursor . source . tagViewText

data TimestampsCursor = TimestampsCursor
    { timestampsCursorParent :: EntryCursor
    , timestampsCursorTimestamps :: HashMap TimestampName UTCTime
    } deriving (Generic)

instance Validity TimestampsCursor where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Show TimestampsCursor where
    show TimestampsCursor {..} =
        unlines ["[Timestamps]", " |-" ++ show timestampsCursorTimestamps]

instance Eq TimestampsCursor where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Rebuild TimestampsCursor where
    type ReBuilding TimestampsCursor = Select (ForestView EntryView)
    rebuild = rebuild . timestampsCursorParent
    selection TimestampsCursor {..} = 3 : selection timestampsCursorParent

instance Build TimestampsCursor where
    type Building TimestampsCursor = Select TimestampsView
    build = select . TimestampsView . timestampsCursorTimestamps

timestampsCursor :: EntryCursor -> TimestampsView -> TimestampsCursor
timestampsCursor ec tsv =
    TimestampsCursor
    {timestampsCursorParent = ec, timestampsCursorTimestamps = source tsv}

data KeyCursor = KeyCursor
    { keyCursorKey :: TextCursor
    , keyCursorValue :: ValueCursor
    } deriving (Show, Eq, Generic)

data ValueCursor = ValueCursor
    { valueCursorKey :: KeyCursor
    , valueCursorValue :: FuzzyTimeCursor
    } deriving (Show, Eq, Generic)

(&&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(&&&) op1 op2 a b = op1 a b && op2 a b
