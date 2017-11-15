{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.View
    ( View(..)
    , ForestView(..)
    , TreeView(..)
    , EntryView(..)
    , TodostateView(..)
    , HeaderView(..)
    , TagsView(..)
    , TagView(..)
    , tagView
    , ContentsView(..)
    , TimestampsView(..)
    , PropertiesView(..)
    , LogbookView(..)
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import Data.Time

-- import Data.Tree
import Smos.Data

import Cursor.Class
import Cursor.Tree

data EntryView = EntryView
    { entryViewTodostate :: TodostateView
    , entryViewTags :: TagsView
    , entryViewHeader :: HeaderView
    , entryViewContents :: Maybe ContentsView
    , entryViewTimestamps :: TimestampsView
    , entryViewProperties :: PropertiesView
    , entryViewLogbook :: LogbookView
    } deriving (Show, Eq, Generic)

instance Validity EntryView

instance View EntryView where
    type Source EntryView = Entry
    source EntryView {..} =
        Entry
        { entryStateHistory = source entryViewTodostate
        , entryHeader = source entryViewHeader
        , entryTags = source entryViewTags
        , entryContents = source <$> entryViewContents
        , entryTimestamps = source entryViewTimestamps
        , entryProperties = source entryViewProperties
        , entryLogbook = source entryViewLogbook
        }
    view Entry {..} =
        EntryView
        { entryViewTodostate = view entryStateHistory
        , entryViewHeader = view entryHeader
        , entryViewTags = view entryTags
        , entryViewContents = view <$> entryContents
        , entryViewTimestamps = view entryTimestamps
        , entryViewProperties = view entryProperties
        , entryViewLogbook = view entryLogbook
        }

newtype TodostateView = TodostateView
    { todostateViewTodostate :: StateHistory
    } deriving (Show, Eq, Generic)

instance Validity TodostateView

instance View TodostateView where
    type Source TodostateView = StateHistory
    source = todostateViewTodostate
    view = TodostateView

newtype HeaderView = HeaderView
    { headerViewHeader :: Header
    } deriving (Show, Eq, Generic)

instance Validity HeaderView

instance View HeaderView where
    type Source HeaderView = Header
    source = headerViewHeader
    view = HeaderView

newtype TagsView = TagsView
    { tagsViewTags :: [TagView]
    } deriving (Show, Eq, Generic)

instance Validity TagsView

instance View TagsView where
    type Source TagsView = [Tag]
    source = filter isValid . map (Tag . source) . tagsViewTags
    view = TagsView . map tagView

newtype TagView = TagView
    { tagViewText :: Text
    } deriving (Show, Eq, Generic)

instance Validity TagView

instance View TagView where
    type Source TagView = Text
    source = tagViewText
    view = TagView

tagView :: Tag -> TagView
tagView t = TagView {tagViewText = tagText t}

newtype ContentsView = ContentsView
    { contentsViewContents :: Contents
    } deriving (Show, Eq, Generic)

instance Validity ContentsView

instance View ContentsView where
    type Source ContentsView = Contents
    source = contentsViewContents
    view = ContentsView

newtype TimestampsView = TimestampsView
    { timestampsViewTimestamps :: HashMap TimestampName UTCTime
    } deriving (Show, Eq, Generic)

instance Validity TimestampsView

instance View TimestampsView where
    type Source TimestampsView = HashMap TimestampName UTCTime
    source = timestampsViewTimestamps
    view = TimestampsView

newtype PropertiesView = PropertiesView
    { propertiesViewProperties :: HashMap PropertyName PropertyValue
    } deriving (Show, Eq, Generic)

instance Validity PropertiesView

instance View PropertiesView where
    type Source PropertiesView = HashMap PropertyName PropertyValue
    source = propertiesViewProperties
    view = PropertiesView

newtype LogbookView = LogbookView
    { logbookViewLogbook :: Logbook
    } deriving (Show, Eq, Generic)

instance Validity LogbookView

instance View LogbookView where
    type Source LogbookView = Logbook
    source = logbookViewLogbook
    view = LogbookView
