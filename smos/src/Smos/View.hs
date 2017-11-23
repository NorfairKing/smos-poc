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

import Smos.Data

import Cursor.Class
import Cursor.Select
import Cursor.Text
import Cursor.TextField
import Cursor.Tree

data EntryView = EntryView
    { entryViewTodostate :: Select TodostateView
    , entryViewTags :: Select TagsView
    , entryViewHeader :: Select HeaderView
    , entryViewContents :: Maybe (Select ContentsView)
    , entryViewTimestamps :: Select TimestampsView
    , entryViewProperties :: Select PropertiesView
    , entryViewLogbook :: Select LogbookView
    } deriving (Show, Eq, Generic)

instance Validity EntryView

instance View EntryView where
    type Source EntryView = Entry
    source EntryView {..} =
        Entry
        { entryStateHistory = source $ selectValue entryViewTodostate
        , entryHeader = source $ selectValue entryViewHeader
        , entryTags = source $ selectValue entryViewTags
        , entryContents = (source . selectValue) <$> entryViewContents
        , entryTimestamps = source $ selectValue entryViewTimestamps
        , entryProperties = source $ selectValue entryViewProperties
        , entryLogbook = source $ selectValue entryViewLogbook
        }
    view Entry {..} =
        EntryView
        { entryViewTodostate = select $ view entryStateHistory
        , entryViewHeader = select $ view entryHeader
        , entryViewTags = select $ view entryTags
        , entryViewContents = (select . view) <$> entryContents
        , entryViewTimestamps = select $ view entryTimestamps
        , entryViewProperties = select $ view entryProperties
        , entryViewLogbook = select $ view entryLogbook
        }

instance Selectable EntryView where
    applySelection msel EntryView {..} =
        EntryView
        { entryViewTodostate = drillStop 0 msel entryViewTodostate
        , entryViewHeader = drillStop 1 msel entryViewHeader
        , entryViewTags = drillApply 2 msel entryViewTags
        , entryViewContents = drillStop 3 msel <$> entryViewContents
        , entryViewTimestamps = drillStop 4 msel entryViewTimestamps
        , entryViewProperties = drillStop 5 msel entryViewProperties
        , entryViewLogbook = drillStop 6 msel entryViewLogbook
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
    { headerViewHeader :: TextView
    } deriving (Show, Eq, Generic)

instance Validity HeaderView

instance View HeaderView where
    type Source HeaderView = Header
    source = Header . source . headerViewHeader
    view = HeaderView . view . headerText

newtype TagsView = TagsView
    { tagsViewTags :: [Select TagView]
    } deriving (Show, Eq, Generic)

instance Validity TagsView

instance View TagsView where
    type Source TagsView = [Tag]
    source = filter isValid . map (Tag . source . selectValue) . tagsViewTags
    view = TagsView . map (select . tagView)

instance Selectable TagsView where
    applySelection msel = TagsView . drillStopList msel . tagsViewTags

newtype TagView = TagView
    { tagViewText :: TextView
    } deriving (Show, Eq, Generic)

instance Validity TagView

instance View TagView where
    type Source TagView = Text
    source = source . tagViewText
    view = TagView . view

tagView :: Tag -> TagView
tagView t = TagView {tagViewText = view $ tagText t}

newtype ContentsView = ContentsView
    { contentsViewContents :: TextFieldView
    } deriving (Show, Eq, Generic)

instance Validity ContentsView

instance View ContentsView where
    type Source ContentsView = Contents
    source = Contents . source . contentsViewContents
    view = ContentsView . view . contentsText

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
