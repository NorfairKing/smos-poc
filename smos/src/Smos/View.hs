{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , TimestampNameView(..)
    , timestampNameView
    , TimestampView(..)
    , PropertiesView(..)
    , LogbookView(..)
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Lens.Micro

import Smos.Data

import Cursor.Class
import Cursor.ListElem
import Cursor.Map
import Cursor.Select
import Cursor.Text
import Cursor.TextField
import Cursor.Tree

data EntryView = EntryView
    { entryViewTodostate :: Select TodostateView
    , entryViewTags :: Maybe (Select TagsView)
    , entryViewHeader :: Select HeaderView
    , entryViewContents :: Maybe (Select ContentsView)
    , entryViewTimestamps :: Maybe (Select TimestampsView)
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
        , entryTags = maybe [] (NE.toList . source . selectValue) entryViewTags
        , entryContents = (source . selectValue) <$> entryViewContents
        , entryTimestamps =
              case entryViewTimestamps of
                  Nothing -> HM.empty
                  Just stsv ->
                      rebuildHashmapFromMapView $
                      (mapViewListElemViewKeysT %~ (TimestampName . source)) $
                      (mapViewListElemViewValuesT %~ source) $
                      timestampsViewTimestamps $ selectValue stsv
        , entryProperties = source $ selectValue entryViewProperties
        , entryLogbook = source $ selectValue entryViewLogbook
        }
    view Entry {..} =
        EntryView
        { entryViewTodostate = select $ view entryStateHistory
        , entryViewHeader = select $ view entryHeader
        , entryViewTags = fmap (select . view) . NE.nonEmpty $ entryTags
        , entryViewContents = (select . view) <$> entryContents
        , entryViewTimestamps =
              fmap (select . view) . NE.nonEmpty . HM.toList $ entryTimestamps
        , entryViewProperties = select $ view entryProperties
        , entryViewLogbook = select $ view entryLogbook
        }

instance Selectable EntryView where
    applySelection msel EntryView {..} =
        EntryView
        { entryViewTodostate = drillPrefixStop 0 msel entryViewTodostate
        , entryViewHeader = drillPrefixApply 1 msel entryViewHeader
        , entryViewTags = drillPrefixApply 2 msel <$> entryViewTags
        , entryViewTimestamps = drillPrefixApply 3 msel <$> entryViewTimestamps
        , entryViewProperties = drillStop 4 msel entryViewProperties
        , entryViewContents = drillPrefixApply 5 msel <$> entryViewContents
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

instance Selectable HeaderView where
    applySelection msel = HeaderView . applySelection msel . headerViewHeader

newtype TagsView = TagsView
    { tagsViewTags :: ListElemView TagView
    } deriving (Show, Eq, Generic)

instance Validity TagsView

instance View TagsView where
    type Source TagsView = NonEmpty Tag
    source = fmap (Tag . source) . source . tagsViewTags
    view = TagsView . fmap tagView . view

instance Selectable TagsView where
    applySelection msel = TagsView . applySelection msel . tagsViewTags

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

instance Selectable TagView where
    applySelection msel = TagView . applySelection msel . tagViewText

newtype ContentsView = ContentsView
    { contentsViewContents :: TextFieldView
    } deriving (Show, Eq, Generic)

instance Validity ContentsView

instance View ContentsView where
    type Source ContentsView = Contents
    source = Contents . source . contentsViewContents
    view = ContentsView . view . contentsText

instance Selectable ContentsView where
    applySelection msel =
        ContentsView . applySelection msel . contentsViewContents

newtype TimestampsView = TimestampsView
    { timestampsViewTimestamps :: MapView TimestampNameView TimestampView
    } deriving (Show, Eq, Generic)

instance Validity TimestampsView

instance View TimestampsView where
    type Source TimestampsView = NonEmpty (TimestampName, Timestamp)
    source =
        source .
        (mapViewListElemViewKeysT %~ (TimestampName . source)) .
        (mapViewListElemViewValuesT %~ source) . timestampsViewTimestamps
    view =
        TimestampsView .
        (mapViewListElemViewKeysT %~ (view . timestampNameText)) .
        (mapViewListElemViewValuesT %~ view) . view

instance Selectable TimestampsView where
    applySelection msel =
        TimestampsView . applySelection msel . timestampsViewTimestamps

newtype TimestampNameView = TimestampNameView
    { timestampNameViewText :: TextView
    } deriving (Show, Eq, Generic)

instance Validity TimestampNameView

instance View TimestampNameView where
    type Source TimestampNameView = Text
    source = source . timestampNameViewText
    view = TimestampNameView . view

timestampNameView :: TimestampName -> TimestampNameView
timestampNameView t =
    TimestampNameView {timestampNameViewText = view $ timestampNameText t}

instance Selectable TimestampNameView where
    applySelection msel =
        TimestampNameView . applySelection msel . timestampNameViewText

data TimestampView = TimestampView
    { timestampViewText :: TextView
    , timestampViewTimestamp :: Timestamp
    } deriving (Show, Eq, Generic)

instance Validity TimestampView

instance View TimestampView where
    type Source TimestampView = Timestamp
    source = timestampViewTimestamp
    view ts =
        TimestampView {timestampViewText = view "", timestampViewTimestamp = ts}

instance Selectable TimestampView where
    applySelection msel = timestampViewTextViewL %~ applySelection msel

timestampViewTextViewL :: Lens' TimestampView TextView
timestampViewTextViewL =
    lens timestampViewText $ \tsv tv -> tsv {timestampViewText = tv}

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
