{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Text
    ( TextCursor(..)
    , TextView(..)
    , emptyTextCursor
    , makeTextCursor
    , rebuildTextCursor
    , textCursorNull
    , textCursorIndex
    , textCursorSelectPrev
    , textCursorSelectNext
    , textCursorSelectIndex
    , textCursorSelectPrevChar
    , textCursorSelectNextChar
    , textCursorSelectStart
    , textCursorSelectEnd
    , textCursorInsert
    , textCursorAppend
    , textCursorRemove
    , textCursorDelete
    , textCursorSplit
    , textCursorCombine
    ) where

import Import

import qualified Data.Text as T
import Lens.Micro

import Cursor.Class
import Cursor.List
import Cursor.Select

newtype TextCursor = TextCursor
    { unTextCursor :: ListCursor Char
    } deriving (Show, Eq, Generic)

instance Validity TextCursor

instance Build TextCursor where
    type Building TextCursor = Maybe Char
    build = build . unTextCursor

instance Rebuild TextCursor where
    type ReBuilding TextCursor = TextView
    rebuild TextCursor {..} =
        TextView
        { textViewLeft = T.reverse $ T.pack $ listCursorPrev unTextCursor
        , textViewRight = T.pack $ listCursorNext unTextCursor
        }
    selection = selection . unTextCursor

instance Reselect TextCursor where
    type Reselection TextCursor = TextCursor
    reselect sel = textCursorListCursorL %~ reselect sel

data TextView = TextView
    { textViewLeft :: Text
    , textViewRight :: Text
    } deriving (Show, Eq, Generic)

instance Validity TextView

instance View TextView where
    type Source TextView = Text
    source TextView {..} = textViewLeft <> textViewRight
    view t = TextView {textViewLeft = T.empty, textViewRight = t}

instance Selectable TextView where
    applySelection msel = textViewListViewL %~ applySelection msel

textViewListViewL :: Lens' TextView (ListView Char)
textViewListViewL = lens getter setter
  where
    getter TextView {..} =
        ListView
        { listViewPrev = T.unpack textViewLeft
        , listViewNext = T.unpack textViewRight
        }
    setter _ ListView {..} =
        TextView
        { textViewLeft = T.pack listViewPrev
        , textViewRight = T.pack listViewNext
        }

emptyTextCursor :: TextCursor
emptyTextCursor = TextCursor emptyListCursor

makeTextCursor :: Text -> TextCursor
makeTextCursor = TextCursor . makeListCursor . T.unpack

rebuildTextCursor :: TextCursor -> Text
rebuildTextCursor = source . rebuild

textCursorListCursorL ::
       Functor f
    => (ListCursor Char -> f (ListCursor Char))
    -> TextCursor
    -> f TextCursor
textCursorListCursorL = lens unTextCursor (\tc lc -> tc {unTextCursor = lc})

textCursorNull :: TextCursor -> Bool
textCursorNull = listCursorNull . unTextCursor

textCursorIndex :: TextCursor -> Int
textCursorIndex = listCursorIndex . unTextCursor

textCursorSelectPrev :: TextCursor -> Maybe TextCursor
textCursorSelectPrev = textCursorListCursorL listCursorSelectPrev

textCursorSelectNext :: TextCursor -> Maybe TextCursor
textCursorSelectNext = textCursorListCursorL listCursorSelectNext

textCursorSelectIndex :: Int -> TextCursor -> TextCursor
textCursorSelectIndex ix_ = textCursorListCursorL %~ listCursorSelectIndex ix_

textCursorSelectPrevChar :: TextCursor -> Maybe Char
textCursorSelectPrevChar = listCursorSelectPrevChar . unTextCursor

textCursorSelectNextChar :: TextCursor -> Maybe Char
textCursorSelectNextChar = listCursorSelectNextChar . unTextCursor

textCursorSelectStart :: TextCursor -> TextCursor
textCursorSelectStart = textCursorListCursorL %~ listCursorSelectStart

textCursorSelectEnd :: TextCursor -> TextCursor
textCursorSelectEnd = textCursorListCursorL %~ listCursorSelectEnd

textCursorInsert :: Char -> TextCursor -> TextCursor
textCursorInsert c = textCursorListCursorL %~ listCursorInsert c

textCursorAppend :: Char -> TextCursor -> TextCursor
textCursorAppend c = textCursorListCursorL %~ listCursorAppend c

textCursorRemove :: TextCursor -> Maybe TextCursor
textCursorRemove = textCursorListCursorL listCursorRemove

textCursorDelete :: TextCursor -> Maybe TextCursor
textCursorDelete = textCursorListCursorL listCursorDelete

textCursorSplit :: TextCursor -> (TextCursor, TextCursor)
textCursorSplit tc =
    let (lc1, lc2) = listCursorSplit $ unTextCursor tc
    in (TextCursor lc1, TextCursor lc2)

textCursorCombine :: TextCursor -> TextCursor -> TextCursor
textCursorCombine (TextCursor lc1) (TextCursor lc2) =
    TextCursor {unTextCursor = listCursorCombine lc1 lc2}
