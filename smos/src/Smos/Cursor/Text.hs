{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.Text
    ( TextCursor
    , emptyTextCursor
    , makeTextCursor
    , textCursorIndex
    , textCursorSelectPrev
    , textCursorSelectNext
    , textCursorSelectPrevChar
    , textCursorSelectNextChar
    , textCursorSelectStart
    , textCursorSelectEnd
    , textCursorInsert
    , textCursorRemove
    , textCursorDelete
    ) where

import Import

import qualified Data.Text as T
import Lens.Micro

import Smos.Cursor.Class
import Smos.Cursor.List

newtype TextCursor = TextCursor
    { unTextCursor :: ListCursor Char
    } deriving (Show, Eq, Generic)

instance Validity TextCursor

instance Rebuild TextCursor where
    type ReBuilding TextCursor = Text
    rebuild = T.pack . rebuildListCursor . unTextCursor

emptyTextCursor :: TextCursor
emptyTextCursor = TextCursor emptyListCursor

makeTextCursor :: Text -> TextCursor
makeTextCursor = TextCursor . makeListCursor . T.unpack

textCursorListCursorL ::
       Functor f
    => (ListCursor Char -> f (ListCursor Char))
    -> TextCursor
    -> f TextCursor
textCursorListCursorL = lens unTextCursor (\tc lc -> tc {unTextCursor = lc})

textCursorIndex :: TextCursor -> Int
textCursorIndex = listCursorIndex . unTextCursor

textCursorSelectPrev :: TextCursor -> Maybe TextCursor
textCursorSelectPrev = textCursorListCursorL listCursorSelectPrev

textCursorSelectNext :: TextCursor -> Maybe TextCursor
textCursorSelectNext = textCursorListCursorL listCursorSelectNext

textCursorSelectPrevChar :: TextCursor -> Maybe Char
textCursorSelectPrevChar = listCursorSelectPrevChar . unTextCursor

textCursorSelectNextChar :: TextCursor -> Maybe Char
textCursorSelectNextChar = listCursorSelectNextChar . unTextCursor

textCursorSelectStart :: TextCursor -> TextCursor
textCursorSelectStart = textCursorListCursorL %~ listCursorSelectStart

textCursorSelectEnd :: TextCursor -> TextCursor
textCursorSelectEnd = textCursorListCursorL %~ listCursorSelectEnd

textCursorInsert :: Char -> TextCursor -> TextCursor
textCursorInsert c = over textCursorListCursorL $ listCursorInsert c

textCursorRemove :: TextCursor -> Maybe TextCursor
textCursorRemove = textCursorListCursorL listCursorRemove

textCursorDelete :: TextCursor -> Maybe TextCursor
textCursorDelete = textCursorListCursorL listCursorDelete
