module Smos.Cursor.Text
    ( TextCursor
    , emptyTextCursor
    , makeTextCursor
    , rebuildTextCursor
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

import Smos.Cursor.List

type TextCursor = ListCursor Char

emptyTextCursor :: TextCursor
emptyTextCursor = emptyListCursor

makeTextCursor :: Text -> TextCursor
makeTextCursor = makeListCursor . T.unpack

rebuildTextCursor :: TextCursor -> Text
rebuildTextCursor = T.pack . rebuildListCursor

textCursorIndex :: TextCursor -> Int
textCursorIndex = listCursorIndex

textCursorSelectPrev :: TextCursor -> Maybe TextCursor
textCursorSelectPrev = listCursorSelectPrev

textCursorSelectNext :: TextCursor -> Maybe TextCursor
textCursorSelectNext = listCursorSelectNext

textCursorSelectPrevChar :: TextCursor -> Maybe Char
textCursorSelectPrevChar = listCursorSelectPrevChar

textCursorSelectNextChar :: TextCursor -> Maybe Char
textCursorSelectNextChar = listCursorSelectNextChar

textCursorSelectStart :: TextCursor -> TextCursor
textCursorSelectStart = listCursorSelectStart

textCursorSelectEnd :: TextCursor -> TextCursor
textCursorSelectEnd = listCursorSelectEnd

textCursorInsert :: Char -> TextCursor -> TextCursor
textCursorInsert = listCursorInsert

textCursorRemove :: TextCursor -> Maybe TextCursor
textCursorRemove = listCursorRemove

textCursorDelete :: TextCursor -> Maybe TextCursor
textCursorDelete = listCursorDelete
