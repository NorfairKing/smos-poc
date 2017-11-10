{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Text
    ( TextCursor
    , emptyTextCursor
    , makeTextCursor
    , foldTextSel
    , textCursorIndex
    , textCursorSelectPrev
    , textCursorSelectNext
    , textCursorSelectPrevChar
    , textCursorSelectNextChar
    , textCursorSelectStart
    , textCursorSelectEnd
    , textCursorInsert
    , textCursorAppend
    , textCursorRemove
    , textCursorDelete
    ) where

import Import

import qualified Data.Text as T
import Lens.Micro

import Cursor.Class
import Cursor.List

newtype TextCursor = TextCursor
    { unTextCursor :: ListCursor Char
    } deriving (Show, Eq, Generic)

instance Validity TextCursor

instance Build TextCursor where
    type Building TextCursor = Maybe Char
    build = build . unTextCursor

instance Rebuild TextCursor where
    type ReBuilding TextCursor = Text
    rebuild = T.pack . rebuild . unTextCursor
    selection = selection . unTextCursor

instance Reselect TextCursor where
    type Reselection TextCursor = TextCursor
    reselect sel = textCursorListCursorL %~ reselect sel

emptyTextCursor :: TextCursor
emptyTextCursor = TextCursor emptyListCursor

makeTextCursor :: Text -> TextCursor
makeTextCursor = TextCursor . makeListCursor . T.unpack

foldTextSel :: (Maybe Int -> Text -> r) -> Maybe [Int] -> Text -> r
foldTextSel func msel =
    case msel of
        Nothing -> func Nothing
        Just [ix_] -> func $ Just ix_
        Just _ -> func Nothing

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
textCursorInsert c = textCursorListCursorL %~ listCursorInsert c

textCursorAppend :: Char -> TextCursor -> TextCursor
textCursorAppend c = textCursorListCursorL %~ listCursorAppend c

textCursorRemove :: TextCursor -> Maybe TextCursor
textCursorRemove = textCursorListCursorL listCursorRemove

textCursorDelete :: TextCursor -> Maybe TextCursor
textCursorDelete = textCursorListCursorL listCursorDelete
