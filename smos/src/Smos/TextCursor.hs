{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.TextCursor
    ( TextCursor
    , emptyTextCursor
    , makeTextCursor
    , rebuildTextCursor
    , textCursorPrev
    , textCursorNext
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

data TextCursor = TextCursor
    { textCursorPrev :: String
    , textCursorNext :: String
    } deriving (Generic)

instance Show TextCursor where
    show TextCursor {..} =
        concat ["|-", reverse textCursorPrev, "-|-", textCursorNext, "-|"]

emptyTextCursor :: TextCursor
emptyTextCursor = TextCursor {textCursorPrev = [], textCursorNext = []}

makeTextCursor :: Text -> TextCursor
makeTextCursor t = TextCursor {textCursorPrev = [], textCursorNext = T.unpack t}

rebuildTextCursor :: TextCursor -> Text
rebuildTextCursor TextCursor {..} =
    T.pack $ reverse textCursorPrev ++ textCursorNext

textCursorSelectPrev :: TextCursor -> Maybe TextCursor
textCursorSelectPrev tc =
    case textCursorPrev tc of
        [] -> Nothing
        (c:cs) ->
            Just
                TextCursor
                {textCursorPrev = cs, textCursorNext = c : textCursorNext tc}

textCursorSelectNext :: TextCursor -> Maybe TextCursor
textCursorSelectNext tc =
    case textCursorNext tc of
        [] -> Nothing
        (c:cs) ->
            Just
                TextCursor
                {textCursorPrev = c : textCursorPrev tc, textCursorNext = cs}

textCursorSelectPrevChar :: TextCursor -> Maybe Char
textCursorSelectPrevChar tc =
    case textCursorPrev tc of
        [] -> Nothing
        (c:_) -> Just c

textCursorSelectNextChar :: TextCursor -> Maybe Char
textCursorSelectNextChar tc =
    case textCursorNext tc of
        [] -> Nothing
        (c:_) -> Just c

textCursorSelectStart :: TextCursor -> TextCursor
textCursorSelectStart tc =
    case textCursorSelectPrev tc of
        Nothing -> tc
        Just tc' -> textCursorSelectStart tc'

textCursorSelectEnd :: TextCursor -> TextCursor
textCursorSelectEnd tc =
    case textCursorSelectNext tc of
        Nothing -> tc
        Just tc' -> textCursorSelectEnd tc'

textCursorInsert :: Char -> TextCursor -> TextCursor
textCursorInsert c tc =
    TextCursor
    {textCursorPrev = c : textCursorPrev tc, textCursorNext = textCursorNext tc}

textCursorRemove :: TextCursor -> Maybe TextCursor
textCursorRemove tc =
    case textCursorPrev tc of
        [] -> Nothing
        (_:prev) -> Just $ tc {textCursorPrev = prev}

textCursorDelete :: TextCursor -> Maybe TextCursor
textCursorDelete tc =
    case textCursorNext tc of
        [] -> Nothing
        (_:next) -> Just $ tc {textCursorNext = next}
