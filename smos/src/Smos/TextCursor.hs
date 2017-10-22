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
    ) where

import Import

import Data.Text as T

data TextCursor = TextCursor
    { textCursorPrev :: Maybe (Char, TextCursor)
    , textCursorNext :: Maybe (Char, TextCursor)
    } deriving (Generic)

instance Show TextCursor where
    show tc = showLeft tc ++ "-|-" ++ showRight tc
      where
        showLeft = go . textCursorPrev
          where
            go Nothing = "|-"
            go (Just (c, tc_)) = showLeft tc_ ++ [c]
        showRight = go . textCursorNext
          where
            go Nothing = "-|"
            go (Just (c, tc_)) = c : showRight tc_

emptyTextCursor :: TextCursor
emptyTextCursor =
    TextCursor {textCursorPrev = Nothing, textCursorNext = Nothing}

makeTextCursor :: Text -> TextCursor
makeTextCursor = go Nothing . T.unpack
  where
    go par [] = TextCursor {textCursorPrev = par, textCursorNext = Nothing}
    go par (c:cs) =
        let pc =
                TextCursor {textCursorPrev = par, textCursorNext = Just (c, nc)}
            nc = go (Just (c, pc)) cs
        in pc

rebuildTextCursor :: TextCursor -> Text
rebuildTextCursor = T.pack . goL
  where
    goL tc =
        case textCursorPrev tc of
            Nothing -> goR tc
            Just (_, tcl) -> goL tcl
    goR tc =
        case textCursorNext tc of
            Nothing -> ""
            Just (c, tcn) -> c : goR tcn

textCursorSelectPrev :: TextCursor -> Maybe TextCursor
textCursorSelectPrev TextCursor {..} = snd <$> textCursorPrev

textCursorSelectNext :: TextCursor -> Maybe TextCursor
textCursorSelectNext TextCursor {..} = snd <$> textCursorNext

textCursorSelectPrevChar :: TextCursor -> Maybe Char
textCursorSelectPrevChar TextCursor {..} = fst <$> textCursorPrev

textCursorSelectNextChar :: TextCursor -> Maybe Char
textCursorSelectNextChar TextCursor {..} = fst <$> textCursorPrev

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

rebuildLeft :: Char -> TextCursor -> TextCursor -> Maybe (Char, TextCursor)
rebuildLeft c par tc =
    case textCursorPrev tc of
        Nothing -> Nothing
        Just (c', tc') ->
            let tc'' =
                    tc'
                    { textCursorPrev = rebuildLeft c' tc'' tc'
                    , textCursorNext = Just (c, par)
                    }
            in Just (c', tc'')

rebuildRight :: Char -> TextCursor -> TextCursor -> Maybe (Char, TextCursor)
rebuildRight c par tc =
    case textCursorNext tc of
        Nothing -> Nothing
        Just (c', tc') ->
            let tc'' =
                    tc'
                    { textCursorNext = Just (c, par)
                    , textCursorPrev = rebuildRight c' tc'' tc'
                    }
            in Just (c', tc'')

textCursorInsert :: Char -> TextCursor -> TextCursor
textCursorInsert c tc = tc3
  where
    tc1 = rebuildLeft c tc2 tc
    tc2 = TextCursor {textCursorPrev = tc1, textCursorNext = Just (c, tc3)}
    tc3 = TextCursor {textCursorPrev = Just (c, tc2), textCursorNext = tc4}
    tc4 = rebuildRight c tc3 tc
