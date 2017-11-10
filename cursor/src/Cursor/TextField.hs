{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.TextField
    ( TextFieldCursor
    , textFieldCursorPrev
    , textFieldSelected
    , textFieldCursorNext
    , emptyTextFieldCursor
    , makeTextFieldCursor
    , foldTextFieldSel
    , rebuildTextFieldCursor
    , textFieldCursorIndices
    , textFieldSelectedL
    , textFieldCursorSelectPrev
    , textFieldCursorSelectNext
    , textFieldCursorSelectUp
    , textFieldCursorSelectDown
    , textFieldCursorInsert
    , textFieldCursorAppend
    , textFieldCursorNewline
    , textFieldCursorRemove
    , textFieldCursorDelete
    , textFieldCursorSelectStart
    , textFieldCursorSelectEnd
    ) where

import Import

import qualified Data.Text as T
import Lens.Micro

import Cursor.Class
import Cursor.Text

data TextFieldCursor = TextFieldCursor
    { textFieldCursorPrev :: [Text]
    , textFieldSelected :: TextCursor
    , textFieldCursorNext :: [Text]
    } deriving (Show, Eq, Generic)

instance Validity TextFieldCursor

instance Build TextFieldCursor where
    type Building TextFieldCursor = Text
    build = rebuild . textFieldSelected

instance Rebuild TextFieldCursor where
    type ReBuilding TextFieldCursor = Text
    rebuild = rebuildTextFieldCursor
    selection TextFieldCursor {..} =
        selection textFieldSelected ++ [length textFieldCursorPrev]

emptyTextFieldCursor :: TextFieldCursor
emptyTextFieldCursor =
    TextFieldCursor
    { textFieldCursorPrev = []
    , textFieldSelected = emptyTextCursor
    , textFieldCursorNext = []
    }

makeTextFieldCursor :: Text -> TextFieldCursor
makeTextFieldCursor t =
    let ls = T.splitOn "\n" t
    in case ls of
           [] -> emptyTextFieldCursor
           (first:rest) ->
               TextFieldCursor
               { textFieldCursorPrev = []
               , textFieldSelected = makeTextCursor first
               , textFieldCursorNext = rest
               }

foldTextFieldSel :: (Maybe (Int, Int) -> Text -> r) -> Maybe [Int] -> Text -> r
foldTextFieldSel func msel =
    case msel of
        Just [xix_, yix_] -> func (Just (xix_, yix_))
        _ -> func Nothing

rebuildTextFieldCursor :: TextFieldCursor -> Text
rebuildTextFieldCursor TextFieldCursor {..} =
    let ls =
            reverse textFieldCursorPrev ++
            [rebuild textFieldSelected] ++ textFieldCursorNext
    in T.intercalate "\n" ls

textFieldCursorIndices :: TextFieldCursor -> [Int]
textFieldCursorIndices TextFieldCursor {..} =
    [length textFieldCursorPrev, textCursorIndex textFieldSelected]

-- reselectTextFieldCursor :: [Int] -> TextFieldCursor -> TextFieldCursor
-- reselectTextFieldCursor [x, y] _ = undefined
-- reselectTextFieldCursor _ tc = tc
textFieldSelectedL ::
       Functor f
    => (TextCursor -> f TextCursor)
    -> TextFieldCursor
    -> f TextFieldCursor
textFieldSelectedL =
    lens textFieldSelected $ \tfc tc -> tfc {textFieldSelected = tc}

textFieldCursorSelectPrev :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectPrev = textFieldSelectedL textCursorSelectPrev

textFieldCursorSelectNext :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectNext = textFieldSelectedL textCursorSelectNext

textFieldCursorSelectPrevLine :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectPrevLine tfc =
    case textFieldCursorPrev tfc of
        [] -> Nothing
        (p:rest) ->
            Just $
            tfc
            { textFieldCursorPrev = rest
            , textFieldSelected = makeTextCursor p
            , textFieldCursorNext =
                  rebuild (textFieldSelected tfc) : textFieldCursorNext tfc
            }

textFieldCursorSelectUp :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectUp = textFieldCursorSelectPrevLine

textFieldCursorSelectNextLine :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectNextLine tfc =
    case textFieldCursorNext tfc of
        [] -> Nothing
        (p:rest) ->
            Just $
            tfc
            { textFieldCursorPrev =
                  rebuild (textFieldSelected tfc) : textFieldCursorPrev tfc
            , textFieldSelected = makeTextCursor p
            , textFieldCursorNext = rest
            }

textFieldCursorSelectDown :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectDown = textFieldCursorSelectNextLine

textFieldCursorInsert :: Char -> TextFieldCursor -> TextFieldCursor
textFieldCursorInsert c = textFieldSelectedL %~ textCursorInsert c

textFieldCursorAppend :: Char -> TextFieldCursor -> TextFieldCursor
textFieldCursorAppend c = textFieldSelectedL %~ textCursorAppend c

textFieldCursorNewline :: TextFieldCursor -> TextFieldCursor
textFieldCursorNewline tfc =
    tfc
    { textFieldCursorPrev =
          rebuild (textFieldSelected tfc) : textFieldCursorPrev tfc
    , textFieldSelected = emptyTextCursor
    }

textFieldCursorRemove :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorRemove tfc =
    case textCursorRemove $ textFieldSelected tfc of
        Nothing ->
            case textFieldCursorPrev tfc of
                [] -> Nothing
                (p:rest) ->
                    Just $
                    tfc
                    { textFieldCursorPrev = rest
                    , textFieldSelected = textCursorSelectEnd $ makeTextCursor p
                    }
        Just tc' -> Just $ tfc & textFieldSelectedL .~ tc'

textFieldCursorDelete :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorDelete tfc =
    case textCursorDelete $ textFieldSelected tfc of
        Nothing ->
            case textFieldCursorNext tfc of
                [] -> Nothing
                (p:rest) ->
                    Just $
                    tfc
                    { textFieldSelected =
                          textCursorSelectStart $ makeTextCursor p
                    , textFieldCursorNext = rest
                    }
        Just tc' -> Just $ tfc & textFieldSelectedL .~ tc'

textFieldCursorSelectStart :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectStart = textFieldSelectedL %~ textCursorSelectStart

textFieldCursorSelectEnd :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectEnd = textFieldSelectedL %~ textCursorSelectEnd
