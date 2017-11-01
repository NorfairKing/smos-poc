{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.TextField
    ( TextFieldCursor
    , textFieldCursorPrev
    , textFieldSelected
    , textFieldCursorNext
    , emptyTextFieldCursor
    , makeTextFieldCursor
    , rebuildTextFieldCursor
    , textFieldCursorIndices
    , textFieldSelectedL
    , textFieldCursorSelectPrev
    , textFieldCursorSelectNext
    , textFieldCursorSelectUp
    , textFieldCursorSelectDown
    ) where

import Import

import qualified Data.Text as T
import Lens.Micro

import Smos.Cursor.Class
import Smos.Cursor.Text

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

textFieldCursorSelectUp :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectUp tfc =
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

textFieldCursorSelectDown :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectDown tfc =
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
