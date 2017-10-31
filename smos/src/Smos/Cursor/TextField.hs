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
    , textFieldSelectedL
    , textFieldCursorSelectPrev
    , textFieldCursorSelectNext
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
    build = rebuildTextCursor . textFieldSelected

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
            [rebuildTextCursor textFieldSelected] ++ textFieldCursorNext
    in T.intercalate "\n" ls

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
