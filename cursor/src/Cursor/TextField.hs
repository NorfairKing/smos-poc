{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.TextField
    ( TextFieldCursor(..)
    , TextFieldView(..)
    , emptyTextFieldCursor
    , makeTextFieldCursor
    , rebuildTextFieldCursor
    , textFieldSelectedL
    , textFieldCursorSelectPrevLine
    , textFieldCursorSelectNextLine
    , textFieldCursorSelectUp
    , textFieldCursorSelectDown
    , textFieldCursorSelectPrev
    , textFieldCursorSelectNext
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
import Cursor.ListElem
import Cursor.Select
import Cursor.Text

newtype TextFieldCursor = TextFieldCursor
    { unTextFieldCursor :: ListElemCursor TextCursor
    } deriving (Eq, Generic)

instance Validity TextFieldCursor

instance Show TextFieldCursor where
    show TextFieldCursor {..} = "TextField:\n" ++ show unTextFieldCursor

instance Build TextFieldCursor where
    type Building TextFieldCursor = TextView
    build = rebuild . listElemCursorCurrent . unTextFieldCursor

instance Rebuild TextFieldCursor where
    type ReBuilding TextFieldCursor = TextFieldView
    rebuild tfc =
        let ListElemCursor {..} = unTextFieldCursor tfc
        in TextFieldView
           { textFieldViewAbove =
                 map (source . rebuild) $ reverse listElemCursorPrev
           , textFieldViewLine = rebuild listElemCursorCurrent
           , textFieldViewBelow = map (source . rebuild) listElemCursorNext
           }
    selection tfc =
        let ListElemCursor {..} = unTextFieldCursor tfc
        in selection listElemCursorCurrent ++ [length listElemCursorPrev]

instance Selectable TextFieldCursor where
    applySelection msel = textFieldListElemCursorL %~ applySelection msel

data TextFieldView = TextFieldView
    { textFieldViewAbove :: [Text]
    , textFieldViewLine :: TextView
    , textFieldViewBelow :: [Text]
    } deriving (Show, Eq, Generic)

instance Validity TextFieldView

instance View TextFieldView where
    type Source TextFieldView = Text
    source TextFieldView {..} =
        T.intercalate "\n" $
        concat
            [textFieldViewAbove, [source textFieldViewLine], textFieldViewBelow]
    view t =
        let lec
                -- This is safe because splitOn produces nonempty lists
             =
                fromJust $
                makeNonEmptyListElemCursor $
                map makeTextCursor $ T.splitOn "\n" t
        in rebuild TextFieldCursor {unTextFieldCursor = lec}

instance Selectable TextFieldView where
    applySelection msel tfv =
        let tfc = makeTextFieldCursor $ source tfv
        in rebuild $ applySelection msel tfc

emptyTextFieldCursor :: TextFieldCursor
emptyTextFieldCursor =
    TextFieldCursor
    {unTextFieldCursor = singletonListElemCursor emptyTextCursor}

makeTextFieldCursor :: Text -> TextFieldCursor
makeTextFieldCursor t =
    let ls = T.splitOn "\n" t
        -- This is safe because 'splitOn' always returns a nonempty list.
    in TextFieldCursor
       { unTextFieldCursor =
             fromJust $ makeNonEmptyListElemCursor $ map makeTextCursor ls
       }

rebuildTextFieldCursor :: TextFieldCursor -> Text
rebuildTextFieldCursor = source . rebuild

textFieldListElemCursorL :: Lens' TextFieldCursor (ListElemCursor TextCursor)
textFieldListElemCursorL =
    lens unTextFieldCursor $ \tfc lec -> tfc {unTextFieldCursor = lec}

textFieldSelectedL :: Lens' TextFieldCursor TextCursor
textFieldSelectedL = textFieldListElemCursorL . listElemCursorElemL

textFieldCursorSelectPrevLine :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectPrevLine =
    textFieldListElemCursorL listElemCursorSelectPrev

textFieldCursorSelectNextLine :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectNextLine =
    textFieldListElemCursorL listElemCursorSelectNext

textFieldCursorSelectUp :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectUp = textFieldCursorSelectPrevLine

textFieldCursorSelectDown :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectDown = textFieldCursorSelectNextLine

textFieldCursorSelectPrev :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectPrev = textFieldSelectedL textCursorSelectPrev

textFieldCursorSelectNext :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectNext = textFieldSelectedL textCursorSelectNext

textFieldCursorInsert :: Char -> TextFieldCursor -> TextFieldCursor
textFieldCursorInsert c = textFieldSelectedL %~ textCursorInsert c

textFieldCursorAppend :: Char -> TextFieldCursor -> TextFieldCursor
textFieldCursorAppend c = textFieldSelectedL %~ textCursorAppend c

textFieldCursorNewline :: TextFieldCursor -> TextFieldCursor
textFieldCursorNewline =
    textFieldListElemCursorL %~
    (\lec@ListElemCursor {..} ->
         let (tc1, tc2) = textCursorSplit listElemCursorCurrent
         in lec
            { listElemCursorPrev = tc1 : listElemCursorPrev
            , listElemCursorCurrent = tc2
            })

textFieldCursorRemove :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorRemove =
    textFieldListElemCursorL
        (\lec@ListElemCursor {..} ->
             case textCursorRemove listElemCursorCurrent of
                 Nothing ->
                     case listElemCursorPrev of
                         [] -> Nothing
                         (pl:pls) ->
                             Just $
                             lec
                             { listElemCursorPrev = pls
                             , listElemCursorCurrent =
                                   textCursorCombine pl listElemCursorCurrent
                             }
                 Just ctc -> Just $ lec & listElemCursorElemL .~ ctc)

textFieldCursorDelete :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorDelete =
    textFieldListElemCursorL
        (\lec@ListElemCursor {..} ->
             case textCursorDelete listElemCursorCurrent of
                 Nothing ->
                     case listElemCursorNext of
                         [] -> Nothing
                         (pl:pls) ->
                             Just $
                             lec
                             { listElemCursorCurrent =
                                   textCursorCombine listElemCursorCurrent pl
                             , listElemCursorNext = pls
                             }
                 Just ctc -> Just $ lec & listElemCursorElemL .~ ctc)

textFieldCursorSelectStart :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectStart = textFieldSelectedL %~ textCursorSelectStart

textFieldCursorSelectEnd :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectEnd = textFieldSelectedL %~ textCursorSelectEnd
