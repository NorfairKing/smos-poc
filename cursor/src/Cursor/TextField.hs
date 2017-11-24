{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.TextField
    ( TextFieldCursor
    , TextFieldView(..)
    , textFieldCursorPrev
    , textFieldSelected
    , textFieldCursorNext
    , emptyTextFieldCursor
    , makeTextFieldCursor
    , rebuildTextFieldCursor
    , foldTextFieldSel
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
import Cursor.Select
import Cursor.Text

data TextFieldCursor = TextFieldCursor
    { textFieldCursorPrev :: [Text]
    , textFieldSelected :: TextCursor
    , textFieldCursorNext :: [Text]
    } deriving (Show, Eq, Generic)

instance Validity TextFieldCursor

instance Build TextFieldCursor where
    type Building TextFieldCursor = TextView
    build = rebuild . textFieldSelected

instance Rebuild TextFieldCursor where
    type ReBuilding TextFieldCursor = TextFieldView
    rebuild TextFieldCursor {..} =
        TextFieldView
        { textFieldViewAbove =
              case textFieldCursorPrev of
                  [] -> Nothing
                  ls -> Just $ T.intercalate "\n" $ reverse ls
        , textFieldViewLine = rebuild textFieldSelected
        , textFieldViewBelow =
              case textFieldCursorNext of
                  [] -> Nothing
                  ls -> Just $ T.intercalate "\n" ls
        }
    selection TextFieldCursor {..} =
        selection textFieldSelected ++ [length textFieldCursorPrev]

data TextFieldView = TextFieldView
    { textFieldViewAbove :: Maybe Text
    , textFieldViewLine :: TextView
    , textFieldViewBelow :: Maybe Text
    } deriving (Show, Eq, Generic)

instance Validity TextFieldView

instance View TextFieldView where
    type Source TextFieldView = Text
    source TextFieldView {..} =
        mconcat
            [ maybe "" (<> "\n") textFieldViewAbove
            , source textFieldViewLine
            , maybe "" ("\n" <>) textFieldViewBelow
            ]
    view t =
        case T.splitOn "\n" t of
            [] ->
                TextFieldView
                { textFieldViewAbove = Nothing
                , textFieldViewLine = view ""
                , textFieldViewBelow = Nothing
                }
            [r] ->
                TextFieldView
                { textFieldViewAbove = Nothing
                , textFieldViewLine = view r
                , textFieldViewBelow = Nothing
                }
            (r:rs) ->
                TextFieldView
                { textFieldViewAbove = Nothing
                , textFieldViewLine = view r
                , textFieldViewBelow = Just $ T.intercalate "\n" rs
                }

instance Selectable TextFieldView where
    applySelection =
        drillWithSel $ \mixr_ tfv ->
            let t = source tfv
            in case mixr_ of
                   Nothing -> view t
                   Just (ix_, sel) ->
                       let (l, m, r) = applyTextLinesSelection ix_ t
                       in TextFieldView
                          { textFieldViewAbove = l
                          , textFieldViewLine =
                                maybe
                                    (view t)
                                    (applySelection (Just sel) . view)
                                    m
                          , textFieldViewBelow = r
                          }

applyTextLinesSelection :: Int -> Text -> (Maybe Text, Maybe Text, Maybe Text)
applyTextLinesSelection x t =
    let ls = T.splitOn "\n" t
        (l, m, r) = applyListSelection x ls
    in (T.intercalate "\n" <$> l, m, T.intercalate "\n" <$> r)

applyListSelection :: Int -> [a] -> (Maybe [a], Maybe a, Maybe [a])
applyListSelection x ls =
    let n [] = Nothing
        n ls_ = Just ls_
    in case drop x ls of
           [] -> (n ls, Nothing, Nothing)
           (m:r) -> (n $ take x ls, Just m, n r)

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
rebuildTextFieldCursor = source . rebuild

foldTextFieldSel :: (Maybe (Int, Int) -> Text -> r) -> Maybe [Int] -> Text -> r
foldTextFieldSel func msel =
    case msel of
        Just [xix_, yix_] -> func (Just (xix_, yix_))
        _ -> func Nothing

textFieldCursorIndices :: TextFieldCursor -> [Int]
textFieldCursorIndices TextFieldCursor {..} =
    [length textFieldCursorPrev, textCursorIndex textFieldSelected]

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
                  source (rebuild (textFieldSelected tfc)) :
                  textFieldCursorNext tfc
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
                  source (rebuild (textFieldSelected tfc)) :
                  textFieldCursorPrev tfc
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
          source (rebuild (textFieldSelected tfc)) : textFieldCursorPrev tfc
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
