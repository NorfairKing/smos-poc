{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Time

import Brick.Types as B
import Brick.Widgets.Center as B
import Brick.Widgets.Core as B
import Graphics.Vty.Input.Events (Key(..), Modifier(..))

import Cursor.ListElem
import Cursor.Select
import Cursor.Text
import Cursor.TextField
import Cursor.Tree

import Smos.Data

import Smos.Cursor
import Smos.Style
import Smos.Types
import Smos.View

smosDraw :: SmosState -> [Widget ResourceName]
smosDraw SmosState {..} = [maybe drawNoContent renderCursor smosStateCursor]
  where
    renderCursor :: ACursor -> Widget ResourceName
    renderCursor cur =
        drawSmosFile sfv <=> str (show rsel) <=> drawHistory smosStateKeyHistory <=>
        strWrap (show sfv) <=>
        strWrap (show cur)
      where
        sfv = applyFileViewSelection rsel $ rebuild cur
        rsel = reverse $ selection $ selectAnyCursor cur

applyFileViewSelection :: [Int] -> SmosFileView -> SmosFileView
applyFileViewSelection sel (SmosFileView f) =
    SmosFileView $ select $ applySelection (Just sel) $ selectValue f

drawNoContent :: Widget n
drawNoContent =
    B.vCenterLayer $
    B.vBox $
    map B.hCenterLayer
        [ str "SMOS"
        , str " "
        , str "version 0.0.0"
        , str "by Tom Sydney Kerckhove"
        , str "Smos is open source and freely distributable"
        ]

drawSmosFile :: SmosFileView -> Widget ResourceName
drawSmosFile = drawForest . smosFileViewForest

drawForest :: Select (ForestView EntryView) -> Widget ResourceName
drawForest = withSel $ padLeft (Pad 2) . B.vBox . map drawTree . forestViewTrees

drawTree :: Select (TreeView EntryView) -> Widget ResourceName
drawTree =
    withSel $ \TreeView {..} ->
        drawEntry treeViewValue <=> drawForest treeViewForest

drawEntry :: Select EntryView -> Widget ResourceName
drawEntry =
    withSel $ \EntryView {..} ->
        B.vBox
            [ B.hBox $
              intersperse (B.txt " ") $
              [B.txt ">"] ++
              maybeToList (drawTodoState entryViewTodostate) ++
              [drawHeader entryViewHeader] ++
              maybeToList (drawTags <$> entryViewTags)
            , drawTimestamps entryViewTimestamps
            , drawProperties entryViewProperties
            , fromMaybe emptyWidget $ drawContents <$> entryViewContents
            , drawLogbook entryViewLogbook
            , drawTodoStateHistory entryViewTodostate
            ]

drawTodoState :: Select TodostateView -> Maybe (Widget n)
drawTodoState sh = do
    ts <- stateHistoryState . source $ selectValue sh
    pure $
        flip withSel (ts <$ sh) $ \s@TodoState {..} ->
            withAttr todoStateAttr $
            withAttr (todoStateSpecificAttr s) $ B.txt todoStateText

drawTodoStateHistory :: Select TodostateView -> Widget n
drawTodoStateHistory =
    withSel $ \TodostateView {..} ->
        let es = unStateHistory todostateViewTodostate
        in withAttr todoStateHistoryAttr $
           B.vBox $
           flip map es $ \StateHistoryEntry {..} ->
               hBox
                   [ drawBoxedTimestamp stateHistoryEntryTimestamp
                   , B.txt " "
                   , case stateHistoryEntryNewState of
                         Just ts -> B.txt $ todoStateText ts
                         Nothing -> B.txt ""
                   ]

drawHeader :: Select HeaderView -> Widget ResourceName
drawHeader = withAttr headerAttr . drawTextView . fmap headerViewHeader

drawContents :: Select ContentsView -> Widget ResourceName
drawContents =
    withAttr contentsAttr . drawTextFieldView . fmap contentsViewContents

drawTags :: Select TagsView -> Widget ResourceName
drawTags stgsv =
    drawHorizontalListElemView drawPrev drawCur drawNext $
    tagsViewTags <$> stgsv
  where
    drawPrev stv = B.txt ":" <+> drawTag stv
    drawNext stv = drawTag stv <+> B.txt ":"
    drawCur stv = B.txt ":" <+> drawTag stv <+> B.txt ":"

drawTag :: Select TagView -> Widget ResourceName
drawTag = withAttr tagAttr . drawTextView . fmap tagViewText

drawTimestamps :: Select TimestampsView -> Widget ResourceName
drawTimestamps =
    withSel $ \TimestampsView {..} ->
        B.vBox $
        flip map (HM.toList timestampsViewTimestamps) $ \(k, ts) ->
            B.hBox [B.txt $ timestampNameText k, B.txt ": ", drawTimestamp ts]

drawProperties :: Select PropertiesView -> Widget ResourceName
drawProperties =
    withSel $ \PropertiesView {..} ->
        B.vBox $
        flip map (HM.toList propertiesViewProperties) $ \(k, p) ->
            B.hBox
                [ B.txt $ propertyNameText k
                , B.txt ": "
                , B.txt $ propertyValueText p
                ]

drawLogbook :: Select LogbookView -> Widget n
drawLogbook = withSel (go . logbookViewLogbook)
  where
    go lb =
        case lb of
            LogOpen start es ->
                B.hBox [str "[", drawTimestamp start, str "]"] <=>
                B.vBox (map drawLogbookEntry es)
            LogClosed es -> B.vBox $ map drawLogbookEntry es

drawLogbookEntry :: LogbookEntry -> Widget n
drawLogbookEntry LogbookEntry {..} =
    B.hBox
        [ drawBoxedTimestamp logbookEntryStart
        , str "--"
        , drawBoxedTimestamp logbookEntryEnd
        ]

drawBoxedTimestamp :: UTCTime -> Widget n
drawBoxedTimestamp ts = B.hBox [str "[", drawTimestamp ts, str "]"]

drawTimestamp :: UTCTime -> Widget n
drawTimestamp = B.str . formatTime defaultTimeLocale "%F %R"

drawHorizontalListElemView ::
       (Select a -> Widget n)
    -> (Select a -> Widget n)
    -> (Select a -> Widget n)
    -> Select (ListElemView a)
    -> Widget n
drawHorizontalListElemView prevFunc curFunc nextFunc =
    drawListElemView
        prevFunc
        curFunc
        nextFunc
        B.hBox
        B.hBox
        (\a b c -> a <+> b <+> c)

drawListElemView ::
       (Select a -> Widget n)
    -> (Select a -> Widget n)
    -> (Select a -> Widget n)
    -> ([Widget n] -> Widget n)
    -> ([Widget n] -> Widget n)
    -> (Widget n -> Widget n -> Widget n -> Widget n)
    -> Select (ListElemView a)
    -> Widget n
drawListElemView prevFunc curFunc nextFunc prevCombFunc nextCombFunc combFunc slv =
    flip withSel slv $ \ListElemView {..} ->
        let prev = prevCombFunc $ map (prevFunc . unsel) listElemViewPrev
            next = nextCombFunc $ map (nextFunc . unsel) listElemViewNext
            cur = curFunc $ sel listElemViewCurrent
        in combFunc prev cur next
  where
    sel a = (select a) {selected = selected slv}
    unsel a = (select a) {selected = False}

drawTextFieldView :: Select TextFieldView -> Widget ResourceName
drawTextFieldView stv =
    let TextFieldView {..} = selectValue stv
        yix_ = length textFieldViewAbove
        xix_ = T.length . textViewLeft $ textFieldViewLine
        addCursor =
            eitherOrSel
                (B.showCursor textCursorName (B.Location (xix_, yix_)))
                id
                stv
        addSelected = eitherOrSel (withAttr selectedAttr) id stv
        w =
            B.vBox (map B.txt textFieldViewAbove) <=>
            drawTextView (select textFieldViewLine) <=>
            B.vBox (map B.txt textFieldViewBelow)
    in addSelected . addCursor $ w

drawTextView :: Select TextView -> Widget ResourceName
drawTextView stv =
    let TextView {..} = selectValue stv
        ix_ = T.length textViewLeft
        addCursor =
            eitherOrSel
                (B.showCursor textCursorName (B.Location (ix_, 0)))
                id
                stv
        addSelected = eitherOrSel (withAttr selectedAttr) id stv
        w = B.txt textViewLeft <+> B.txt textViewRight
    in addSelected . addCursor $ w

withSel :: (a -> Widget b) -> Select a -> Widget b
withSel func s =
    eitherOrSel (withAttr selectedAttr) id s . func . selectValue $ s

eitherOrSel :: b -> b -> Select a -> b
eitherOrSel b1 b2 s =
    if selected s
        then b1
        else b2

drawHistory :: [KeyPress] -> Widget n
drawHistory = strWrap . unwords . map showKeypress . reverse
  where
    showKeypress (KeyPress key mods) =
        case mods of
            [] -> showKey key
            _ -> intercalate "-" $ map showMod mods ++ [showKey key]
    showKey (KChar c) = [c]
    showKey (KFun i) = "F" ++ show i
    showKey k = show k
    showMod MShift = "S"
    showMod MCtrl = "C"
    showMod MMeta = "M"
    showMod MAlt = "A"
