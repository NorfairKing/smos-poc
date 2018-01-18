{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
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
    renderCursor :: SmosFileCursor -> Widget ResourceName
    renderCursor cur =
        drawSmosFile sfv <=>
        if smosStateShowDebug
            then B.vBox
                     [ str $ show rsel
                     , drawHistory smosStateKeyHistory
                     , strWrap $ show sfv
                     , strWrap $ show cur
                     ]
            else emptyWidget
      where
        sfv = applyFileViewSelection rsel $ rebuild cur
        rsel = reverse $ selection $ selectAnyCursor $ fileCursorA cur

applyFileViewSelection :: [Int] -> SmosFileView -> SmosFileView
applyFileViewSelection sel = applySelection (Just sel)

drawNoContent :: Widget n
drawNoContent =
    B.vCenterLayer $
    B.vBox $
    map
        B.hCenterLayer
        [ str "SMOS"
        , str " "
        , str "version 0.0.0"
        , str "by Tom Sydney Kerckhove"
        , str "Smos is open source and freely distributable"
        ]

drawSmosFile :: SmosFileView -> Widget ResourceName
drawSmosFile = drawForest 1 . smosFileViewForest

drawForest :: Int -> Select (ForestView EntryView) -> Widget ResourceName
drawForest level = withSel $ B.vBox . map (drawTree level) . forestViewTrees

drawTree :: Int -> Select (TreeView EntryView) -> Widget ResourceName
drawTree level =
    withSel $ \TreeView {..} ->
        drawEntry level treeViewValue <=> drawForest (level + 1) treeViewForest

drawEntry :: Int -> Select EntryView -> Widget ResourceName
drawEntry level sev =
    let EntryView {..} = selectValue sev
    in B.vBox
           [ B.hBox $
             intersperse (B.txt " ") $
             [flip withSel (() <$ sev) $ \() -> B.str $ replicate level '*'] ++
             maybeToList (drawTodoState entryViewTodostate) ++
             [drawHeader entryViewHeader] ++
             maybeToList (drawTags <$> entryViewTags)
           , maybe emptyWidget drawTimestamps entryViewTimestamps
           , drawProperties entryViewProperties
           , maybe emptyWidget drawContents entryViewContents
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
        NE.toList $
        flip fmap (source timestampsViewTimestamps) $ \(k, ts) ->
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
                B.hBox [str "[", drawUTCTime start, str "]"] <=>
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
drawBoxedTimestamp ts = B.hBox [str "[", drawUTCTime ts, str "]"]

drawTimestamp :: Timestamp -> Widget n
drawTimestamp (TimestampDay d) =
    B.str $ formatTime defaultTimeLocale timestampDayFormat d
drawTimestamp (TimestampTime lt) =
    B.str $ formatTime defaultTimeLocale timestampTimeFormat lt

drawUTCTime :: UTCTime -> Widget n
drawUTCTime = B.str . formatTime defaultTimeLocale "%F %R"

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
    let ListElemView {..} = selectValue slv
        prev = prevCombFunc $ map (prevFunc . unsel) listElemViewPrev
        cur = curFunc $ sel listElemViewCurrent
        next = nextCombFunc $ map (nextFunc . unsel) listElemViewNext
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
