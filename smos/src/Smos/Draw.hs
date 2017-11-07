{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Time
import Data.Tree hiding (drawForest, drawTree)

import Brick.Types as B
import Brick.Widgets.Center as B
import Brick.Widgets.Core as B

import Smos.Data

import Smos.Cursor
import Smos.Cursor.Entry
import Smos.Cursor.Text
import Smos.Cursor.TextField
import Smos.Cursor.Tree
import Smos.Style
import Smos.Types

smosDraw :: SmosState -> [Widget ResourceName]
smosDraw SmosState {..} = [maybe drawNoContent renderCursor smosStateCursor]
  where
    renderCursor :: ACursor -> Widget ResourceName
    renderCursor cur =
        drawForest msel for <=> str (show rsel) <=>
        strWrap (show smosStateKeyHistory)
      where
        msel = Just rsel
        rsel = reverse $ selection $ selectAnyCursor cur
        for = smosFileForest $ rebuild cur

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

drawForest :: Maybe [Int] -> Forest Entry -> Widget ResourceName
drawForest = foldForestSel drawTree $ padLeft (Pad 2) . B.vBox . map snd

drawTree :: Maybe [Int] -> Tree Entry -> Widget ResourceName
drawTree = foldTreeSel drawEntry drawForest (<=>)

drawEntry :: Maybe [Int] -> Entry -> Widget ResourceName
drawEntry msel =
    foldEntrySel
        drawTodoState
        drawHeader
        drawTags
        drawTimestamps
        drawProperties
        drawContents
        drawLogbook
        (\mts h tgs tss pss mc lb ->
             withSel msel $
             B.vBox
                 [ B.hBox $
                   intersperse (B.txt " ") $
                   [B.txt ">"] ++ maybeToList mts ++ [h, tgs]
                 , tss
                 , pss
                 , fromMaybe emptyWidget mc
                 , lb
                 ])
        msel

drawTodoState :: Maybe [Int] -> StateHistory -> Maybe (Widget ResourceName)
drawTodoState msel sh = do
    ts <- stateHistoryState sh
    pure $
        withSel msel $
        withAttr todoStateAttr $
        withAttr (todoStateSpecificAttr ts) $ B.txt $ todoStateText ts

drawHeader :: Maybe [Int] -> Header -> Widget ResourceName
drawHeader msel Header {..} = withAttr headerAttr $ withTextSel msel headerText

drawContents :: Maybe [Int] -> Contents -> Widget ResourceName
drawContents msel Contents {..} =
    withAttr contentsAttr $ withTextFieldSel msel contentsText

drawTags :: Maybe [Int] -> [Tag] -> Widget ResourceName
drawTags msel =
    withSel msel . foldTagsSel drawTag (B.hBox . addColons . map snd) msel
  where
    addColons ls =
        case ls of
            [] -> []
            _ -> colon : intersperse colon ls ++ [colon]
      where
        colon = B.txt ":"

drawTag :: Maybe [Int] -> Tag -> Widget ResourceName
drawTag msel Tag {..} = withAttr tagAttr $ withTextSel msel tagText

drawTimestamps ::
       Maybe [Int] -> HashMap TimestampName UTCTime -> Widget ResourceName
drawTimestamps msel tss =
    withSel msel $
    B.vBox $
    flip map (HM.toList tss) $ \(k, ts) ->
        B.hBox [B.txt $ timestampNameText k, B.txt ": ", drawTimestamp ts]

drawProperties ::
       Maybe [Int] -> HashMap PropertyName PropertyValue -> Widget ResourceName
drawProperties msel pss =
    withSel msel $
    B.vBox $
    flip map (HM.toList pss) $ \(k, p) ->
        B.hBox
            [ B.txt $ propertyNameText k
            , B.txt ": "
            , B.txt $ propertyValueText p
            ]

drawTimestamp :: UTCTime -> Widget n
drawTimestamp = B.str . formatTime defaultTimeLocale "%F %R"

drawLogbook :: Maybe [Int] -> Logbook -> Widget n
drawLogbook msel = withSel msel . go
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
        [ str "["
        , drawTimestamp logbookEntryStart
        , str "]--["
        , drawTimestamp logbookEntryEnd
        , str "]"
        ]

withSel :: Maybe [Int] -> Widget n -> Widget n
withSel msel =
    case msel of
        Nothing -> id
        Just [] -> withAttr selectedAttr
        Just _ -> id

withTextSel :: Maybe [Int] -> Text -> Widget ResourceName
withTextSel =
    foldTextSel $ \mix t ->
        case mix of
            Nothing -> B.txt t
            Just ix_ ->
                withAttr selectedAttr $
                B.showCursor textCursorName (B.Location (ix_, 0)) $ B.txt t

withTextFieldSel :: Maybe [Int] -> Text -> Widget ResourceName
withTextFieldSel =
    foldTextFieldSel $ \mixs t ->
        let ls = T.splitOn "\n" t
            textOrSpace t_ =
                if T.null t
                    then B.txt " "
                    else B.txt t_
            tw = B.vBox $ map textOrSpace ls
        in case mixs of
               Nothing -> tw
               Just (yix_, xix_) ->
                   withAttr selectedAttr $
                   B.showCursor textCursorName (B.Location (xix_, yix_)) tw
