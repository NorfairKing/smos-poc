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

import Smos.Data

import Smos.Cursor
import Smos.Style
import Smos.Types

smosDraw :: SmosState -> [Widget ResourceName]
smosDraw SmosState {..} =
    [fromMaybe drawNoContent $ renderCursor <$> smosStateCursor]
  where
    renderCursor :: ACursor -> Widget ResourceName
    renderCursor cur = drawForest sel for
      where
        sel = Just $ makeASelection $ selectAnyCursor cur
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

drawForest :: Maybe [Int] -> SmosForest -> Widget ResourceName
drawForest msel SmosForest {..} =
    padLeft (Pad 2) . B.vBox $
    flip map (zip [0 ..] smosTrees) $ \(ix, st) ->
        drawTree (drillSel msel ix) st

drawTree :: Maybe [Int] -> SmosTree -> Widget ResourceName
drawTree msel SmosTree {..} =
    drawEntry (drillSel msel 0) treeEntry <=>
    drawForest (drillSel msel 1) treeForest

drawEntry :: Maybe [Int] -> Entry -> Widget ResourceName
drawEntry msel Entry {..} =
    withSel msel $
    B.vBox
        [ B.hBox $
          intersperse (B.txt " ") $
          [B.txt ">"] ++
          (case entryState of
               Nothing -> []
               Just ts ->
                   [ withAttr todoStateAttr $
                     withAttr (todoStateSpecificAttr ts) $
                     B.txt $ todoStateText ts
                   ]) ++
          [ drawHeader (drillSel msel 0) entryHeader
          , B.hBox $ intersperse (B.txt ":") $ map (B.txt . tagText) entryTags
          ]
        , drawContents (drillSel msel 2) entryContents
        , B.vBox $
          flip map (HM.toList entryTimestamps) $ \(k, ts) ->
              B.hBox [B.txt $ timestampNameText k, B.txt ": ", drawTimestamp ts]
        , drawLogbook entryLogbook
        ]

drawTimestamp :: UTCTime -> Widget n
drawTimestamp = B.str . formatTime defaultTimeLocale "%F %R"

drawHeader :: Maybe [Int] -> Header -> Widget ResourceName
drawHeader msel Header {..} = withAttr headerAttr $ withTextSel msel headerText

drawContents :: Maybe [Int] -> Maybe Contents -> Widget ResourceName
drawContents msel mcon =
    case mcon of
        Nothing -> emptyWidget
        Just Contents {..} ->
            withAttr contentsAttr $ withTextFieldSel msel contentsText

drawLogbook :: Logbook -> Widget n
drawLogbook LogEnd = B.emptyWidget
drawLogbook (LogEntry b e l) =
    B.hBox [str "[", drawTimestamp b, str "]--[", drawTimestamp e, str "]"] <=>
    drawLogbook l
drawLogbook (LogOpenEntry b l) =
    B.hBox [str "[", drawTimestamp b, str "]"] <=> drawLogbook l

drillSel :: Maybe [Int] -> Int -> Maybe [Int]
drillSel msel ix =
    case msel of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (x:xs) ->
            if x == ix
                then Just xs
                else Nothing

withSel :: Maybe [Int] -> Widget n -> Widget n
withSel msel =
    case msel of
        Nothing -> id
        Just [] -> withAttr selectedAttr
        Just _ -> id

withTextSel :: Maybe [Int] -> Text -> Widget ResourceName
withTextSel msel t =
    case msel of
        Nothing -> B.txt t
        Just [ix_] ->
            withAttr selectedAttr $
            B.showCursor textCursorName (B.Location (ix_, 0)) $ B.txt t
        Just _ -> B.txt t

withTextFieldSel :: Maybe [Int] -> Text -> Widget ResourceName
withTextFieldSel msel t =
    let ls = T.splitOn "\n" t
        textOrSpace t =
            if T.null t
                then B.txt " "
                else B.txt t
        tw = B.vBox $ map textOrSpace ls
    in case msel of
           Nothing -> tw
           Just [xix_, yix_] ->
               withAttr selectedAttr $
               B.showCursor textCursorName (B.Location (xix_, yix_)) tw
           Just _ -> tw
