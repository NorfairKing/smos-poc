{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import

import qualified Data.HashMap.Lazy as HM
import Data.List
import qualified Data.Text as T
import Data.Time

import Brick.Types as B
import Brick.Widgets.Core as B

import Smos.Data

import Smos.Cursor
import Smos.Style
import Smos.Types

smosDraw :: SmosState -> [Widget ResourceName]
smosDraw SmosState {..} =
    [fromMaybe (str "NO CONTENT") $ renderForest <$> smosStateCursor]
  where
    renderForest cur = drawForest (Just $ makeASelection cur) (rebuild cur)

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
                     withAttr
                         (todoStateSpecificAttr (T.unpack $ todoStateText ts)) $
                     B.txt $ todoStateText ts
                   ]) ++
          [ drawHeader (drillSel msel 0) entryHeader
          , B.hBox $ intersperse (B.txt ":") $ map (B.txt . tagText) entryTags
          ]
        , mayW entryContents $ B.txt . contentsText
        , B.vBox $
          flip map (HM.toList entryTimestamps) $ \(k, ts) ->
              B.hBox [B.txt $ timestampNameText k, B.txt ": ", drawTimestamp ts]
        , drawLogbook entryLogbook
        ]

drawTimestamp :: UTCTime -> Widget n
drawTimestamp = B.str . show

drawHeader :: Maybe [Int] -> Header -> Widget ResourceName
drawHeader msel Header {..} = withAttr headerAttr $ withTextSel msel headerText

drawLogbook :: Logbook -> Widget n
drawLogbook LogEnd = B.emptyWidget
drawLogbook (LogEntry b e l) =
    B.hBox [drawTimestamp b, drawTimestamp e] <=> drawLogbook l
drawLogbook (LogOpenEntry b l) =
    B.hBox [drawTimestamp b, B.txt "present"] <=> drawLogbook l

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

mayW :: Maybe a -> (a -> Widget n) -> Widget n
mayW mw func = maybe emptyWidget func mw
