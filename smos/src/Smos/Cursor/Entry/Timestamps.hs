module Smos.Cursor.Entry.Timestamps
    ( entryCursorTimestampsL
    , entryCursorTimestampsMapL
    , entryCursorTimestampsIndex
    ) where

import Import

import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import qualified Data.List.NonEmpty as NE

import Lens.Micro

import Cursor.Class
import Cursor.Tree

import Smos.Cursor.Types
import Smos.Data
import Smos.View

entryCursorTimestampsL :: Lens' EntryCursor (Maybe TimestampsCursor)
entryCursorTimestampsL = lens getter setter
  where
    getter = entryCursorTimestamps
    setter ec ts = ec'
      where
        ec' =
            ec
            { entryCursorParent = entryCursorParent ec & treeCursorValueL .~ ec'
            , entryCursorState = (entryCursorState ec) {stateCursorParent = ec'}
            , entryCursorHeader =
                  (entryCursorHeader ec) {headerCursorParent = ec'}
            , entryCursorContents =
                  (\ec_ -> ec_ {contentsCursorParent = ec'}) <$>
                  entryCursorContents ec
            , entryCursorTags =
                  (\ec_ -> ec_ {tagsCursorParent = ec'}) <$> entryCursorTags ec
            , entryCursorTimestamps = ts
            }

entryCursorTimestampsMapL :: Lens' EntryCursor (HashMap TimestampName Timestamp)
entryCursorTimestampsMapL = lens getter setter
  where
    getter ec =
        maybe
            HM.empty
            (HM.fromList .
             NE.toList . source . rebuild . timestampsCursorTimestamps) $
        ec ^. entryCursorTimestampsL
    setter :: EntryCursor -> HashMap TimestampName Timestamp -> EntryCursor
    setter ec hm =
        case NE.nonEmpty (HM.toList hm) of
            Nothing -> ec & entryCursorTimestampsL .~ Nothing
            Just ne ->
                ec & entryCursorTimestampsL .~
                Just (timestampsCursor ec $ TimestampsView $ view ne)

entryCursorTimestampsIndex :: Int
entryCursorTimestampsIndex = 4
