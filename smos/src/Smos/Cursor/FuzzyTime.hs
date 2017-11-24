{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.FuzzyTime
    ( FuzzyTimeCursor
    , fuzzyTimeCursorTextCursor
    , fuzzyTimeCursorCurrent
    , fuzzyTimeCursorResolve
    , fuzzyTimeCursorIndex
    , fuzzyTimeCursorSelectPrev
    , fuzzyTimeCursorSelectNext
    , fuzzyTimeCursorSelectStart
    , fuzzyTimeCursorSelectEnd
    , fuzzyTimeCursorInsert
    , fuzzyTimeCursorAppend
    , fuzzyTimeCursorRemove
    , fuzzyTimeCursorDelete
    ) where

import Import

import Lens.Micro

import qualified Data.Text as T
import Data.Time
import Text.Megaparsec

import Data.FuzzyTime

import Cursor.Class
import Cursor.Text

data FuzzyTimeCursor = FuzzyTimeCursor
    { fuzzyTimeCursorTextCursor :: TextCursor
    , fuzzyTimeCursorCurrent :: ZonedTime
    } deriving (Show, Generic)

instance Eq FuzzyTimeCursor where
    (==) = (==) `on` fuzzyTimeCursorTextCursor

fuzzyTimeCursorResolve :: ZonedTime -> FuzzyTimeCursor -> ZonedTime
fuzzyTimeCursorResolve now FuzzyTimeCursor {..} =
    fromMaybe fuzzyTimeCursorCurrent $
    resolveZonedTime now <$>
    parseMaybe
        fuzzyZonedTimeP
        (T.unpack $ source $ rebuild fuzzyTimeCursorTextCursor)

fuzzyTimeCursorTextCursorL ::
       Functor f
    => (TextCursor -> f TextCursor)
    -> FuzzyTimeCursor
    -> f FuzzyTimeCursor
fuzzyTimeCursorTextCursorL =
    lens
        fuzzyTimeCursorTextCursor
        (\ftc tc -> ftc {fuzzyTimeCursorTextCursor = tc})

fuzzyTimeCursorIndex :: FuzzyTimeCursor -> Int
fuzzyTimeCursorIndex = textCursorIndex . fuzzyTimeCursorTextCursor

fuzzyTimeCursorSelectPrev :: FuzzyTimeCursor -> Maybe FuzzyTimeCursor
fuzzyTimeCursorSelectPrev = fuzzyTimeCursorTextCursorL textCursorSelectPrev

fuzzyTimeCursorSelectNext :: FuzzyTimeCursor -> Maybe FuzzyTimeCursor
fuzzyTimeCursorSelectNext = fuzzyTimeCursorTextCursorL textCursorSelectNext

fuzzyTimeCursorSelectStart :: FuzzyTimeCursor -> FuzzyTimeCursor
fuzzyTimeCursorSelectStart = fuzzyTimeCursorTextCursorL %~ textCursorSelectStart

fuzzyTimeCursorSelectEnd :: FuzzyTimeCursor -> FuzzyTimeCursor
fuzzyTimeCursorSelectEnd = fuzzyTimeCursorTextCursorL %~ textCursorSelectEnd

fuzzyTimeCursorInsert :: Char -> FuzzyTimeCursor -> FuzzyTimeCursor
fuzzyTimeCursorInsert c = fuzzyTimeCursorTextCursorL %~ textCursorInsert c

fuzzyTimeCursorAppend :: Char -> FuzzyTimeCursor -> FuzzyTimeCursor
fuzzyTimeCursorAppend c = fuzzyTimeCursorTextCursorL %~ textCursorAppend c

fuzzyTimeCursorRemove :: FuzzyTimeCursor -> Maybe FuzzyTimeCursor
fuzzyTimeCursorRemove = fuzzyTimeCursorTextCursorL textCursorRemove

fuzzyTimeCursorDelete :: FuzzyTimeCursor -> Maybe FuzzyTimeCursor
fuzzyTimeCursorDelete = fuzzyTimeCursorTextCursorL textCursorDelete
