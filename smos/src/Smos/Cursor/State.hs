module Smos.Cursor.State
    ( StateCursor(..)
    , makeStateCursor
    , stateCursorStateL
    , stateCursorClear
    , stateCursorSetState
    ) where

import Import

import Data.Time

import Lens.Micro

import Cursor.Class

import Smos.Cursor.Entry.State
import Smos.Cursor.Types
import Smos.Data

makeStateCursor :: EntryCursor -> StateHistory -> StateCursor
makeStateCursor ec sh = stateCursor ec $ view sh

stateCursorStateL ::
       Functor f
    => UTCTime
    -> (Maybe TodoState -> f (Maybe TodoState))
    -> StateCursor
    -> f StateCursor
stateCursorStateL now = lens getter setter
  where
    getter = stateHistoryState . stateCursorStateHistory
    setter sc mts = sc'
      where
        sc' =
            StateCursor
            { stateCursorParent =
                  stateCursorParent sc & entryCursorStateL .~ sc'
            , stateCursorStateHistory =
                  stateHistorySetState now mts $ stateCursorStateHistory sc
            }

stateCursorClear :: UTCTime -> StateCursor -> StateCursor
stateCursorClear now sc = sc & stateCursorStateL now .~ Nothing

stateCursorSetState :: UTCTime -> TodoState -> StateCursor -> StateCursor
stateCursorSetState now ts sc = sc & stateCursorStateL now .~ Just ts
