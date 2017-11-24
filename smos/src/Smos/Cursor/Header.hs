module Smos.Cursor.Header
    ( HeaderCursor(..)
    , makeHeaderCursor
    , headerCursorTextCursorL
    , headerCursorHeaderL
    , headerCursorSetHeader
    , headerCursorInsert
    , headerCursorAppend
    , headerCursorRemove
    , headerCursorDelete
    , headerCursorLeft
    , headerCursorRight
    , headerCursorStart
    , headerCursorEnd
    ) where

import Import

import Lens.Micro

import Cursor.Class
import Cursor.Select
import Cursor.Text

import Smos.Cursor.Entry.Header
import Smos.Cursor.Types
import Smos.Data

makeHeaderCursor :: EntryCursor -> Header -> HeaderCursor
makeHeaderCursor par h = headerCursor par $ view h

headerCursorTextCursorL ::
       Functor f
    => (TextCursor -> f TextCursor)
    -> HeaderCursor
    -> f HeaderCursor
headerCursorTextCursorL = lens getter setter
  where
    getter = headerCursorHeader
    setter hc tc = hc'
      where
        ec' = headerCursorParent hc & entryCursorHeaderL .~ hc'
        hc' = HeaderCursor {headerCursorParent = ec', headerCursorHeader = tc}

headerCursorHeaderL ::
       Functor f => (Header -> f Header) -> HeaderCursor -> f HeaderCursor
headerCursorHeaderL =
    lens (source . selectValue . build) $ flip headerCursorSetHeader

headerCursorSetHeader :: Header -> HeaderCursor -> HeaderCursor
headerCursorSetHeader h hc =
    hc & headerCursorTextCursorL .~ makeTextCursor (headerText h)

headerCursorInsert :: Char -> HeaderCursor -> HeaderCursor
headerCursorInsert c = headerCursorTextCursorL %~ textCursorInsert c

headerCursorAppend :: Char -> HeaderCursor -> HeaderCursor
headerCursorAppend c = headerCursorTextCursorL %~ textCursorAppend c

headerCursorRemove :: HeaderCursor -> Maybe HeaderCursor
headerCursorRemove = headerCursorTextCursorL textCursorRemove

headerCursorDelete :: HeaderCursor -> Maybe HeaderCursor
headerCursorDelete = headerCursorTextCursorL textCursorDelete

headerCursorLeft :: HeaderCursor -> Maybe HeaderCursor
headerCursorLeft = headerCursorTextCursorL textCursorSelectPrev

headerCursorRight :: HeaderCursor -> Maybe HeaderCursor
headerCursorRight = headerCursorTextCursorL textCursorSelectNext

headerCursorStart :: HeaderCursor -> HeaderCursor
headerCursorStart = headerCursorTextCursorL %~ textCursorSelectStart

headerCursorEnd :: HeaderCursor -> HeaderCursor
headerCursorEnd = headerCursorTextCursorL %~ textCursorSelectEnd
