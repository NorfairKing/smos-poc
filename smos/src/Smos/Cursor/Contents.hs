module Smos.Cursor.Contents
    ( entryCursorContentsML
    , emptyContentsCursor
    , makeContentsCursor
    , contentsCursorTextFieldL
    , contentsCursorSetContents
    , contentsCursorContentsL
    , contentsCursorInsert
    , contentsCursorAppend
    , contentsCursorNewline
    , contentsCursorRemove
    , contentsCursorDelete
    , contentsCursorLeft
    , contentsCursorRight
    , contentsCursorUp
    , contentsCursorDown
    , contentsCursorStart
    , contentsCursorEnd
    ) where

import Import

import qualified Data.Text as T

import Lens.Micro

import Cursor.Class
import Cursor.Select
import Cursor.TextField

import Smos.Cursor.Entry.Contents
import Smos.Cursor.Types
import Smos.Data

entryCursorContentsML :: Lens' EntryCursor (Maybe Contents)
entryCursorContentsML =
    lens (fmap (source . selectValue . build) . entryCursorContents) setter
  where
    setter ec mc = ec'
      where
        ec' = ec & entryCursorContentsL .~ ((contentsCursor ec' . view) <$> mc)

emptyContentsCursor :: EntryCursor -> ContentsCursor
emptyContentsCursor ec = makeContentsCursor ec $ Contents T.empty

makeContentsCursor :: EntryCursor -> Contents -> ContentsCursor
makeContentsCursor ec cts = contentsCursor ec $ view cts

contentsCursorTextFieldL ::
       Functor f
    => (TextFieldCursor -> f TextFieldCursor)
    -> ContentsCursor
    -> f ContentsCursor
contentsCursorTextFieldL = lens getter setter
  where
    getter = contentsCursorContents
    setter cc tfc = cc'
      where
        ec' = contentsCursorParent cc & entryCursorContentsL .~ Just cc'
        cc' = cc {contentsCursorParent = ec', contentsCursorContents = tfc}

contentsCursorSetContents :: Contents -> ContentsCursor -> ContentsCursor
contentsCursorSetContents cs =
    contentsCursorTextFieldL .~ makeTextFieldCursor (contentsText cs)

contentsCursorContentsL :: Lens' ContentsCursor Contents
contentsCursorContentsL =
    lens (source . selectValue . build) $ flip contentsCursorSetContents

contentsCursorInsert :: Char -> ContentsCursor -> ContentsCursor
contentsCursorInsert c = contentsCursorTextFieldL %~ textFieldCursorInsert c

contentsCursorAppend :: Char -> ContentsCursor -> ContentsCursor
contentsCursorAppend c = contentsCursorTextFieldL %~ textFieldCursorAppend c

contentsCursorNewline :: ContentsCursor -> ContentsCursor
contentsCursorNewline = contentsCursorTextFieldL %~ textFieldCursorNewline

contentsCursorRemove :: ContentsCursor -> Maybe ContentsCursor
contentsCursorRemove = contentsCursorTextFieldL textFieldCursorRemove

contentsCursorDelete :: ContentsCursor -> Maybe ContentsCursor
contentsCursorDelete = contentsCursorTextFieldL textFieldCursorDelete

contentsCursorLeft :: ContentsCursor -> Maybe ContentsCursor
contentsCursorLeft = contentsCursorTextFieldL textFieldCursorSelectPrev

contentsCursorRight :: ContentsCursor -> Maybe ContentsCursor
contentsCursorRight = contentsCursorTextFieldL textFieldCursorSelectNext

contentsCursorUp :: ContentsCursor -> Maybe ContentsCursor
contentsCursorUp = contentsCursorTextFieldL textFieldCursorSelectUp

contentsCursorDown :: ContentsCursor -> Maybe ContentsCursor
contentsCursorDown = contentsCursorTextFieldL textFieldCursorSelectDown

contentsCursorStart :: ContentsCursor -> ContentsCursor
contentsCursorStart = contentsCursorTextFieldL %~ textFieldCursorSelectStart

contentsCursorEnd :: ContentsCursor -> ContentsCursor
contentsCursorEnd = contentsCursorTextFieldL %~ textFieldCursorSelectEnd
