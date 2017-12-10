module Smos.Cursor.Tags.Tag
    ( tagsCursorTagCursorsL
    ) where

import Import

import Lens.Micro

import Cursor.ListElem

import Smos.Cursor.Entry.Tags
import Smos.Cursor.Types

tagsCursorTagCursorsL :: Lens' TagsCursor (ListElemCursor TagCursor)
tagsCursorTagCursorsL = lens getter setter
  where
    getter = tagsCursorTags
    setter cc ts = cc'
      where
        ec' = tagsCursorParent cc & entryCursorTagsL .~ Just cc'
        cc' = cc {tagsCursorParent = ec', tagsCursorTags = ts}
