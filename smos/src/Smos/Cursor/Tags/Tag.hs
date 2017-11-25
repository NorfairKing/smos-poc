module Smos.Cursor.Tags.Tag
    ( tagsCursorTagCursorsL
    ) where

import Import

import Lens.Micro

import Smos.Cursor.Entry.Tags
import Smos.Cursor.Types

tagsCursorTagCursorsL ::
       Functor f => ([TagCursor] -> f [TagCursor]) -> TagsCursor -> f TagsCursor
tagsCursorTagCursorsL = lens getter setter
  where
    getter = tagsCursorTags
    setter cc ts = cc'
      where
        ec' = tagsCursorParent cc & entryCursorTagsL .~ cc'
        cc' = cc {tagsCursorParent = ec', tagsCursorTags = ts}
