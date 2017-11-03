{-# LANGUAGE OverloadedStrings #-}

module Smos.Style
    ( defaultAttrMap
    , selectedAttr
    , headerAttr
    , contentsAttr
    , todoStateAttr
    , todoStateSpecificAttr
    , tagAttr
    , tagSpecificAttr
    -- * Names of widgets
    , textCursorName
    -- * Re-exports
    , applyAttrMappings
    , fg
    , bg
    , module Graphics.Vty.Attributes
    ) where

import Import

import qualified Data.Text as T

import Brick.AttrMap as B
import Brick.Util as B
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes

import Smos.Data
import Smos.Types

defaultAttrMap :: s -> AttrMap
defaultAttrMap _ =
    attrMap defAttr [(selectedAttr, fg V.white), (headerAttr, fg V.yellow)]

selectedAttr :: AttrName
selectedAttr = "selected"

headerAttr :: AttrName
headerAttr = "header"

contentsAttr :: AttrName
contentsAttr = "contents"

todoStateAttr :: AttrName
todoStateAttr = "todostate"

todoStateSpecificAttr :: TodoState -> AttrName
todoStateSpecificAttr tss =
    fromString $ "todostate-" ++ T.unpack (todoStateText tss)

tagAttr :: AttrName
tagAttr = "tag"

tagSpecificAttr :: Tag -> AttrName
tagSpecificAttr t = fromString $ "tag-" ++ T.unpack (tagText t)

textCursorName :: ResourceName
textCursorName = "text-cursor"
