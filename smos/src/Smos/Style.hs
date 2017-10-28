{-# LANGUAGE OverloadedStrings #-}

module Smos.Style
    ( defaultAttrMap
    , selectedAttr
    , headerAttr
    , todoStateAttr
    , todoStateSpecificAttr
    -- * Names of widgets
    , headerCursorName
    -- * Re-exports
    , applyAttrMappings
    , fg
    , bg
    , module Graphics.Vty.Attributes
    ) where

import Import

import Brick.AttrMap as B
import Brick.Util as B
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes

import Smos.Types

defaultAttrMap :: s -> AttrMap
defaultAttrMap _ =
    attrMap defAttr [(selectedAttr, fg V.white), (headerAttr, fg V.yellow)]

selectedAttr :: AttrName
selectedAttr = "selected"

headerAttr :: AttrName
headerAttr = "header"

todoStateAttr :: AttrName
todoStateAttr = "todostate"

todoStateSpecificAttr :: String -> AttrName
todoStateSpecificAttr tss = fromString $ "todostate-" ++ tss

headerCursorName :: ResourceName
headerCursorName = "header-cursor"
