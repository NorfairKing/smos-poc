{-# LANGUAGE OverloadedStrings #-}

module Cursor.ListElemSpec
    ( spec
    ) where

import TestImport

import qualified Data.List.NonEmpty as NE

import Cursor.ListElem
import Cursor.ListElem.Gen ()

spec :: Spec
spec =
    describe "rebuildListElemCursor" $
    it "is the inverse of makeNonEmptyListElemCursor for integers" $
    inverseFunctionsIfFirstSucceeds
        (makeNonEmptyListElemCursor :: [Int] -> Maybe (ListElemCursor Int))
        (NE.toList . rebuildListElemCursor)
