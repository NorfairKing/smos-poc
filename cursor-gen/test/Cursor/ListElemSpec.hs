{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.ListElemSpec
    ( spec
    ) where

import TestImport

import qualified Data.List.NonEmpty as NE

import Cursor.Class
import Cursor.ListElem
import Cursor.ListElem.Gen ()
import Cursor.TestUtils

spec :: Spec
spec =
    describe "rebuildListElemCursor" $
    it "is the inverse of makeNonEmptyListElemCursor for integers" $
    inverseFunctionsIfFirstSucceeds
        (makeNonEmptyListElemCursor :: [Int] -> Maybe (ListElemCursor Int))
        (NE.toList . rebuildListElemCursor)
