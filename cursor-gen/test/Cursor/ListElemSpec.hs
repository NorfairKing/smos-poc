{-# LANGUAGE OverloadedStrings #-}

module Cursor.ListElemSpec
    ( spec
    ) where

import TestImport

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Cursor.ListElem
import Cursor.ListElem.Gen ()

spec :: Spec
spec =
    describe "rebuildListElemCursor" $ do
        it "is the inverse of makeListElemCursor for integers" $
            inverseFunctions
                (makeListElemCursor :: NonEmpty Int -> ListElemCursor Int)
                rebuildListElemCursor
        it "is the inverse of makeNonEmptyListElemCursor for integers" $
            inverseFunctionsIfFirstSucceeds
                (makeNonEmptyListElemCursor :: [Int] -> Maybe (ListElemCursor Int))
                (NE.toList . rebuildListElemCursor)
        it "is the inverse of makeListElemCursorWithSelection for integers, for any index" $
            forAll genUnchecked $ \i ->
                inverseFunctions
                    (makeListElemCursorWithSelection i :: NonEmpty Int -> ListElemCursor Int)
                    rebuildListElemCursor
