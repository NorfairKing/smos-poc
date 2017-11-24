module Data.FuzzyTime.ResolveSpec
    ( spec
    ) where

import TestImport

import Data.FuzzyTime.Resolve

import Data.FuzzyTime.FuzzyTypes.Gen ()

spec :: Spec
spec =
    describe "resolveDay" $
    it "produces valid days" $ producesValidsOnValids2 resolveDay
