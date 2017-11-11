{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FuzzyTimeSpec
    ( spec
    ) where

import TestImport

import Data.FuzzyTime

spec :: Spec
spec =
    describe "parseFuzzyDateTime" $ do
        let lit s fd =
                it (unwords ["parses", show s, "as", show fd]) $
                parseFuzzyDateTime s `shouldBe` Just fd
        let litR i s fd = mapM_ (`lit` fd) $ drop i $ inits s
        litR 1 "yesterday" Yesterday
        litR 3 "today" Today
        litR 3 "tomorrow" Tomorrow
        litR 1 "now" Now
