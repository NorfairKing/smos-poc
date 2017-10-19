{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Gen where

import Import

import Smos.Data

instance GenUnchecked SmosFile

instance GenValid SmosFile

instance GenUnchecked Entry

instance GenValid Entry

instance GenUnchecked Contents

instance GenValid Contents

instance GenUnchecked TimestampName

instance GenValid TimestampName

instance GenUnchecked TodoState

instance GenValid TodoState

instance GenUnchecked Tag

instance GenValid Tag

instance GenUnchecked Logbook

instance GenValid Logbook
