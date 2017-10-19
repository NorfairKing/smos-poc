{-# OPTIONS_GHC -fno-warn-orphans #-}
module Smos.Data.Gen where

import Import

import Smos.Data

instance GenUnchecked SmosFile

instance GenUnchecked Entry

instance GenUnchecked Contents

instance GenUnchecked TimestampName

instance GenUnchecked TodoState

instance GenUnchecked Tag

instance GenUnchecked Logbook
