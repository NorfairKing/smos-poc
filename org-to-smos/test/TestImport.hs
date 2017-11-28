module TestImport
    ( module X
    ) where

import Prelude as X hiding (head, init, last, tail)

import GHC.Generics as X hiding (Selector)

import Test.Hspec as X
