module Import
    ( module X
    ) where

import Prelude as X hiding (head, init, last, tail)

import Control.Monad.IO.Class as X

import Path as X
import Path.IO as X
