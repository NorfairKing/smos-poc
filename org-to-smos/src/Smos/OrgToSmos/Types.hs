module Smos.OrgToSmos.Types where

import qualified Data.Text as T
import Import

data Flags =
    Flags
    deriving (Show, Eq)

newtype Conversion =
    Conversion Options
    deriving (Show, Eq)

data Options = Options
    { source :: String
    , target :: String
    , verbosity :: Verbosity
    } deriving (Show, Eq)

data Verbosity
    = Normal
    | Verbose
    deriving (Eq, Show)

data OrgDoc = OrgDoc
    { metadata :: [Metadata]
    , headlines :: [Headline]
    } deriving (Eq, Show)

newtype Metadata =
    Metadata T.Text
    deriving (Eq, Show)

data Headline = Headline
    { tier :: Tier
    , state :: Status
    , text :: T.Text
    } deriving (Eq, Show)

newtype Status = Status
    { keyword :: T.Text
    } deriving (Eq, Show)

newtype Tier =
    Tier Int
    deriving (Eq, Show)
