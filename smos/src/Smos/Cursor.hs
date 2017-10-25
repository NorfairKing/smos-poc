{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor
    ( ACursor(..)
    , makeACursor
    , makeASelection
    , Rebuild(..)
    , Build(..)
    , ForestCursor
    , makeForestCursor
    , forestCursorParent
    , forestCursorElems
    , forestCursorSelectIx
    , forestCursorSelectFirst
    , forestCursorSelectLast
    , forestCursorInsertAt
    , forestCursorInsertAtStart
    , forestCursorInsertAtEnd
    , TreeCursor
    , treeCursorParent
    , treeCursorPrevElemens
    , treeCursorNextElemens
    , treeCursorEntry
    , treeCursorForest
    , treeCursorSelectPrev
    , treeCursorSelectNext
    , treeCursorModifyEntry
    , treeCursorInsertAbove
    , treeCursorInsertUnder
    , treeCursorDeleteCurrent
    , EntryCursor
    , entryCursorParent
    , entryCursorHeader
    , entryCursorModifyHeader
    , HeaderCursor
    , headerCursor
    , headerCursorParent
    , headerCursorHeader
    , headerCursorModifyTextCursor
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import Data.Time

import Smos.Data
import Smos.TextCursor

class Rebuild a where
    rebuild :: a -> SmosForest

class Build a where
    type Building a :: *
    build :: a -> Building a

data ACursor
    = AnEntry EntryCursor
    | AHeader HeaderCursor

makeACursor :: SmosFile -> Maybe ACursor
makeACursor SmosFile {..} =
    (AnEntry . treeCursorEntry) <$>
    forestCursorSelectFirst (makeForestCursor smosFileForest)

makeASelection :: ACursor -> [Int]
makeASelection = reverse . go
  where
    go (AnEntry ec) = goe ec
    go (AHeader ec) = goh ec
    gof ForestCursor {..} = maybe [] ((1 :) . got) forestCursorParent
    got TreeCursor {..} = treeCursorIndex : gof treeCursorParent
    goe EntryCursor {..} = 0 : got entryCursorParent
    goh HeaderCursor {..} = 0 : goe headerCursorParent

instance Rebuild ACursor where
    rebuild (AnEntry ec) = rebuild ec
    rebuild (AHeader ec) = rebuild ec

data ForestCursor = ForestCursor
    { forestCursorParent :: Maybe TreeCursor
    , forestCursorElems :: [TreeCursor]
    }

instance Rebuild ForestCursor where
    rebuild fc =
        case forestCursorParent fc of
            Nothing -> build fc
            Just pc -> rebuild pc

instance Build ForestCursor where
    type Building ForestCursor = SmosForest
    build = SmosForest . map build . forestCursorElems

makeForestCursor :: SmosForest -> ForestCursor
makeForestCursor = forestCursor Nothing

forestCursor :: Maybe TreeCursor -> SmosForest -> ForestCursor
forestCursor mpar sf = fc
  where
    fc =
        ForestCursor
        { forestCursorParent = mpar
        , forestCursorElems = treeElems fc $ smosTrees sf
        }

forestModifyElems ::
       ([TreeCursor] -> [TreeCursor]) -> ForestCursor -> ForestCursor
forestModifyElems func ForestCursor {..} = fc'
  where
    fc' =
        ForestCursor
        { forestCursorParent =
              rebuildForestParentCursor (const fc') forestCursorParent
        , forestCursorElems = func forestCursorElems
        }

rebuildForestParentCursor ::
       (ForestCursor -> ForestCursor) -> Maybe TreeCursor -> Maybe TreeCursor
rebuildForestParentCursor func = fmap $ treeCursorModifyForest func

forestCursorSelectIx :: ForestCursor -> Int -> Maybe TreeCursor
forestCursorSelectIx fc = atMay $ forestCursorElems fc

forestCursorSelectFirst :: ForestCursor -> Maybe TreeCursor
forestCursorSelectFirst fc =
    case forestCursorElems fc of
        [] -> Nothing
        (tc:_) -> Just tc

forestCursorSelectLast :: ForestCursor -> Maybe TreeCursor
forestCursorSelectLast fc =
    case reverse $ forestCursorElems fc of
        [] -> Nothing
        (tc:_) -> Just tc

forestCursorInsertAt :: ForestCursor -> Int -> SmosTree -> ForestCursor
forestCursorInsertAt fc ix newTree = fc'
  where
    fc' =
        forestModifyElems
            (\els ->
                 treeElems fc' $
                 map build (prevs els) ++ [newTree] ++ map build (nexts els))
            fc
    ffilter rel = filter ((`rel` ix) . treeCursorIndex)
    prevs = ffilter (<)
    nexts = ffilter (>=)

forestCursorInsertAtStart :: ForestCursor -> SmosTree -> ForestCursor
forestCursorInsertAtStart fc = forestCursorInsertAt fc 0

forestCursorInsertAtEnd :: ForestCursor -> SmosTree -> ForestCursor
forestCursorInsertAtEnd fc =
    forestCursorInsertAt fc $ length $ forestCursorElems fc

data TreeCursor = TreeCursor
    { treeCursorParent :: ForestCursor
    , treeCursorPrevElemens :: [TreeCursor] -- ^ In reverse order, so that the first element is the nearest.
    , treeCursorNextElemens :: [TreeCursor]
    , treeCursorIndex :: Int
    , treeCursorEntry :: EntryCursor
    , treeCursorForest :: ForestCursor
    }

instance Rebuild TreeCursor where
    rebuild = rebuild . treeCursorParent

instance Build TreeCursor where
    type Building TreeCursor = SmosTree
    build TreeCursor {..} =
        SmosTree
        {treeEntry = build treeCursorEntry, treeForest = build treeCursorForest}

treeCursorModifyEntry ::
       (EntryCursor -> EntryCursor) -> TreeCursor -> TreeCursor
treeCursorModifyEntry func = treeCursorModify func id

treeCursorModifyForest ::
       (ForestCursor -> ForestCursor) -> TreeCursor -> TreeCursor
treeCursorModifyForest = treeCursorModify id

treeCursorModify ::
       (EntryCursor -> EntryCursor)
    -> (ForestCursor -> ForestCursor)
    -> TreeCursor
    -> TreeCursor
treeCursorModify efunc ffunc tc = tc''
  where
    tc' =
        tc
        { treeCursorEntry = efunc $ treeCursorEntry tc
        , treeCursorForest = ffunc $ treeCursorForest tc
        }
    tcs =
        reverse (treeCursorPrevElemens tc) ++ [tc'] ++ treeCursorNextElemens tc
    trees = map build tcs
    els = treeElems (forestModifyElems (const els) (treeCursorParent tc)) trees
    tc'' = els !! treeCursorIndex tc

treeElems :: ForestCursor -> [SmosTree] -> [TreeCursor]
treeElems fc sts = tcs
  where
    tcs = zipWith tc [0 ..] sts
    tc i st = cur
      where
        cur =
            TreeCursor
            { treeCursorParent = fc
            , treeCursorPrevElemens =
                  reverse $ filter ((< i) . treeCursorIndex) tcs
            , treeCursorNextElemens = filter ((> i) . treeCursorIndex) tcs
            , treeCursorIndex = i
            , treeCursorEntry = entryCursor cur $ treeEntry st
            , treeCursorForest = fc'
            }
        fc' = forestCursor (Just cur) (treeForest st)

treeCursorSelectPrev :: TreeCursor -> Maybe TreeCursor
treeCursorSelectPrev tc =
    case treeCursorPrevElemens tc of
        [] -> Nothing
        (tc':_) -> Just tc'

treeCursorSelectNext :: TreeCursor -> Maybe TreeCursor
treeCursorSelectNext tc =
    case treeCursorNextElemens tc of
        [] -> Nothing
        (tc':_) -> Just tc'

treeCursorInsertAbove :: TreeCursor -> SmosTree -> TreeCursor
treeCursorInsertAbove tc t = fromJust $ forestCursorSelectIx newpar newIx
  where
    newIx = treeCursorIndex tc
    newpar = forestCursorInsertAt (treeCursorParent tc) newIx t

treeCursorInsertUnder :: TreeCursor -> SmosTree -> TreeCursor
treeCursorInsertUnder tc t =
    fromJust $ forestCursorSelectIx newpar $ treeCursorIndex tc + 1
  where
    newIx = treeCursorIndex tc + 1
    newpar = forestCursorInsertAt (treeCursorParent tc) newIx t

treeCursorDeleteCurrent :: TreeCursor -> Either ForestCursor TreeCursor
treeCursorDeleteCurrent tc = tc''
  where
    tcs = reverse (treeCursorPrevElemens tc) ++ treeCursorNextElemens tc
    trees = map build tcs
    for = forestModifyElems (const els) (treeCursorParent tc)
    els = treeElems for trees
    tc'' =
        let ix = treeCursorIndex tc
        in maybe (Left for) Right $
           (els `atMay` ix) `mplus` (els `atMay` (ix - 1))

data EntryCursor = EntryCursor
    { entryCursorParent :: TreeCursor
    , entryCursorHeader :: HeaderCursor
    , entryCursorContents :: Maybe Contents
    , entryCursorTimestamps :: HashMap TimestampName UTCTime
    , entryCursorState :: Maybe TodoState
    , entryCursorTags :: [Tag]
    , entryCursorLogbook :: Logbook
    }

instance Rebuild EntryCursor where
    rebuild = rebuild . entryCursorParent

instance Build EntryCursor where
    type Building EntryCursor = Entry
    build EntryCursor {..} =
        Entry
        { entryHeader = build entryCursorHeader
        , entryContents = entryCursorContents
        , entryTimestamps = entryCursorTimestamps
        , entryState = entryCursorState
        , entryTags = entryCursorTags
        , entryLogbook = entryCursorLogbook
        }

entryCursor :: TreeCursor -> Entry -> EntryCursor
entryCursor par Entry {..} = ec
  where
    ec =
        EntryCursor
        { entryCursorParent = par
        , entryCursorHeader = headerCursor ec entryHeader
        , entryCursorContents = entryContents
        , entryCursorTimestamps = entryTimestamps
        , entryCursorState = entryState
        , entryCursorTags = entryTags
        , entryCursorLogbook = entryLogbook
        }

entryCursorModifyHeader ::
       (HeaderCursor -> HeaderCursor) -> EntryCursor -> EntryCursor
entryCursorModifyHeader func ec@EntryCursor {..} = ec'
  where
    ec' =
        ec
        { entryCursorParent =
              treeCursorModifyEntry (const ec') entryCursorParent
        , entryCursorHeader = func entryCursorHeader
        }

data HeaderCursor = HeaderCursor
    { headerCursorParent :: EntryCursor
    , headerCursorHeader :: TextCursor
    }

instance Rebuild HeaderCursor where
    rebuild = rebuild . headerCursorParent

instance Build HeaderCursor where
    type Building HeaderCursor = Header
    build HeaderCursor {..} = Header $ rebuildTextCursor headerCursorHeader

headerCursor :: EntryCursor -> Header -> HeaderCursor
headerCursor par h =
    HeaderCursor
    { headerCursorParent = par
    , headerCursorHeader = makeTextCursor $ headerText h
    }

headerCursorModifyTextCursor ::
       (TextCursor -> TextCursor) -> HeaderCursor -> HeaderCursor
headerCursorModifyTextCursor func HeaderCursor {..} = hc
  where
    hc =
        HeaderCursor
        { headerCursorParent =
              entryCursorModifyHeader (const hc) headerCursorParent
        , headerCursorHeader = func headerCursorHeader
        }
