{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor
    ( ACursor(..)
    , makeACursor
    , Rebuild(..)
    , Build(..)
    , ForestCursor
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
    ) where

import Import

import Smos.Data

class Rebuild a where
    rebuild :: a -> SmosForest

class Build a where
    type Building a :: *
    build :: a -> Building a

data ACursor
    = AForest ForestCursor
    | ATree TreeCursor

makeACursor :: SmosFile -> ACursor
makeACursor SmosFile {..} = AForest $ makeForestCursor smosFileForest

instance Rebuild ACursor where
    rebuild (AForest fc) = rebuild fc
    rebuild (ATree tc) = rebuild tc

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

forestCursorInsertAt :: ForestCursor -> Int -> Entry -> ForestCursor -- TODO change 'Entry' -> 'SmosTree'
forestCursorInsertAt fc ix e = fc'
  where
    fc' = forestModifyElems (\els -> prevs els ++ [newTc els] ++ nexts els) fc
    ffilter rel = filter ((`rel` ix) . treeCursorIndex)
    prevs = ffilter (<)
    nexts = ffilter (>=)
    newTc els =
        let tc =
                TreeCursor
                { treeCursorParent = fc'
                , treeCursorPrevElemens = reverse $ prevs els
                , treeCursorNextElemens = nexts els
                , treeCursorIndex = ix
                , treeCursorEntry = e
                , treeCursorForest = forestCursor (Just tc) (SmosForest [])
                }
        in tc

forestCursorInsertAtStart :: ForestCursor -> Entry -> ForestCursor
forestCursorInsertAtStart fc = forestCursorInsertAt fc 0

forestCursorInsertAtEnd :: ForestCursor -> Entry -> ForestCursor
forestCursorInsertAtEnd fc =
    forestCursorInsertAt fc $ length $ forestCursorElems fc

data TreeCursor = TreeCursor
    { treeCursorParent :: ForestCursor
    , treeCursorPrevElemens :: [TreeCursor] -- ^ In reverse order, so that the first element is the nearest.
    , treeCursorNextElemens :: [TreeCursor]
    , treeCursorIndex :: Int
    , treeCursorEntry :: Entry
    , treeCursorForest :: ForestCursor
    }

instance Rebuild TreeCursor where
    rebuild = rebuild . treeCursorParent

instance Build TreeCursor where
    type Building TreeCursor = SmosTree
    build TreeCursor {..} =
        SmosTree
        {treeEntry = treeCursorEntry, treeForest = build treeCursorForest}

treeCursorModifyEntry :: (Entry -> Entry) -> TreeCursor -> TreeCursor
treeCursorModifyEntry func = treeCursorModify func id

treeCursorModifyForest ::
       (ForestCursor -> ForestCursor) -> TreeCursor -> TreeCursor
treeCursorModifyForest = treeCursorModify id

treeCursorModify ::
       (Entry -> Entry)
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
            , treeCursorEntry = treeEntry st
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

treeCursorInsertAbove :: TreeCursor -> Entry -> TreeCursor
treeCursorInsertAbove tc e = fromJust $ forestCursorSelectIx newpar newIx
  where
    newIx = treeCursorIndex tc
    newpar = forestCursorInsertAt (treeCursorParent tc) newIx e

treeCursorInsertUnder :: TreeCursor -> Entry -> TreeCursor
treeCursorInsertUnder tc e =
    fromJust $ forestCursorSelectIx newpar $ treeCursorIndex tc + 1
  where
    newIx = treeCursorIndex tc + 1
    newpar = forestCursorInsertAt (treeCursorParent tc) newIx e
