{-# LANGUAGE DataKinds #-}

module RVV.Synthesizer.Specification.CountNumSplitted (fullSplit) where

import Data.Foldable (Foldable (toList))
import Grisette (WordN)
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
  )
import HieraSynth.Reasoning.Parallel.Scheduler.DCTree
  ( DCTree,
    NodeId,
    emptyDCTree,
    insertRootSketch,
    insertSplittedSketches,
    nodeSketchSpec,
  )
import RVV.Synthesizer.Op
  ( SketchSpecTable,
  )

fullSplit :: SketchSpecTable (WordN 8) -> DCTree (SketchSpecTable (WordN 8))
fullSplit table = helper [root] [] newLattice
  where
    (root, newLattice) = insertRootSketch emptyDCTree table
    helper ::
      [NodeId] ->
      [NodeId] ->
      DCTree (SketchSpecTable (WordN 8)) ->
      DCTree (SketchSpecTable (WordN 8))
    helper [] [] l = l
    helper [] lst l = helper lst [] l
    helper (nid : rest) lst l =
      let sketch = nodeSketchSpec l nid
          seqNum = lowestSeqNum True sketch
       in case seqNum of
            Nothing -> helper rest lst l
            Just seqNum ->
              let sketches =
                    filter (/= sketch) $ partitionSpec seqNum sketch
                  (added, newl) = insertSplittedSketches l nid sketches
               in if null sketches
                    then helper rest lst l
                    else
                      helper rest (toList added ++ lst) newl
