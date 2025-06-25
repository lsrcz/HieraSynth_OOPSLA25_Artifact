{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module HieraSynth.Reasoning.Parallel.Scheduler.BiasedQueue
  ( BasePriority (..),
    Priority (..),
    BiasedQueue,
    empty,
    delete,
    setPriority,
    insert,
    size,
    HieraSynth.Reasoning.Parallel.Scheduler.BiasedQueue.null,
    popMin,
    recipBasePriority,
    recipPriority,
    minQueuedDepth,
    minSimplePriority,
    minBiasedPriority,
    queuedNodes,
    popSimpleMin,
    popBiasedMin,
  )
where

import qualified Data.HashPSQ as PSQ
import Grisette (PPrint, derive)
import HieraSynth.Reasoning.Parallel.Scheduler.DCTree (NodeId)
import System.Random.Stateful (AtomicGenM, StdGen, UniformRange (uniformRM))

data BasePriority = BasePriority {depth :: Int, randomPriority :: Double}

recipBasePriority :: BasePriority -> BasePriority
recipBasePriority BasePriority {depth, randomPriority} =
  BasePriority depth (recip randomPriority)

derive [''BasePriority] [''Show, ''Eq, ''PPrint]

instance Ord BasePriority where
  BasePriority d1 r1 <= BasePriority d2 r2 =
    if d1 == d2 then r1 <= r2 else d1 <= d2

data Priority = Priority
  { basePriority :: BasePriority,
    knownWorking :: Bool,
    knownWorkingAncestorDistance :: Maybe Int,
    ancestorSiblingKnownWorking :: Bool
  }

recipPriority :: Priority -> Priority
recipPriority Priority {..} =
  Priority
    (recipBasePriority basePriority)
    knownWorking
    knownWorkingAncestorDistance
    ancestorSiblingKnownWorking

derive [''Priority] [''Show, ''Eq, ''PPrint]

instance Ord Priority where
  Priority p1 k1 pk1 ask1 <= Priority p2 k2 pk2 ask2 =
    if k1 == k2
      then
        if pk1 == pk2
          then (if ask1 == ask2 then p1 <= p2 else ask1)
          else case (pk1, pk2) of
            (Nothing, _) -> False
            (Just _, Nothing) -> True
            (Just n1, Just n2) -> n1 <= n2
      else k1

data BiasedQueue = BiasedQueue
  { baseQueue :: PSQ.HashPSQ NodeId Priority NodeId,
    simpleQueue :: PSQ.HashPSQ NodeId BasePriority NodeId,
    biasProbability :: Double
  }

queuedNodes :: BiasedQueue -> [NodeId]
queuedNodes BiasedQueue {..} = PSQ.keys baseQueue

empty :: Double -> BiasedQueue
empty biasProbability =
  BiasedQueue {baseQueue = PSQ.empty, simpleQueue = PSQ.empty, biasProbability}

delete :: NodeId -> BiasedQueue -> BiasedQueue
delete nid BiasedQueue {..} =
  BiasedQueue
    { baseQueue = PSQ.delete nid baseQueue,
      simpleQueue = PSQ.delete nid simpleQueue,
      biasProbability
    }

getPriority :: NodeId -> BiasedQueue -> IO Priority
getPriority nid BiasedQueue {..} = do
  let Just (priority, _) = PSQ.lookup nid baseQueue
  return priority

setPriority :: NodeId -> Priority -> BiasedQueue -> BiasedQueue
setPriority nid priority BiasedQueue {..} =
  BiasedQueue
    { baseQueue = case PSQ.lookup nid baseQueue of
        Nothing -> baseQueue
        Just (_, v) -> PSQ.insert nid priority v baseQueue,
      simpleQueue = case PSQ.lookup nid simpleQueue of
        Nothing -> simpleQueue
        Just (_, v) -> PSQ.insert nid (basePriority priority) v simpleQueue,
      biasProbability
    }

insert :: NodeId -> Priority -> BiasedQueue -> BiasedQueue
insert nid priority BiasedQueue {..} =
  BiasedQueue
    { baseQueue = PSQ.insert nid priority nid baseQueue,
      simpleQueue = PSQ.insert nid (basePriority priority) nid simpleQueue,
      biasProbability
    }

size :: BiasedQueue -> Int
size BiasedQueue {..} = PSQ.size baseQueue

null :: BiasedQueue -> Bool
null BiasedQueue {..} = PSQ.null baseQueue

popSimpleMin :: BiasedQueue -> IO (BasePriority, NodeId, BiasedQueue)
popSimpleMin BiasedQueue {..} = do
  let Just (_, priority, nodeId) = PSQ.findMin simpleQueue
  return (priority, nodeId, delete nodeId BiasedQueue {..})

popBiasedMin :: BiasedQueue -> IO (Priority, NodeId, BiasedQueue)
popBiasedMin BiasedQueue {..} = do
  let Just (_, priority, nodeId) = PSQ.findMin baseQueue
  return (priority, nodeId, delete nodeId BiasedQueue {..})

popMin :: AtomicGenM StdGen -> BiasedQueue -> IO (Priority, NodeId, Bool, BiasedQueue)
popMin randGen queue@BiasedQueue {..} = do
  let Just (_, _, nodeIdBiased) = PSQ.findMin baseQueue
      Just (_, _, nodeIdRandom) = PSQ.findMin simpleQueue
  randVar <- uniformRM (0, 1) randGen
  let pickBiased = randVar < biasProbability
  let nodeId = if pickBiased then nodeIdBiased else nodeIdRandom
  priority <- getPriority nodeId queue
  return (priority, nodeId, pickBiased, delete nodeId queue)

minQueuedDepth :: BiasedQueue -> Maybe Int
minQueuedDepth BiasedQueue {..} = do
  case PSQ.findMin simpleQueue of
    Nothing -> Nothing
    Just (_, priority, _) -> Just (depth priority)

minSimplePriority :: BiasedQueue -> Maybe BasePriority
minSimplePriority BiasedQueue {..} = do
  case PSQ.findMin simpleQueue of
    Nothing -> Nothing
    Just (_, priority, _) -> Just priority

minBiasedPriority :: BiasedQueue -> Maybe Priority
minBiasedPriority BiasedQueue {..} = do
  case PSQ.findMin baseQueue of
    Nothing -> Nothing
    Just (_, priority, _) -> Just priority
