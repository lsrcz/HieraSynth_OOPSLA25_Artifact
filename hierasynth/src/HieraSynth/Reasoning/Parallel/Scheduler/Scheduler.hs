{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HieraSynth.Reasoning.Parallel.Scheduler.Scheduler
  ( Scheduler (..),
    NodeInfo (..),
    newScheduler,
    getCPid,
    getProcessByCPid,
    getProcess,
    getCurrentMinimalCost,
    getCurrentTimeout,
    getCurrentElapsedTime,
    getStatus,
    getNodeInfo,
    getDepth,
    getIsSplitted,
    getSketchTable,
    getPriority,
    getNumRunningProcess,
    getNumQueuedProcess,
    setIsSplitted,
    setPriority,
    setTimeout,
    removeProcessByCPid,
    removeProcess,
    updateCurrentMinimalCost,
    getNodesByStatus,
    getNodesByStatusAndDepth,
    getNodesByDepth,
    updateNodeState,
    getSplitNodesByDepth,
    isSplitAtDepth,
    updateFirstNotFullySplitDepth,
    getFirstNotFullySplitDepth,
    resetFirstNotFullySplitDepth,
    isDepthFullySplit,
    getEverStartedNodes,
    getEverStartedNodesByDepth,
  )
where

import Control.Concurrent (MVar, newMVar)
import Control.Exception (throwIO)
import Control.Monad (when)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Time
  ( NominalDiffTime,
    UTCTime,
    getCurrentTime,
  )
import Foreign.C (eBADF)
import GHC.Stack (HasCallStack)
import HieraSynth.Program.SymbolTable (SymbolTable)
import qualified HieraSynth.Reasoning.Parallel.Scheduler.BiasedQueue as Q
import HieraSynth.Reasoning.Parallel.Scheduler.Config
  ( SchedulerConfig,
    biasedDrawProbability,
    initialMinimalCost,
    schedulerRandomSeed,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.DCTree
  ( DCTree,
    NodeId,
    emptyDCTree,
    nodeDepth,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.NodeState
  ( NodeState (nodeStatus),
    nodeStateCurrentElapsedTime,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.NodeStatus
  ( NodeStatus,
    StatusType,
    statusType,
    statusTypeDoNotNeedChild,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.Process
  ( Process (pipeRd, pipeWr),
  )
import HieraSynth.Util.Exception (catchErrno)
import System.Posix
  ( closeFd,
  )
import System.Random.Stateful
  ( AtomicGenM,
    StdGen,
    mkStdGen,
    newAtomicGenM,
  )

data NodeInfo sketchSpec = NodeInfo
  { nodeSplitted :: Bool,
    nodeSketchTable :: SymbolTable sketchSpec,
    nodePriority :: Q.Priority,
    nodeTimeoutSeconds :: Int
  }

data
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher
  where
  Scheduler ::
    { config ::
        SchedulerConfig
          sketchSpec
          sketch
          conProg
          costObj
          cost
          symSemObj
          symVal
          conSemObj
          conVal
          matcher,
      randGen :: AtomicGenM StdGen,
      nodeInfo :: IORef (HM.HashMap NodeId (NodeInfo sketchSpec)),
      nodeStates ::
        IORef
          ( HM.HashMap
              NodeId
              (NodeState conProg symSemObj symVal conSemObj conVal matcher)
          ),
      nodeQueue :: IORef Q.BiasedQueue,
      nodeSplitQueue :: IORef Q.BiasedQueue,
      processes :: IORef (HM.HashMap Int32 Process),
      processToNode :: IORef (HM.HashMap Int32 NodeId),
      nodeToProcess :: IORef (HM.HashMap NodeId Int32),
      dcTree :: IORef (DCTree (SymbolTable sketchSpec)),
      currentMinimalCost :: IORef (Maybe Int),
      queueLock :: MVar (),
      stopped :: IORef Bool,
      schedulerStartTime :: UTCTime,
      -- Track nodes by depth and status type
      nodeStatusSets :: IORef (HM.HashMap Int (HM.HashMap StatusType (HS.HashSet NodeId))),
      -- Track all nodes at each depth
      depthToNodes :: IORef (HM.HashMap Int (HS.HashSet NodeId)),
      -- Track nodes that have been split at each depth
      splitNodesByDepth :: IORef (HM.HashMap Int (HS.HashSet NodeId)),
      -- Track the first depth that is not fully split
      firstNotFullySplitDepth :: IORef Int,
      -- Track all nodes that have ever been started by depth
      everStartedNodesByDepth :: IORef (HM.HashMap Int (HS.HashSet NodeId))
    } ->
    Scheduler
      sketchSpec
      sketch
      conProg
      costObj
      cost
      symSemObj
      symVal
      conSemObj
      conVal
      matcher

newScheduler ::
  SchedulerConfig
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO
    ( Scheduler
        sketchSpec
        sketch
        conProg
        costObj
        cost
        symSemObj
        symVal
        conSemObj
        conVal
        matcher
    )
newScheduler config = do
  randGen <- newAtomicGenM (mkStdGen (schedulerRandomSeed config))
  nodeInfo <- newIORef HM.empty
  nodeStates <- newIORef HM.empty
  nodeQueue <- newIORef $ Q.empty (biasedDrawProbability config)
  nodeSplitQueue <- newIORef $ Q.empty (biasedDrawProbability config)
  processes <- newIORef HM.empty
  processToNode <- newIORef HM.empty
  nodeToProcess <- newIORef HM.empty
  dcTree <- newIORef emptyDCTree
  currentMinimalCost <- newIORef $ initialMinimalCost config
  queueLock <- newMVar ()
  stopped <- newIORef False
  schedulerStartTime <- getCurrentTime
  nodeStatusSets <- newIORef HM.empty
  depthToNodes <- newIORef HM.empty
  splitNodesByDepth <- newIORef HM.empty
  firstNotFullySplitDepth <- newIORef 0 -- Initialize to 0
  everStartedNodesByDepth <- newIORef HM.empty -- Initialize to empty map
  return $ Scheduler {..}

getCPid ::
  (HasCallStack) =>
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Int32
getCPid Scheduler {..} nid =
  readIORef nodeToProcess >>= \m -> return $ m HM.! nid

getProcessByCPid ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Int32 ->
  IO Process
getProcessByCPid Scheduler {..} cpid =
  readIORef processes >>= \m -> return $ m HM.! cpid

getProcess ::
  (HasCallStack) =>
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Process
getProcess scheduler nid = do
  cpid <- getCPid scheduler nid
  getProcessByCPid scheduler cpid

getCurrentMinimalCost ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO (Maybe Int)
getCurrentMinimalCost Scheduler {..} = readIORef currentMinimalCost

getCurrentTimeout ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Int
getCurrentTimeout Scheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  case HM.lookup nid nodeInfo of
    Just NodeInfo {nodeTimeoutSeconds} -> return nodeTimeoutSeconds
    Nothing -> error "Should not happen: node info not found"

getCurrentElapsedTime ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO NominalDiffTime
getCurrentElapsedTime Scheduler {..} nid = do
  curTime <- getCurrentTime
  nodeStates <- readIORef nodeStates
  return $ nodeStateCurrentElapsedTime curTime $ nodeStates HM.! nid

getStatus ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO (NodeStatus conProg)
getStatus Scheduler {..} nid = do
  nodeStates <- readIORef nodeStates
  return $ nodeStatus $ nodeStates HM.! nid

getNodeInfo ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO (NodeInfo sketchSpec)
getNodeInfo Scheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  return $ nodeInfo HM.! nid

getDepth ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Int
getDepth Scheduler {..} nid = do
  dcTree <- readIORef dcTree
  return $ nodeDepth dcTree nid

getIsSplitted ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Bool
getIsSplitted Scheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  return $ nodeSplitted $ nodeInfo HM.! nid

getSketchTable ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO (SymbolTable sketchSpec)
getSketchTable Scheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  return $ nodeSketchTable $ nodeInfo HM.! nid

getPriority ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Q.Priority
getPriority Scheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  return $ nodePriority $ nodeInfo HM.! nid

getNumRunningProcess ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO Int
getNumRunningProcess Scheduler {..} = length <$> readIORef processes

getNumQueuedProcess ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO Int
getNumQueuedProcess Scheduler {..} = Q.size <$> readIORef nodeQueue

setIsSplitted ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  Bool ->
  IO ()
setIsSplitted scheduler@Scheduler {..} nid isSplitted = do
  modifyIORef' nodeInfo $ HM.adjust (\ni -> ni {nodeSplitted = isSplitted}) nid

  -- If we're marking a node as split, also update the splitNodesByDepth field
  when isSplitted $ do
    depth <- getDepth scheduler nid
    modifyIORef' splitNodesByDepth $ \splitNodesMap ->
      let existingSplitNodes = HM.lookupDefault HS.empty depth splitNodesMap
          updatedSplitNodes = HS.insert nid existingSplitNodes
       in HM.insert depth updatedSplitNodes splitNodesMap

    -- Check if we need to update the first not fully split depth
    currentNotFullySplitDepth <- readIORef firstNotFullySplitDepth
    when (depth == currentNotFullySplitDepth) $
      updateFirstNotFullySplitDepth scheduler

setPriority ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  Q.Priority ->
  IO ()
setPriority Scheduler {..} nid priority = do
  modifyIORef' nodeInfo $ HM.adjust (\ni -> ni {nodePriority = priority}) nid
  modifyIORef' nodeQueue $ Q.setPriority nid priority

  -- Update the split queue with the reciprocal priority
  let reciprocalPriority =
        priority
          { Q.basePriority = Q.recipBasePriority (Q.basePriority priority),
            -- Keep the same status flags but invert the base priority
            Q.knownWorking = Q.knownWorking priority,
            Q.knownWorkingAncestorDistance = Q.knownWorkingAncestorDistance priority,
            Q.ancestorSiblingKnownWorking = Q.ancestorSiblingKnownWorking priority
          }
  modifyIORef' nodeSplitQueue $ Q.setPriority nid reciprocalPriority

setTimeout ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  Int ->
  IO ()
setTimeout Scheduler {..} nid timeout = do
  modifyIORef' nodeInfo $
    HM.adjust
      (\ni -> ni {nodeTimeoutSeconds = timeout})
      nid

removeProcessByCPid ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Int32 ->
  IO ()
removeProcessByCPid Scheduler {..} cpid = do
  process <- readIORef processes >>= \m -> return $ m HM.! cpid
  let ec e v = if v == eBADF then return () else throwIO e
  catchErrno (closeFd $ pipeRd process) ec
  catchErrno (closeFd $ pipeWr process) ec
  modifyIORef' processes $ HM.delete cpid
  nid <- readIORef processToNode >>= \m -> return $ m HM.! cpid
  modifyIORef' processToNode $ HM.delete cpid
  modifyIORef' nodeToProcess $ HM.delete nid

removeProcess ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO ()
removeProcess scheduler nid = do
  cpid <- getCPid scheduler nid
  removeProcessByCPid scheduler cpid

updateCurrentMinimalCost ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Maybe Int ->
  IO ()
updateCurrentMinimalCost Scheduler {..} newCost = do
  modifyIORef' currentMinimalCost $ \case
    Nothing -> newCost
    Just oldCost -> case newCost of
      Nothing -> return oldCost
      Just newCost -> Just $ min oldCost newCost

-- Get node IDs by status type
getNodesByStatus ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  StatusType ->
  IO (HS.HashSet NodeId)
getNodesByStatus Scheduler {..} statusType = do
  statusSets <- readIORef nodeStatusSets
  return $
    HS.unions $
      map
        (HM.lookupDefault HS.empty statusType)
        (HM.elems statusSets)

-- Get node IDs by status type and depth
getNodesByStatusAndDepth ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  StatusType ->
  Int ->
  IO (HS.HashSet NodeId)
getNodesByStatusAndDepth Scheduler {..} statusType depth = do
  statusSets <- readIORef nodeStatusSets
  case HM.lookup depth statusSets of
    Nothing -> return HS.empty
    Just depthMap -> return $ HM.lookupDefault HS.empty statusType depthMap

-- Get all node IDs at a specific depth
getNodesByDepth ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Int ->
  IO (HS.HashSet NodeId)
getNodesByDepth Scheduler {..} depth = do
  depthMap <- readIORef depthToNodes
  return $ HM.lookupDefault HS.empty depth depthMap

-- Update node status and update the status sets
updateNodeState ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  NodeState conProg symSemObj symVal conSemObj conVal matcher ->
  IO ()
updateNodeState Scheduler {..} nid newState = do
  -- Update the node state
  oldStateMap <- readIORef nodeStates
  let oldState = oldStateMap HM.! nid
  let oldStatus = nodeStatus oldState

  -- Update nodeStates with new status
  modifyIORef' nodeStates $ HM.insert nid newState

  -- Get depth
  tree <- readIORef dcTree
  let depth = nodeDepth tree nid

  -- Only update sets if the status type changed
  let oldStatusType = statusType oldStatus
  let newStatusType = statusType (nodeStatus newState)

  when (oldStatusType /= newStatusType) $ do
    modifyIORef' nodeStatusSets $ \depthMap ->
      let -- Remove from old status set
          updatedDepthMap = case HM.lookup depth depthMap of
            Nothing -> depthMap
            Just statusMap ->
              let updatedStatusMap = HM.adjust (HS.delete nid) oldStatusType statusMap
               in HM.insert depth updatedStatusMap depthMap

          -- Add to new status set
          finalDepthMap = case HM.lookup depth updatedDepthMap of
            Nothing ->
              HM.insert depth (HM.singleton newStatusType (HS.singleton nid)) updatedDepthMap
            Just statusMap ->
              let newStatusMap = HM.insertWith HS.union newStatusType (HS.singleton nid) statusMap
               in HM.insert depth newStatusMap updatedDepthMap
       in finalDepthMap

    -- If the node transitioned to a status that doesn't need children,
    -- check if we need to update the first not fully split depth
    when (statusTypeDoNotNeedChild newStatusType) $ do
      currentNotFullySplitDepth <- readIORef firstNotFullySplitDepth
      -- Only need to update if this node is at the current firstNotFullySplitDepth
      when (depth == currentNotFullySplitDepth) $
        updateFirstNotFullySplitDepth (Scheduler {..})

-- | Get all nodes that have been split at a specific depth
getSplitNodesByDepth ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Int ->
  IO (HS.HashSet NodeId)
getSplitNodesByDepth Scheduler {..} depth = do
  splitNodesMap <- readIORef splitNodesByDepth
  return $ HM.lookupDefault HS.empty depth splitNodesMap

-- | Check if a node has been split at a specific depth
isSplitAtDepth ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  Int ->
  IO Bool
isSplitAtDepth scheduler nid depth = do
  splitNodes <- getSplitNodesByDepth scheduler depth
  return $ nid `HS.member` splitNodes

-- | Get the first depth that is not fully split
getFirstNotFullySplitDepth ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO Int
getFirstNotFullySplitDepth Scheduler {..} = readIORef firstNotFullySplitDepth

-- | Reset the first not fully split depth to 0 (usually when adding a new root node)
resetFirstNotFullySplitDepth ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO ()
resetFirstNotFullySplitDepth Scheduler {..} = writeIORef firstNotFullySplitDepth 0

-- | Check if a specific depth is fully split
isDepthFullySplit ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Int ->
  IO Bool
isDepthFullySplit scheduler@Scheduler {..} depth = do
  -- Get all nodes at this depth
  allNodesAtDepth <- getNodesByDepth scheduler depth

  -- If the set is empty, consider it fully split (no nodes to split)
  if HS.null allNodesAtDepth
    then return True
    else do
      -- Get all split nodes at this depth
      splitNodesAtDepth <- getSplitNodesByDepth scheduler depth

      -- Get nodes at this depth by their status types
      nodesByStatusType <- do
        statusSets <- readIORef nodeStatusSets
        case HM.lookup depth statusSets of
          Nothing -> return HM.empty
          Just statusMap -> return statusMap

      -- Collect all nodes that don't need children
      let doNotNeedChildNodes =
            HS.unions
              [ nodes
              | (statusType, nodes) <- HM.toList nodesByStatusType,
                statusTypeDoNotNeedChild statusType
              ]

      -- Union of nodes that are split and nodes that don't need children
      let effectivelySplitNodes = HS.union splitNodesAtDepth doNotNeedChildNodes

      -- The depth is fully split if all nodes are either split or don't need children
      return $ allNodesAtDepth == effectivelySplitNodes

-- | Update the first not fully split depth by checking if the current depth
-- is fully split, and if so, advancing to the next depth
updateFirstNotFullySplitDepth ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO ()
updateFirstNotFullySplitDepth scheduler@Scheduler {..} = do
  currentDepth <- readIORef firstNotFullySplitDepth
  allNodesAtDepth <- getNodesByDepth scheduler currentDepth
  if null allNodesAtDepth
    then return ()
    else do
      isFullySplit <- isDepthFullySplit scheduler currentDepth

      when isFullySplit $ do
        -- Advance to the next depth
        modifyIORef' firstNotFullySplitDepth (+ 1)

        -- Recursively check the next depth too
        updateFirstNotFullySplitDepth scheduler

-- | Get all nodes that have ever been started across all depths
getEverStartedNodes ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO (HS.HashSet NodeId)
getEverStartedNodes Scheduler {..} = do
  everStartedMap <- readIORef everStartedNodesByDepth
  return $ HS.unions $ HM.elems everStartedMap

-- | Get all nodes that have ever been started at a specific depth
getEverStartedNodesByDepth ::
  Scheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Int ->
  IO (HS.HashSet NodeId)
getEverStartedNodesByDepth Scheduler {..} depth = do
  everStartedMap <- readIORef everStartedNodesByDepth
  return $ HM.lookupDefault HS.empty depth everStartedMap
