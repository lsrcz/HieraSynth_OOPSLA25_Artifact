{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HieraSynth.Reasoning.Parallel.Scheduler.Sketch
  ( addRootSketch,
    splitNode,
  )
where

import Control.Monad (void, when)
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Grisette
  ( PPrint (pformat),
    nest,
    vsep,
  )
import HieraSynth.Program.Choice.Counting
  ( ComponentChoicesNumResult (ComponentChoicesNumResult),
    countNumChoicesWithEvidence,
    countNumComponentChoicesWithEvidence,
    countNumProgsWithEvidence,
  )
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
  )
import HieraSynth.Program.SymbolTable (SymbolTable)
import HieraSynth.Reasoning.Parallel.Scheduler.BiasedQueue
  ( BasePriority (BasePriority, randomPriority),
    Priority (basePriority),
  )
import qualified HieraSynth.Reasoning.Parallel.Scheduler.BiasedQueue as Q
import HieraSynth.Reasoning.Parallel.Scheduler.Config
  ( SchedulerConfig
      ( SchedulerConfig,
        biasedDrawProbability,
        cmdline,
        costObj,
        countNumProgsEvidence,
        doDeadCodeElimination,
        exactCost,
        fastTrackTimeoutSeconds,
        generalizationSketchFromFastResult,
        initialMinimalCost,
        initialSplitRatio,
        initialTimeoutSeconds,
        logConfig,
        logger,
        parallelism,
        pollIntervalSeconds,
        restartRunningTimeThresholdSeconds,
        schedulerRandomSeed,
        schedulerTimeoutSeconds,
        solverConfig,
        successNodeNewTimeoutSeconds,
        synthesisSketchSymbol,
        targetCost,
        transcriptSMT,
        verifiers
      ),
  )
import HieraSynth.Reasoning.Parallel.Scheduler.DCTree
  ( NodeId,
    insertRootSketches,
    insertSplittedSketches,
    nodeDepth,
    nodeFailed,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.NodeState
  ( NodeState
      ( NodeState
      ),
  )
import HieraSynth.Reasoning.Parallel.Scheduler.NodeStatus
  ( NodeStatus
      ( NodeNotYetStarted
      ),
    StatusType (StatusNotYetStarted),
  )
import HieraSynth.Reasoning.Parallel.Scheduler.Scheduler
  ( NodeInfo
      ( NodeInfo
      ),
    Scheduler
      ( Scheduler,
        config,
        currentMinimalCost,
        dcTree,
        depthToNodes,
        nodeInfo,
        nodeQueue,
        nodeSplitQueue,
        nodeStates,
        nodeStatusSets,
        nodeToProcess,
        processToNode,
        processes,
        queueLock,
        randGen,
        schedulerStartTime,
        stopped
      ),
    getDepth,
    getFirstNotFullySplitDepth,
    getIsSplitted,
    getPriority,
    getSketchTable,
    resetFirstNotFullySplitDepth,
    setIsSplitted,
    updateFirstNotFullySplitDepth,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.Transition
  ( nodeInferFailureTransition,
  )
import HieraSynth.Util.Logging (logMultiLineDoc)
import System.Log.Logger (Priority (DEBUG, NOTICE))
import System.Random.Stateful (UniformRange (uniformRM))

_initializeNodeStatus ::
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
_initializeNodeStatus Scheduler {..} nid = do
  -- Get node depth
  tree <- readIORef dcTree
  let depth = nodeDepth tree nid

  -- Add to status sets
  modifyIORef' nodeStatusSets $ \depthMap ->
    let finalDepthMap = case HM.lookup depth depthMap of
          Nothing ->
            HM.insert depth (HM.singleton StatusNotYetStarted (HS.singleton nid)) depthMap
          Just statusMap ->
            let newStatusMap = HM.insertWith HS.union StatusNotYetStarted (HS.singleton nid) statusMap
             in HM.insert depth newStatusMap depthMap
     in finalDepthMap

  -- Add to depthToNodes map
  modifyIORef' depthToNodes $ \depthMap ->
    HM.insertWith HS.union depth (HS.singleton nid) depthMap

_addSubSketches ::
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
  Maybe NodeId ->
  HM.HashMap (SymbolTable sketchSpec) Double ->
  IO [NodeId]
_addSubSketches
  scheduler@Scheduler {config = SchedulerConfig {..}, ..}
  parentId
  sketchPriorities = do
    oldDcTree <- readIORef dcTree
    let sketches = HM.keys sketchPriorities
    let (nodeIds, newDcTree) = case parentId of
          Nothing -> insertRootSketches oldDcTree sketches
          Just parentId ->
            insertSplittedSketches oldDcTree parentId sketches
    let sketchesToNodeId = HM.fromList $ zip sketches nodeIds

    let nodeIdToSketches =
          HM.fromList $ (\(a, b) -> (b, a)) <$> HM.toList sketchesToNodeId
    let nodeIdPriorities =
          HM.map (sketchPriorities HM.!) nodeIdToSketches

    newDepth <-
      case parentId of
        Just parentId -> return $ nodeDepth oldDcTree parentId + 1
        Nothing -> return 0

    parentBasePriority <-
      case parentId of
        Just parentId -> getPriority scheduler parentId
        Nothing ->
          return $
            Q.Priority (error "Should not be used") False Nothing False
    writeIORef dcTree newDcTree
    let initialState = NodeState NodeNotYetStarted Nothing Nothing [] [] []
    modifyIORef' nodeStates $ \nodeStates' ->
      foldr (`HM.insert` initialState) nodeStates' sketchesToNodeId
    let taskPriority nid =
          parentBasePriority
            { Q.basePriority = BasePriority newDepth (nodeIdPriorities HM.! nid),
              Q.knownWorking = False,
              Q.knownWorkingAncestorDistance =
                (+ 1) <$> Q.knownWorkingAncestorDistance parentBasePriority
            }
    let taskNodeInfo nid =
          NodeInfo
            False
            (nodeIdToSketches HM.! nid)
            (taskPriority nid)
            initialTimeoutSeconds

    modifyIORef' nodeInfo $ \nodeInfo' ->
      foldr (\nid acc -> HM.insert nid (taskNodeInfo nid) acc) nodeInfo' $
        HM.keysSet nodeIdToSketches
    modifyIORef' nodeQueue $ \q ->
      foldr (\nid -> Q.insert nid (taskPriority nid)) q $
        HM.keysSet nodeIdToSketches

    modifyIORef' nodeSplitQueue $ \q ->
      foldr (\nid -> Q.insert nid (Q.recipPriority $ taskPriority nid)) q $
        HM.keysSet nodeIdToSketches

    let nodeIdToTaskPriority =
          HM.fromList $
            (\nid -> (nid, taskPriority nid))
              <$> HM.keys nodeIdToSketches

    -- Initialize status tracking for each new node
    mapM_ (_initializeNodeStatus scheduler) (HM.keys nodeIdToSketches)

    finalNodeInfo <- readIORef nodeInfo
    inferredFailures <-
      concat
        <$> traverse
          ( \nid ->
              if nodeFailed newDcTree nid
                then do
                  logMultiLineDoc logger NOTICE $
                    "Newly added node "
                      <> pformat nid
                      <> " is inferred to fail, not inserting to the queue."
                  modifyIORef' nodeQueue $ Q.delete nid
                  nodeInferFailureTransition scheduler nid
                  return [nid]
                else return []
          )
          nodeIds

    numsInfo <- case countNumProgsEvidence of
      Just countNumProgsEvidence -> do
        let nodeIdToNumChoices =
              HM.fromList $
                ( \(sketch, nid) ->
                    (nid, countNumChoicesWithEvidence countNumProgsEvidence sketch)
                )
                  <$> HM.toList sketchesToNodeId
        let nodeIdToNumWellTyped =
              HM.fromList $
                ( \(sketch, nid) ->
                    (nid, countNumProgsWithEvidence countNumProgsEvidence sketch)
                )
                  <$> HM.toList sketchesToNodeId
        let nodeIdToAvgCompChoices =
              HM.fromList $
                ( \(sketch, nid) ->
                    ( nid,
                      case countNumComponentChoicesWithEvidence countNumProgsEvidence sketch of
                        ComponentChoicesNumResult numComponents numTotalChoices ->
                          fromIntegral numTotalChoices / fromIntegral numComponents :: Double
                    )
                )
                  <$> HM.toList sketchesToNodeId
        return
          [ "Num of choices in sketches: ",
            pformat nodeIdToNumChoices,
            "Num of well typed programs in sketches: ",
            pformat nodeIdToNumWellTyped,
            "Avg component choices in sketches: ",
            pformat nodeIdToAvgCompChoices
          ]
      Nothing -> return []

    logMultiLineDoc logger NOTICE $
      nest 2 $
        vsep $
          concat
            [ [ "Added sub sketches to node " <> pformat parentId <> ":",
                "Priorities and new node IDs: ",
                pformat nodeIdToTaskPriority
              ],
              numsInfo,
              [ "Num of inferred failures: "
                  <> pformat (length inferredFailures),
                "Num of nodes: " <> pformat (HM.size finalNodeInfo)
              ]
            ]
    return $ toList sketchesToNodeId

addRootSketch ::
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
  SymbolTable sketchSpec ->
  IO () -- NodeId
addRootSketch
  scheduler@Scheduler {config = SchedulerConfig {..}}
  sketch = do
    logMultiLineDoc logger NOTICE $
      nest 2 $
        vsep ["Adding root sketch: ", pformat sketch]

    -- Reset the first not fully split depth counter to 0 when adding a root node
    resetFirstNotFullySplitDepth scheduler

    void $ _addSubSketches scheduler Nothing (HM.fromList [(sketch, 1)])

splitNode ::
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
  Bool ->
  NodeId ->
  IO [NodeId]
splitNode
  scheduler@Scheduler {config = SchedulerConfig {..}}
  success
  nodeId = do
    nodeSplitted <- getIsSplitted scheduler nodeId
    if nodeSplitted
      then do
        logMultiLineDoc logger NOTICE $
          nest 2 $
            vsep
              [ "Node " <> pformat nodeId <> " is already splitted.",
                "Skipping."
              ]
        return []
      else do
        nodeDepth <- getDepth scheduler nodeId
        currentNotFullySplitDepth <- getFirstNotFullySplitDepth scheduler

        parentBasePriority <- getPriority scheduler nodeId
        sketch <- getSketchTable scheduler nodeId
        let seqNum = lowestSeqNum success sketch
        let splittedSketches =
              case seqNum of
                Nothing -> []
                Just seqNum ->
                  filter (/= sketch) $ partitionSpec seqNum sketch
        logMultiLineDoc logger NOTICE $
          nest 2 $
            vsep
              [ "Splitting node " <> pformat nodeId <> ":",
                "Splitted seq num: " <> pformat seqNum,
                "Splitted under successful node: " <> pformat success
              ]
        logMultiLineDoc logger DEBUG $
          vsep
            [ nest 2 $ vsep ["Original sketch: ", pformat sketch],
              nest 2 $ vsep ["Splitted sketches: ", pformat splittedSketches]
            ]
        splittedSketchesWithPriority <-
          HM.fromList
            <$> traverse
              ( \s -> do
                  randomMultiplier <- uniformRM (0.9, 1.1) (randGen scheduler)
                  return
                    ( s,
                      randomMultiplier
                        / randomPriority (basePriority parentBasePriority)
                    )
              )
              splittedSketches
        r <-
          if null splittedSketches
            then do
              logMultiLineDoc logger NOTICE $
                nest 2 $
                  vsep
                    [ "Skipping node "
                        <> pformat nodeId
                        <> " because it has no sub-sketches."
                    ]
              setIsSplitted scheduler nodeId True
              modifyIORef' (nodeSplitQueue scheduler) $ Q.delete nodeId
              return []
            else do
              splittedNodeIds <-
                _addSubSketches
                  scheduler
                  (Just nodeId)
                  splittedSketchesWithPriority
              setIsSplitted scheduler nodeId True
              modifyIORef' (nodeSplitQueue scheduler) $ Q.delete nodeId
              return splittedNodeIds
        -- Check if we need to update the first not fully split depth
        when (nodeDepth == currentNotFullySplitDepth) $
          updateFirstNotFullySplitDepth scheduler

        return r
