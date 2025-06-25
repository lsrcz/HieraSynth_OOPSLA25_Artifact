{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HieraSynth.Reasoning.Parallel.Scheduler.TreeStats
  ( logTreeStats,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (readIORef)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Time
  ( getCurrentTime,
  )
import Grisette
  ( Doc,
    PPrint (pformat),
    nest,
    vsep,
    (<+>),
  )
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
    nodeDividedChildren,
    rootNodes,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.NodeState
  ( pformatNodeStateSummaryWithElapsedTime,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.NodeStatus
  ( statusType,
    statusTypeIsDetermined,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.Scheduler
  ( Scheduler
      ( Scheduler,
        config,
        currentMinimalCost,
        dcTree,
        nodeInfo,
        nodeQueue,
        nodeStates,
        nodeToProcess,
        processToNode,
        processes,
        queueLock,
        randGen,
        schedulerStartTime,
        stopped
      ),
    getIsSplitted,
    getStatus,
  )
import HieraSynth.Util.Logging (logMultiLineDoc)
import System.Log.Logger (Priority)

logTreeStats ::
  Priority ->
  Bool ->
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
logTreeStats
  level
  onlyUndetermined
  scheduler@Scheduler {config = SchedulerConfig {..}, ..} = do
    logMultiLineDoc logger level $
      if onlyUndetermined
        then "Dumping stats for undetermined nodes."
        else "Dumping stats for all nodes."
    curTime <- getCurrentTime
    dcTree <- readIORef dcTree
    let allRootNodes = sort $ HS.toList $ rootNodes dcTree
    let walkNode :: NodeId -> IO (Doc ann)
        walkNode nid = do
          result <- nodeResult nid
          status <- getStatus scheduler nid
          splitted <- getIsSplitted scheduler nid
          if not splitted
            then return result
            else do
              let children =
                    sort $
                      HS.toList $
                        fromMaybe mempty $
                          nodeDividedChildren dcTree nid
              if
                | null children -> return $ result <+> "{}"
                | onlyUndetermined && statusTypeIsDetermined (statusType status) ->
                    return $ result <+> "{...}"
                | otherwise -> do
                    childrenDocs <- mapM walkNode children
                    return $
                      vsep
                        [ nest 2 $ vsep [result <+> "{", vsep childrenDocs],
                          "}"
                        ]
        nodeResult :: NodeId -> IO (Doc ann)
        nodeResult nid = do
          state <- (HM.! nid) <$> readIORef nodeStates
          return $
            pformat nid
              <> ": "
              <> pformatNodeStateSummaryWithElapsedTime curTime state
    doc <- vsep <$> traverse walkNode allRootNodes
    logMultiLineDoc logger level doc
