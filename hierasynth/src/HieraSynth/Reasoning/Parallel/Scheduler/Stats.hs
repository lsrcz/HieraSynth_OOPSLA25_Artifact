{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module HieraSynth.Reasoning.Parallel.Scheduler.Stats
  ( reportStatistics,
    MessageStat (..),
    collectStats,
    viableMessageStats,
    generalizationFailureMessageStats,
    succeedMessageStats,
    generalizationSucceedMessageStats,
  )
where

import Control.Monad (forM_, guard, unless, when)
import Data.Bifunctor (second)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.IORef (readIORef)
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.String (IsString (fromString))
import Data.Time
  ( UTCTime,
    getCurrentTime,
  )
import Graphics.Rendering.Chart.Backend.Cairo
  ( FileFormat (SVG),
    FileOptions (FileOptions),
    toFile,
  )
import Graphics.Rendering.Chart.Easy
  ( Colour,
    PointShape
      ( PointShapeCircle,
        PointShapeCross,
        PointShapePolygon,
        PointShapeStar
      ),
    aqua,
    black,
    deeppink,
    dodgerblue,
    font_size,
    gray,
    green,
    layout_title,
    liftEC,
    line,
    mediumpurple,
    opaque,
    orange,
    plot,
    plot_annotation_style,
    plot_annotation_values,
    points,
    red,
    setColors,
    setShapes,
    yellowgreen,
    (.=),
  )
import Grisette
  ( Doc,
    PPrint (pformat),
    derive,
    nest,
    vsep,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.Config
  ( SchedulerConfig
      ( SchedulerConfig,
        biasedDrawProbability,
        cmdline,
        costObj,
        countNumProgsEvidence,
        doDeadCodeElimination,
        enablePlotting,
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
    nodeDepth,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.LogConfig (logRootDir)
import HieraSynth.Reasoning.Parallel.Scheduler.NodeState
  ( NodeState,
    nodeStateCurrentElapsedTime,
    nodeStateMajorRelativeTimeLog,
    nodeStateNumCollectedExamples,
    nodeStateNumInProgressExamples,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.NodeStatus
  ( StatusType
      ( StatusInferredFailure,
        StatusJustStarted,
        StatusNotYetStarted,
        StatusRefining,
        StatusSucceeded,
        StatusTerminated,
        StatusUnknown,
        StatusUnsat,
        StatusViable
      ),
  )
import HieraSynth.Reasoning.Parallel.Scheduler.Process
  ( MessageType (MessageGeneralizationFailure, MessageSuccess, MessageViable),
    responseType,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.Scheduler
  ( Scheduler
      ( Scheduler,
        config,
        currentMinimalCost,
        dcTree,
        firstNotFullySplitDepth,
        nodeInfo,
        nodeQueue,
        nodeStates,
        nodeToProcess,
        processToNode,
        processes,
        queueLock,
        randGen,
        schedulerStartTime,
        splitNodesByDepth,
        stopped
      ),
    getEverStartedNodes,
    getEverStartedNodesByDepth,
    getFirstNotFullySplitDepth,
    getNodesByDepth,
    getNodesByStatus,
    getNodesByStatusAndDepth,
    getSplitNodesByDepth,
  )
import HieraSynth.Util.Logging (logMultiLineDoc)
import HieraSynth.Util.Show (showFloat)
import System.Log.Logger (Priority (NOTICE))

-- | Sum type for categorizing stats
data StatCategory
  = NodeCategory StatusType
  | MessageCategory MessageType

derive [''StatCategory] [''Eq, ''Ord, ''Hashable]

instance Show StatCategory where
  show (NodeCategory s) = show s
  show (MessageCategory m) = show m

-- | Statistical data for a single node in the scheduler
data NodeStats = NodeStats
  { nodeId :: NodeId,
    sortedIdx :: Int,
    nodeTime :: Double,
    collectedExamples :: Int,
    inProgressExamples :: Int
  }

-- | Statistics for a message
data MessageStat = MessageStat
  { nodeIdOrigin :: NodeId,
    sortedIdx :: Int,
    msgTime :: Double
  }

-- | Summary statistics for a group of nodes
data SummaryStats = SummaryStats
  { num :: Int,
    percentAll :: Double,
    percentEverStarted :: Double,
    nodeStats :: [NodeStats],
    avgTime :: Double,
    time75Percentile :: Double,
    time90Percentile :: Double,
    time95Percentile :: Double,
    avgCollectedExamples :: Double,
    avgInProgressExamples :: Double
  }

-- | Statistics categorized by node status
data Stats = Stats
  { allStartedNodeStats :: SummaryStats,
    numNotStarted :: Int,
    numSplitNodes :: Int,
    numDoNotNeedChildNodes :: Int,
    numNeedSplitNodes :: Int,
    numEverStartedNodes :: Int,
    viableStats :: SummaryStats,
    refiningStats :: SummaryStats,
    succeedStats :: SummaryStats,
    unsatStats :: SummaryStats,
    unknownStats :: SummaryStats,
    terminatedStats :: SummaryStats,
    inferredFailureStats :: SummaryStats,
    justStartedStats :: SummaryStats,
    viableMessageStats :: [MessageStat],
    generalizationFailureMessageStats :: [MessageStat],
    succeedMessageStats :: [MessageStat],
    generalizationSucceedMessageStats :: [MessageStat]
  }

-- | Categories that should be considered for annotations
annotationCategories :: [StatCategory]
annotationCategories =
  [ NodeCategory StatusViable,
    NodeCategory StatusRefining,
    NodeCategory StatusSucceeded,
    MessageCategory MessageViable,
    MessageCategory MessageGeneralizationFailure,
    MessageCategory (MessageSuccess False),
    MessageCategory (MessageSuccess True)
  ]

-- | Color mapping for different node and message types
colorMap :: HM.HashMap StatCategory (Colour Double)
colorMap =
  HM.fromList
    [ (NodeCategory StatusViable, aqua),
      (NodeCategory StatusRefining, dodgerblue),
      (NodeCategory StatusSucceeded, green),
      (NodeCategory StatusUnsat, mediumpurple),
      (NodeCategory StatusUnknown, yellowgreen),
      (NodeCategory StatusTerminated, red),
      (NodeCategory StatusInferredFailure, orange),
      (NodeCategory StatusJustStarted, black),
      (MessageCategory MessageViable, gray),
      (MessageCategory MessageGeneralizationFailure, deeppink),
      (MessageCategory (MessageSuccess False), green),
      (MessageCategory (MessageSuccess True), yellowgreen)
    ]

-- | Shape mapping for different node and message types
shapeMap :: HM.HashMap StatCategory PointShape
shapeMap =
  HM.fromList
    [ (NodeCategory StatusViable, PointShapeCircle),
      (NodeCategory StatusRefining, PointShapeCircle),
      (NodeCategory StatusSucceeded, PointShapeStar),
      (NodeCategory StatusUnsat, PointShapeCross),
      (NodeCategory StatusUnknown, PointShapeCross),
      (NodeCategory StatusTerminated, PointShapeCross),
      (NodeCategory StatusInferredFailure, PointShapeCross),
      (NodeCategory StatusJustStarted, PointShapeCircle),
      (MessageCategory MessageViable, PointShapePolygon 4 True),
      (MessageCategory MessageGeneralizationFailure, PointShapePolygon 4 True),
      (MessageCategory (MessageSuccess False), PointShapePolygon 4 True),
      (MessageCategory (MessageSuccess True), PointShapePolygon 4 True)
    ]

-- | Validates the node status assignments and logs any inconsistencies
validateNodeStatus ::
  HS.HashSet NodeId -> -- All nodes
  [(StatusType, HS.HashSet NodeId)] -> -- Status type to node set mappings
  (Doc ann -> IO ()) -> -- Logger function
  IO ()
validateNodeStatus allNodes statusTypeMappings logFunction = do
  let allTrackedNodes = foldl HS.union HS.empty (map snd statusTypeMappings)
      missingNodes = allNodes `HS.difference` allTrackedNodes

      findDuplicates node =
        filter
          (\(_, nodes) -> node `HS.member` nodes)
          statusTypeMappings

      multiStatusNodes = HM.fromList $ do
        node <- HS.toList allTrackedNodes
        let statuses = findDuplicates node
        [(node, map fst statuses) | length statuses > 1]

  -- Log any issues
  unless (HS.null missingNodes) $
    logFunction $
      "STATS ERROR: " <> pformat (HS.size missingNodes) <> " nodes without any status: " <> pformat (HS.toList missingNodes)

  unless (HM.null multiStatusNodes) $
    logFunction $
      "STATS ERROR: Nodes with multiple statuses: " <> pformat multiStatusNodes

-- | Calculates a percentile value from a list of values
percentile :: (Ord a) => Double -> [a] -> a
percentile _ [] = error "Cannot calculate percentile of an empty list"
percentile cutoff l =
  let sorted = sort l
      len = length sorted
      idx = min (len - 1) $ ceiling $ fromIntegral len * cutoff
   in sorted !! idx

-- | Converts node states to NodeStats objects
nodeStateToNodeStats ::
  UTCTime ->
  (NodeId, NodeState conProg symSemObj symVal conSemObj conVal matcher) ->
  HM.HashMap NodeId Int -> -- NodeId to sortedIdx mapping
  NodeStats
nodeStateToNodeStats curTime (nid, state) nodeIdToSortedIdx =
  NodeStats
    nid
    (nodeIdToSortedIdx HM.! nid)
    (realToFrac $ nodeStateCurrentElapsedTime curTime state)
    (nodeStateNumCollectedExamples state)
    (nodeStateNumInProgressExamples state)

-- | Creates SummaryStats from a list of NodeStats
createSummaryStats :: Int -> Int -> [NodeStats] -> SummaryStats
createSummaryStats allNodeNum everStartedNum nodeStats =
  let nodeCount = length nodeStats
      times = map nodeTime nodeStats
      collectedExamplesSum = sum (map collectedExamples nodeStats)
      inProgressExamplesSum = sum (map inProgressExamples nodeStats)

      avgTime = if null times then -1 else sum times / fromIntegral nodeCount
      p75 = if null times then -1 else percentile 0.75 times
      p90 = if null times then -1 else percentile 0.90 times
      p95 = if null times then -1 else percentile 0.95 times

      avgCollected =
        if nodeCount == 0
          then 0
          else
            fromIntegral collectedExamplesSum / fromIntegral nodeCount

      avgInProgress =
        if nodeCount == 0
          then 0
          else
            fromIntegral inProgressExamplesSum / fromIntegral nodeCount
   in SummaryStats
        nodeCount
        (realToFrac $ 100 * nodeCount % allNodeNum)
        (realToFrac $ 100 * nodeCount % everStartedNum)
        nodeStats
        avgTime
        p75
        p90
        p95
        avgCollected
        avgInProgress

-- | Collects statistics about nodes in the scheduler
collectStats ::
  UTCTime ->
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
  Maybe Int -> -- Optional depth filter
  IO Stats
collectStats curTime scheduler@Scheduler {config = SchedulerConfig {..}, ..} maybeDepth = do
  -- Get node sets by status type
  let getNodeSet statusType = case maybeDepth of
        Nothing -> getNodesByStatus scheduler statusType
        Just depth -> getNodesByStatusAndDepth scheduler statusType depth

  viableNodes <- getNodeSet StatusViable
  refiningNodes <- getNodeSet StatusRefining
  succeedNodes <- getNodeSet StatusSucceeded
  unsatNodes <- getNodeSet StatusUnsat
  unknownNodes <- getNodeSet StatusUnknown
  terminatedNodes <- getNodeSet StatusTerminated
  inferredFailureNodes <- getNodeSet StatusInferredFailure
  justStartedNodes <- getNodeSet StatusJustStarted
  notYetStartedNodes <- getNodeSet StatusNotYetStarted

  -- Get all nodes that have ever been started (for the specific depth if provided)
  everStartedNodeSet <- case maybeDepth of
    Nothing -> getEverStartedNodes scheduler
    Just depth -> getEverStartedNodesByDepth scheduler depth
  let everStartedNodeCount = HS.size everStartedNodeSet

  -- Get split nodes for this depth (or all depths)
  splitNodes <- case maybeDepth of
    Just depth -> getSplitNodesByDepth scheduler depth
    Nothing -> do
      -- Collect split nodes from all depths
      splitNodesMap <- readIORef splitNodesByDepth
      return $ HS.unions $ HM.elems splitNodesMap

  -- Compute nodes that don't need children
  let doNotNeedChildNodes = HS.unions [succeedNodes, unsatNodes, inferredFailureNodes]

  -- Read all node states
  nodeStatesMap <- readIORef nodeStates

  -- Get all nodes for the current depth (or all depths)
  allNodes <- case maybeDepth of
    Just depth -> getNodesByDepth scheduler depth
    Nothing -> return $ HS.fromList $ HM.keys nodeStatesMap

  -- Compute nodes that need to be split (not already split and don't fall into doNotNeedChild)
  let needSplitNodes = allNodes `HS.difference` HS.union splitNodes doNotNeedChildNodes

  -- Validate node status assignments
  let statusMappings =
        [ (StatusViable, viableNodes),
          (StatusRefining, refiningNodes),
          (StatusSucceeded, succeedNodes),
          (StatusUnsat, unsatNodes),
          (StatusUnknown, unknownNodes),
          (StatusTerminated, terminatedNodes),
          (StatusInferredFailure, inferredFailureNodes),
          (StatusJustStarted, justStartedNodes),
          (StatusNotYetStarted, notYetStartedNodes)
        ]

  validateNodeStatus allNodes statusMappings (logMultiLineDoc logger NOTICE)

  -- Helper function to safely get node state
  let getNodeState nid =
        case HM.lookup nid nodeStatesMap of
          Just s -> (nid, s)
          Nothing -> error $ "Node " ++ show nid ++ " not found in nodeStates"

  -- Convert each set of nodes to (NodeId, NodeState) pairs
  let nodeSetToStatePairs nodeSet = map getNodeState (HS.toList nodeSet)

  let viableStatePairs = nodeSetToStatePairs viableNodes
      refiningStatePairs = nodeSetToStatePairs refiningNodes
      succeedStatePairs = nodeSetToStatePairs succeedNodes
      unsatStatePairs = nodeSetToStatePairs unsatNodes
      unknownStatePairs = nodeSetToStatePairs unknownNodes
      terminatedStatePairs = nodeSetToStatePairs terminatedNodes
      inferredFailureStatePairs = nodeSetToStatePairs inferredFailureNodes
      justStartedStatePairs = nodeSetToStatePairs justStartedNodes

  -- All nodes that have ever started
  let everStartedStatePairs =
        viableStatePairs
          ++ refiningStatePairs
          ++ succeedStatePairs
          ++ unsatStatePairs
          ++ unknownStatePairs
          ++ terminatedStatePairs
          ++ inferredFailureStatePairs
          ++ justStartedStatePairs

  let allNodeNum = length everStartedStatePairs + HS.size notYetStartedNodes
  let everStartedNum = length everStartedStatePairs
  let notStarted = HS.size notYetStartedNodes
  let splitNodesCount = HS.size splitNodes

  -- Sort nodes by elapsed time and create index mapping
  let startedWithLinspace =
        zip [0 ..] $
          sortOn (nodeStateCurrentElapsedTime curTime . snd) everStartedStatePairs

  -- Create a mapping from NodeId to sortedIdx
  let nodeIdToSortedIdx =
        HM.fromList [(nid, idx) | (idx, (nid, _)) <- startedWithLinspace]

  -- Convert state pairs to NodeStats objects
  let toNodeStats = map (\(nid, s) -> nodeStateToNodeStats curTime (nid, s) nodeIdToSortedIdx)

  -- Create message statistics
  let logsWithLinspace =
        map
          (second $ second nodeStateMajorRelativeTimeLog)
          startedWithLinspace

  let createMessageStats messageTypeToCheck = do
        (idx, (nid, logs)) <- logsWithLinspace
        (diffTime, msg) <- logs
        guard $ responseType msg == Right messageTypeToCheck
        return $ MessageStat nid idx (realToFrac diffTime :: Double)

  let viableMsgs = createMessageStats MessageViable
      generalizationFailureMsgs = createMessageStats MessageGeneralizationFailure
      succeedMsgs = createMessageStats (MessageSuccess False)
      generalizationSucceedMsgs = createMessageStats (MessageSuccess True)

  -- Convert state pairs to NodeStats
  let viableNodeStats = toNodeStats viableStatePairs
      refiningNodeStats = toNodeStats refiningStatePairs
      succeedNodeStats = toNodeStats succeedStatePairs
      unsatNodeStats = toNodeStats unsatStatePairs
      unknownNodeStats = toNodeStats unknownStatePairs
      terminatedNodeStats = toNodeStats terminatedStatePairs
      inferredFailureNodeStats = toNodeStats inferredFailureStatePairs
      justStartedNodeStats = toNodeStats justStartedStatePairs
      allStartedStats = toNodeStats (map snd startedWithLinspace)

  -- Create SummaryStats for each category
  let createSummary = createSummaryStats allNodeNum everStartedNum

  return $
    Stats
      (createSummary allStartedStats)
      notStarted
      splitNodesCount
      (HS.size doNotNeedChildNodes)
      (HS.size needSplitNodes)
      everStartedNodeCount
      (createSummary viableNodeStats)
      (createSummary refiningNodeStats)
      (createSummary succeedNodeStats)
      (createSummary unsatNodeStats)
      (createSummary unknownNodeStats)
      (createSummary terminatedNodeStats)
      (createSummary inferredFailureNodeStats)
      (createSummary justStartedNodeStats)
      viableMsgs
      generalizationFailureMsgs
      succeedMsgs
      generalizationSucceedMsgs

-- | Plots statistics to an SVG file
plotStatistics :: FilePath -> String -> Stats -> IO ()
plotStatistics path title stats = do
  -- Create point data
  let pointsMap = createPointsMap stats
      annotationPoints = getAnnotationPoints pointsMap
      nonEmptyCategories = getNonEmptyCategories pointsMap
      categoryColors = getCategoryColors nonEmptyCategories
      categoryShapes = getCategoryShapes nonEmptyCategories

  -- Generate the SVG file
  toFile (FileOptions (1600, 900) SVG) path $ do
    -- Set the plot title
    layout_title .= title

    -- Plot time line for all started nodes
    plotTimeLine stats

    -- Plot annotations if any exist
    plotAnnotations annotationPoints

    -- Set colors and shapes for all categories
    setColors $ map opaque categoryColors
    setShapes categoryShapes

    -- Plot all categories
    plotCategories nonEmptyCategories pointsMap
  where
    -- Plot the time line for all started nodes
    plotTimeLine stats =
      plot $ line "time" [map snd $ summaryStatsToPoints $ allStartedNodeStats stats]
      where
        summaryStatsToPoints = map nodeStatToPoint . nodeStats
        nodeStatToPoint NodeStats {nodeId, sortedIdx, nodeTime} =
          (nodeId, (fromIntegral sortedIdx :: Double, nodeTime))

    -- Plot annotations if any exist
    plotAnnotations annotationPoints =
      unless (null annotationPoints) $
        plot $
          liftEC $ do
            plot_annotation_values .= map (\(nid, (x, y)) -> (x, y, show nid)) annotationPoints
            plot_annotation_style . font_size .= 8

    -- Plot all categories with points
    plotCategories categories pointsMap =
      forM_ categories $ \category ->
        plot $ points (show category) $ map snd $ pointsMap HM.! category

-- | Helper functions for plotStatistics

-- | Create a mapping of point category to point data
createPointsMap :: Stats -> HM.HashMap StatCategory [(NodeId, (Double, Double))]
createPointsMap stats =
  HM.fromList $ messagePointsData ++ nodePointsData
  where
    -- Message points data
    messagePointsData =
      [ (MessageCategory MessageViable, convertMessageStats $ viableMessageStats stats),
        (MessageCategory MessageGeneralizationFailure, convertMessageStats $ generalizationFailureMessageStats stats),
        (MessageCategory (MessageSuccess False), convertMessageStats $ succeedMessageStats stats),
        (MessageCategory (MessageSuccess True), convertMessageStats $ generalizationSucceedMessageStats stats)
      ]

    -- Node points data
    nodePointsData =
      [ (NodeCategory StatusViable, convertSummaryStats $ viableStats stats),
        (NodeCategory StatusRefining, convertSummaryStats $ refiningStats stats),
        (NodeCategory StatusSucceeded, convertSummaryStats $ succeedStats stats),
        (NodeCategory StatusUnsat, convertSummaryStats $ unsatStats stats),
        (NodeCategory StatusUnknown, convertSummaryStats $ unknownStats stats),
        (NodeCategory StatusTerminated, convertSummaryStats $ terminatedStats stats),
        (NodeCategory StatusInferredFailure, convertSummaryStats $ inferredFailureStats stats),
        (NodeCategory StatusJustStarted, convertSummaryStats $ justStartedStats stats)
      ]

    -- Convert message stats to points
    convertMessageStats :: [MessageStat] -> [(NodeId, (Double, Double))]
    convertMessageStats = map convertMessageStat

    -- Convert a single message stat to a point
    convertMessageStat :: MessageStat -> (NodeId, (Double, Double))
    convertMessageStat MessageStat {nodeIdOrigin, sortedIdx, msgTime} =
      (nodeIdOrigin, (fromIntegral sortedIdx :: Double, msgTime))

    -- Convert summary stats to points
    convertSummaryStats :: SummaryStats -> [(NodeId, (Double, Double))]
    convertSummaryStats = map convertNodeStat . nodeStats

    -- Convert a node stat to a point
    convertNodeStat :: NodeStats -> (NodeId, (Double, Double))
    convertNodeStat NodeStats {nodeId, sortedIdx, nodeTime} =
      (nodeId, (fromIntegral sortedIdx :: Double, nodeTime))

-- | Get points for special annotations
getAnnotationPoints :: HM.HashMap StatCategory [(NodeId, (Double, Double))] -> [(NodeId, (Double, Double))]
getAnnotationPoints pointsMap =
  concatMap snd $
    filter (\(key, _) -> key `elem` annotationCategories) $
      HM.toList pointsMap

-- | Get categories with non-empty point lists
getNonEmptyCategories :: HM.HashMap StatCategory [(NodeId, (Double, Double))] -> [StatCategory]
getNonEmptyCategories = HM.keys . HM.filter (not . null)

-- | Get colors for categories
getCategoryColors :: [StatCategory] -> [Colour Double]
getCategoryColors categories = [colorMap HM.! cat | cat <- categories]

-- | Get shapes for categories
getCategoryShapes :: [StatCategory] -> [PointShape]
getCategoryShapes categories = [shapeMap HM.! cat | cat <- categories]

-- | Collects statistics for all depths in the scheduler
collectAllStats ::
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
  IO (Stats, HM.HashMap Int Stats)
collectAllStats scheduler@Scheduler {..} = do
  curTime <- getCurrentTime

  -- Collect overall stats (all depths)
  allStats <- collectStats curTime scheduler Nothing

  -- Get max depth
  dcTreeRef <- readIORef dcTree
  nodeStatesMap <- readIORef nodeStates
  let nodeIds = HM.keys nodeStatesMap
  let depths = map (nodeDepth dcTreeRef) nodeIds

  if null depths
    then return (allStats, HM.empty)
    else do
      let maxDepth = maximum depths

      -- Collect stats for each depth
      let collectDepthStats depth
            | depth > maxDepth = return HM.empty
            | otherwise = do
                stats <- collectStats curTime scheduler (Just depth)
                remaining <- collectDepthStats (depth + 1)
                return $ HM.insert depth stats remaining

      depthStats <- collectDepthStats 0
      return (allStats, depthStats)

-- | Creates a formatted display string for a statistics category
formatStatsCategory ::
  Int -> -- Maximum name length
  Int -> -- Maximum number length
  Int -> -- Maximum percentAll length
  Int -> -- Maximum percentEverStarted length
  Int -> -- Maximum avgTime length
  Int -> -- Maximum time75Percentile length
  Int -> -- Maximum time90Percentile length
  Int -> -- Maximum time95Percentile length
  Int -> -- Maximum avgCollectedExamples length
  Int -> -- Maximum avgInProgressExamples length
  (StatCategory, SummaryStats) -> -- Name and stats pair
  String
formatStatsCategory
  maxNameLen
  maxNumLen
  maxPercentAllLen
  maxPercentEverStartedLen
  maxAvgTimeLen
  maxTime75PercentileLen
  maxTime90PercentileLen
  maxTime95PercentileLen
  maxAvgCollectedExamplesLen
  maxAvgInProgressExamplesLen
  (category, SummaryStats {..}) =
    let name = show category
     in name
          <> ": "
          <> replicate ((maxNameLen + maxNumLen) - (length name + length (show num))) ' '
          <> show num
          <> replicate (maxPercentEverStartedLen - length (showFloat percentEverStarted)) ' '
          <> " ("
          <> showFloat percentEverStarted
          <> "%/"
          <> replicate (maxPercentAllLen - length (showFloat percentAll)) ' '
          <> showFloat percentAll
          <> "%), time(avg/75%/90%/95%): "
          <> replicate (maxAvgTimeLen - length (showFloat avgTime)) ' '
          <> showFloat avgTime
          <> "s/"
          <> replicate (maxTime75PercentileLen - length (showFloat time75Percentile)) ' '
          <> showFloat time75Percentile
          <> "s/"
          <> replicate (maxTime90PercentileLen - length (showFloat time90Percentile)) ' '
          <> showFloat time90Percentile
          <> "s/"
          <> replicate (maxTime95PercentileLen - length (showFloat time95Percentile)) ' '
          <> showFloat time95Percentile
          <> "s, examples(collected/in progress): "
          <> replicate (maxAvgCollectedExamplesLen - length (showFloat avgCollectedExamples)) ' '
          <> showFloat avgCollectedExamples
          <> "/"
          <> replicate (maxAvgInProgressExamplesLen - length (showFloat avgInProgressExamples)) ' '
          <> showFloat avgInProgressExamples

-- | Logs statistics for a specific category
logStatistics ::
  Maybe Int ->
  Stats ->
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
logStatistics
  depth
  stats
  scheduler@Scheduler {config = SchedulerConfig {..}, ..} = do
    let statistics =
          [ (NodeCategory StatusViable, viableStats stats),
            (NodeCategory StatusRefining, refiningStats stats),
            (NodeCategory StatusSucceeded, succeedStats stats),
            (NodeCategory StatusUnsat, unsatStats stats),
            (NodeCategory StatusUnknown, unknownStats stats),
            (NodeCategory StatusTerminated, terminatedStats stats),
            (NodeCategory StatusInferredFailure, inferredFailureStats stats),
            (NodeCategory StatusJustStarted, justStartedStats stats)
          ]
    let filteredStatistics = filter (\(_, s) -> num s > 0) statistics

    -- Get the first not fully split depth
    currentNotFullySplitDepth <- getFirstNotFullySplitDepth scheduler

    -- Calculate maximum field lengths for formatting
    let maxNameLen = maximum $ map (length . show . fst) filteredStatistics
    let maxNumLen = maximum $ map (length . show . num . snd) filteredStatistics
    let maxPercentAllLen = maximum $ map (length . showFloat . percentAll . snd) filteredStatistics
    let maxPercentEverStartedLen = maximum $ map (length . showFloat . percentEverStarted . snd) filteredStatistics
    let maxAvgTimeLen = maximum $ map (length . showFloat . avgTime . snd) filteredStatistics
    let maxTime75PercentileLen = maximum $ map (length . showFloat . time75Percentile . snd) filteredStatistics
    let maxTime90PercentileLen = maximum $ map (length . showFloat . time90Percentile . snd) filteredStatistics
    let maxTime95PercentileLen = maximum $ map (length . showFloat . time95Percentile . snd) filteredStatistics
    let maxAvgCollectedExamplesLen = maximum $ map (length . showFloat . avgCollectedExamples . snd) filteredStatistics
    let maxAvgInProgressExamplesLen = maximum $ map (length . showFloat . avgInProgressExamples . snd) filteredStatistics

    -- Format each statistics category
    let formattedStats =
          map
            ( formatStatsCategory
                maxNameLen
                maxNumLen
                maxPercentAllLen
                maxPercentEverStartedLen
                maxAvgTimeLen
                maxTime75PercentileLen
                maxTime90PercentileLen
                maxTime95PercentileLen
                maxAvgCollectedExamplesLen
                maxAvgInProgressExamplesLen
            )
            filteredStatistics

    let firstLine = case depth of
          Just depth -> "Depth " <> pformat depth
          Nothing -> "All started (not fully split depth: " <> pformat currentNotFullySplitDepth <> ")"

    -- Message statistics (deduplicated by node ID)
    let uniqueViableNodes = HS.size $ HS.fromList $ map nodeIdOrigin $ viableMessageStats stats
    let uniqueGeneralizationFailureNodes = HS.size $ HS.fromList $ map nodeIdOrigin $ generalizationFailureMessageStats stats
    let uniqueSucceedNodes = HS.size $ HS.fromList $ map nodeIdOrigin $ succeedMessageStats stats
    let uniqueGeneralizationSucceedNodes = HS.size $ HS.fromList $ map nodeIdOrigin $ generalizationSucceedMessageStats stats

    let messageStatsLine =
          "Unique nodes: "
            <> pformat uniqueViableNodes
            <> " viable, "
            <> pformat uniqueGeneralizationFailureNodes
            <> " generalization failures, "
            <> pformat uniqueSucceedNodes
            <> " regular successes, "
            <> pformat uniqueGeneralizationSucceedNodes
            <> " generalization successes"

    -- Log the formatted statistics
    logMultiLineDoc logger NOTICE $
      nest 2 $
        vsep $
          [ firstLine
              <> " ("
              <> pformat (num $ allStartedNodeStats stats)
              <> " nodes + "
              <> pformat (numNotStarted stats)
              <> " in queue, "
              <> pformat (numSplitNodes stats)
              <> " splitted, "
              <> pformat (numDoNotNeedChildNodes stats)
              <> " do not need child, "
              <> pformat (numNeedSplitNodes stats)
              <> " need split, "
              <> pformat (numEverStartedNodes stats)
              <> " ever started):",
            messageStatsLine
          ]
            <> (fromString <$> formattedStats)

-- | Generates and logs statistics for each depth in the scheduler
logLayeredStatistics ::
  HM.HashMap Int Stats ->
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
logLayeredStatistics
  depthStats
  scheduler@Scheduler {config = SchedulerConfig {..}, ..} = do
    let maxDepth = maximum $ HM.keys depthStats

    let processDepth depth
          | depth > maxDepth = return ()
          | otherwise = do
              let title = case cmdline of
                    Just cmdline -> cmdline <> " (depth " <> show depth <> ")"
                    Nothing -> "Depth " <> show depth
              let stats = depthStats HM.! depth

              -- Plot and log statistics for this depth
              when enablePlotting $
                plotStatistics
                  (logRootDir logConfig <> "/stats." <> show depth <> ".svg")
                  title
                  stats
              logStatistics (Just depth) stats scheduler

              -- Process next depth
              processDepth (depth + 1)

    unless (HM.null depthStats) $ processDepth 0

-- | Reports statistics for a scheduler
reportStatistics ::
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
reportStatistics
  scheduler@Scheduler
    { config = SchedulerConfig {..},
      ..
    } = do
    -- Collect statistics for all nodes and per-depth
    (stats, layerStats) <- collectAllStats scheduler

    -- Generate title for overall statistics
    let title = fromMaybe "Statistics" cmdline

    -- Log and plot overall statistics
    logStatistics Nothing stats scheduler
    when enablePlotting $
      plotStatistics (logRootDir logConfig <> "/stats.svg") title stats

    -- Log and plot per-depth statistics
    logLayeredStatistics layerStats scheduler
