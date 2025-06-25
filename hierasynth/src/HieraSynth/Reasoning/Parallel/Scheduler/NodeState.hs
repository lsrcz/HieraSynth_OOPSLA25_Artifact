{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HieraSynth.Reasoning.Parallel.Scheduler.NodeState
  ( NodeMessageLog,
    NodeMessageAbsoluteTimeLog,
    NodeMessageRelativeTimeLog,
    NodeState (..),
    nodeStateMajorRelativeTimeLog,
    nodeStateCurrentElapsedTime,
    pformatNodeStateSummary,
    pformatNodeStateSummaryWithElapsedTime,
    nodeStateResetTransition,
    nodeStateStartTransition,
    nodeStateTransition,
    nodeStateInferFailureTransition,
    nodeStateNumCollectedExamples,
    nodeStateNumInProgressExamples,
  )
where

import Control.Monad (unless, when)
import Data.Maybe (isJust)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Grisette (Doc, viaShow, (<+>))
import HieraSynth.Reasoning.Parallel.Scheduler.NodeStatus
  ( NodeAction,
    NodeStatus (NodeNotYetStarted, NodeStarted),
    StatusType (StatusNotYetStarted),
    nodeStatusInferFailureTransition,
    nodeStatusTransition,
    pformatNodeStatusSummary,
    statusType,
    statusTypeIsEnded,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.Process
  ( Message
      ( Failure,
        GeneralizationFailure,
        GotExample,
        Success,
        Viable
      ),
    ProcessResponse,
  )

type NodeMessageLog time conProg symSemObj symVal conSemObj conVal matcher =
  [ ( time,
      ProcessResponse conProg symSemObj symVal conSemObj conVal matcher
    )
  ]

type NodeMessageAbsoluteTimeLog conProg symSemObj symVal conSemObj conVal matcher =
  NodeMessageLog UTCTime conProg symSemObj symVal conSemObj conVal matcher

type NodeMessageRelativeTimeLog conProg symSemObj symVal conSemObj conVal matcher =
  NodeMessageLog NominalDiffTime conProg symSemObj symVal conSemObj conVal matcher

data NodeState conProg symSemObj symVal conSemObj conVal matcher = NodeState
  { nodeStatus :: NodeStatus conProg,
    nodeStartTime :: Maybe UTCTime,
    nodeEndTime :: Maybe UTCTime,
    nodeResponseReverseLog ::
      NodeMessageAbsoluteTimeLog conProg symSemObj symVal conSemObj conVal matcher,
    nodeMajorResponseReverseLog ::
      NodeMessageAbsoluteTimeLog conProg symSemObj symVal conSemObj conVal matcher,
    nodeGotExampleResponseLogSinceLastMajorResponse ::
      NodeMessageAbsoluteTimeLog conProg symSemObj symVal conSemObj conVal matcher
  }

nodeStateMajorRelativeTimeLog ::
  NodeState conProg symSemObj symVal conSemObj conVal matcher ->
  NodeMessageRelativeTimeLog conProg symSemObj symVal conSemObj conVal matcher
nodeStateMajorRelativeTimeLog
  (NodeState _ (Just startTime) _ _ revLog _) =
    reverse $ map (\(t, r) -> (diffUTCTime t startTime, r)) revLog
nodeStateMajorRelativeTimeLog (NodeState _ Nothing _ _ [] _) = []
nodeStateMajorRelativeTimeLog _ = error "Should not happen"

nodeStateCurrentElapsedTime ::
  UTCTime ->
  NodeState conProg symSemObj symVal conSemObj conVal matcher ->
  NominalDiffTime
nodeStateCurrentElapsedTime curTime NodeState {..} = do
  case (nodeStartTime, nodeEndTime) of
    (Just startTime, Just endTime) -> diffUTCTime endTime startTime
    (Just startTime, Nothing) -> diffUTCTime curTime startTime
    (Nothing, Just _) -> error "Should not happen"
    (Nothing, Nothing) -> 0

pformatNodeStateSummary ::
  NodeState conProg symSemObj symVal conSemObj conVal matcher -> Doc ann
pformatNodeStateSummary NodeState {..} = pformatNodeStatusSummary nodeStatus

pformatNodeStateSummaryWithElapsedTime ::
  UTCTime ->
  NodeState conProg symSemObj symVal conSemObj conVal matcher ->
  Doc ann
pformatNodeStateSummaryWithElapsedTime curTime nodeState@NodeState {..} =
  let curElapsedTime = nodeStateCurrentElapsedTime curTime nodeState
      elapsedTime = "(" <> viaShow curElapsedTime <> ")"
   in pformatNodeStatusSummary nodeStatus
        <+> elapsedTime

nodeStateResetTransition ::
  NodeState conProg symSemObj symVal conSemObj conVal matcher ->
  IO (NodeState conProg symSemObj symVal conSemObj conVal matcher)
nodeStateResetTransition NodeState {..} =
  return
    NodeState
      { nodeStatus = NodeNotYetStarted,
        nodeStartTime = Nothing,
        nodeEndTime = Nothing,
        nodeResponseReverseLog = [],
        nodeMajorResponseReverseLog = [],
        nodeGotExampleResponseLogSinceLastMajorResponse = []
      }

nodeStateStartTransition ::
  NodeState conProg symSemObj symVal conSemObj conVal matcher ->
  IO (NodeState conProg symSemObj symVal conSemObj conVal matcher)
nodeStateStartTransition NodeState {..} = do
  unless (statusType nodeStatus == StatusNotYetStarted) $
    error "Can only start a node that is not yet started"
  curTime <- getCurrentTime
  return
    NodeState
      { nodeStatus = NodeStarted,
        nodeStartTime = Just curTime,
        nodeEndTime = Nothing,
        nodeResponseReverseLog = [],
        nodeMajorResponseReverseLog = [],
        nodeGotExampleResponseLogSinceLastMajorResponse = []
      }

nodeStateTransition ::
  UTCTime ->
  NodeState conProg symSemObj symVal conSemObj conVal matcher ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  IO (NodeState conProg symSemObj symVal conSemObj conVal matcher, NodeAction)
nodeStateTransition curTime NodeState {..} response = do
  let (newStatus, nextStep) = nodeStatusTransition nodeStatus response
  let newResponseReverseLog = (curTime, response) : nodeResponseReverseLog
  let newMajorReverseLog = case response of
        Right (GotExample _ _) -> nodeMajorResponseReverseLog
        _ -> (curTime, response) : nodeMajorResponseReverseLog
  let newGotExampleReverseLog = case response of
        Right (GotExample _ _) ->
          (curTime, response) : nodeGotExampleResponseLogSinceLastMajorResponse
        _ -> []
  let newEndTime =
        if statusTypeIsEnded (statusType newStatus)
          then Just curTime
          else nodeEndTime
  when (not (statusTypeIsEnded (statusType newStatus)) && isJust nodeEndTime) $
    error "A running node should not have an end time"
  return
    ( NodeState
        { nodeStatus = newStatus,
          nodeStartTime = nodeStartTime,
          nodeEndTime = newEndTime,
          nodeResponseReverseLog = newResponseReverseLog,
          nodeMajorResponseReverseLog = newMajorReverseLog,
          nodeGotExampleResponseLogSinceLastMajorResponse =
            newGotExampleReverseLog
        },
      nextStep
    )

nodeStateInferFailureTransition ::
  NodeState conProg symSemObj symVal conSemObj conVal matcher ->
  IO (NodeState conProg symSemObj symVal conSemObj conVal matcher)
nodeStateInferFailureTransition NodeState {..} = do
  let newStatus = nodeStatusInferFailureTransition nodeStatus
  curTime <- getCurrentTime
  return
    NodeState
      { nodeStatus = newStatus,
        nodeStartTime = case nodeStartTime of
          Nothing -> Just curTime
          Just {} -> nodeStartTime,
        nodeEndTime = case nodeEndTime of
          Nothing -> Just curTime
          Just {} -> nodeEndTime,
        nodeResponseReverseLog = nodeResponseReverseLog,
        nodeMajorResponseReverseLog = nodeMajorResponseReverseLog,
        nodeGotExampleResponseLogSinceLastMajorResponse =
          nodeGotExampleResponseLogSinceLastMajorResponse
      }

nodeStateNumCollectedExamples ::
  NodeState conProg symSemObj symVal conSemObj conVal matcher -> Int
nodeStateNumCollectedExamples NodeState {..} =
  sum $ map go $ snd <$> nodeMajorResponseReverseLog
  where
    go (Right (GotExample _ _)) = error "Not a major response"
    go (Right (Success False _ e _ _)) = length e
    go (Right (Success True _ _ _ _)) = 0
    go (Right (Viable _ e _ _)) = length e
    go (Right GeneralizationFailure {}) = 0
    go (Right (Failure e _)) = length e
    go (Left _) = 0

nodeStateNumInProgressExamples ::
  NodeState conProg symSemObj symVal conSemObj conVal matcher -> Int
nodeStateNumInProgressExamples NodeState {..} =
  length nodeGotExampleResponseLogSinceLastMajorResponse
