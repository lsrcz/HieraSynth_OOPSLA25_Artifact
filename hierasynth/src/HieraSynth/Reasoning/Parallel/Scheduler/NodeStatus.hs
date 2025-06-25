{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module HieraSynth.Reasoning.Parallel.Scheduler.NodeStatus
  ( NodeStatus (..),
    NodeAction (..),
    StatusType (..),
    statusType,
    statusTypeIsEnded,
    statusTypeIsRunning,
    statusTypeIsDetermined,
    statusTypeIsNotYetStarted,
    nodeStatusBestProgWithCost,
    nodeStatusInferFailureTransition,
    nodeStatusTransition,
    pformatNodeStatusSummary,
    statusTypeDoNotNeedChild,
  )
where

import Data.Hashable (Hashable)
import qualified Data.Text as T
import Grisette (Doc, PPrint (pformat), SolvingFailure (Unsat), derive, nest, vsep)
import HieraSynth.Program.Concrete (ProgPPrint)
import HieraSynth.Program.SymbolTable (SymbolTable)
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
import HieraSynth.Util.Show (showAsText)

data NodeStatus conProg
  = NodeViable
      { _curTrack :: Int,
        _viableProg :: SymbolTable conProg
      }
  | NodeRefining {_curTrack :: Int, _cost :: Int, _prog :: SymbolTable conProg}
  | NodeSucceeded
      { _lastIterErrorMsg :: Maybe T.Text,
        _cost :: Int,
        _prog :: SymbolTable conProg
      }
  | NodeFailed {_failure :: SolvingFailure}
  | NodeTerminated {_errorMsg :: T.Text}
  | NodeInferredFailure
  | NodeStarted
  | NodeNotYetStarted

derive [''NodeStatus] [''Eq, ''Show]

data StatusType
  = StatusViable
  | StatusRefining
  | StatusSucceeded
  | StatusUnsat
  | StatusUnknown
  | StatusTerminated
  | StatusInferredFailure
  | StatusJustStarted
  | StatusNotYetStarted

derive [''StatusType] [''Eq, ''Show, ''PPrint, ''Hashable, ''Ord]

statusType :: NodeStatus conProg -> StatusType
statusType status = case status of
  NodeViable {} -> StatusViable
  NodeRefining {} -> StatusRefining
  NodeSucceeded {} -> StatusSucceeded
  NodeFailed Unsat -> StatusUnsat
  NodeFailed _ -> StatusUnknown
  NodeTerminated {} -> StatusTerminated
  NodeInferredFailure -> StatusInferredFailure
  NodeStarted -> StatusJustStarted
  NodeNotYetStarted -> StatusNotYetStarted

statusTypeIsEnded :: StatusType -> Bool
statusTypeIsEnded status =
  status == StatusSucceeded
    || status == StatusUnsat
    || status == StatusUnknown
    || status == StatusTerminated
    || status == StatusInferredFailure

statusTypeIsRunning :: StatusType -> Bool
statusTypeIsRunning StatusNotYetStarted = False
statusTypeIsRunning StatusJustStarted = True
statusTypeIsRunning StatusViable = True
statusTypeIsRunning StatusRefining = True
statusTypeIsRunning _ = False

statusTypeIsDetermined :: StatusType -> Bool
statusTypeIsDetermined status =
  status == StatusSucceeded
    || status == StatusUnsat
    || status == StatusUnknown
    || status == StatusInferredFailure

statusTypeDoNotNeedChild :: StatusType -> Bool
statusTypeDoNotNeedChild StatusSucceeded = True
statusTypeDoNotNeedChild StatusUnsat = True
statusTypeDoNotNeedChild StatusInferredFailure = True
statusTypeDoNotNeedChild _ = False

statusTypeIsNotYetStarted :: StatusType -> Bool
statusTypeIsNotYetStarted StatusNotYetStarted = True
statusTypeIsNotYetStarted _ = False

nodeStatusBestProgWithCost ::
  NodeStatus conProg -> Maybe (Int, SymbolTable conProg)
nodeStatusBestProgWithCost NodeViable {} = Nothing
nodeStatusBestProgWithCost (NodeRefining _ cost m) = Just (cost, m)
nodeStatusBestProgWithCost (NodeSucceeded _ cost m) = Just (cost, m)
nodeStatusBestProgWithCost NodeFailed {} = Nothing
nodeStatusBestProgWithCost (NodeTerminated _) = Nothing
nodeStatusBestProgWithCost NodeInferredFailure = Nothing
nodeStatusBestProgWithCost NodeStarted {} = Nothing
nodeStatusBestProgWithCost NodeNotYetStarted = Nothing

data NodeAction
  = Refine {_succeed :: Bool, _nodeBestCostKnowledge :: Maybe Int}
  | RefineAndSplitSketch {_nodeBestCostKnowledge :: Maybe Int}
  | CleanUpAndSplitSketch
  | MarkFailure

_ensureNewCostIsSmaller :: Int -> Int -> a -> a
_ensureNewCostIsSmaller newCost oldCost x =
  if newCost >= oldCost
    then error "Should not happen"
    else x

nodeStatusInferFailureTransition :: NodeStatus conProg -> NodeStatus conProg
nodeStatusInferFailureTransition NodeFailed {} =
  error "Should not happen" -- status
nodeStatusInferFailureTransition status@NodeInferredFailure {} = status
nodeStatusInferFailureTransition NodeTerminated {} = NodeInferredFailure
nodeStatusInferFailureTransition (NodeSucceeded _ cost prog) =
  NodeSucceeded Nothing cost prog
nodeStatusInferFailureTransition (NodeViable {}) = NodeInferredFailure
nodeStatusInferFailureTransition (NodeRefining _ cost prog) =
  NodeSucceeded Nothing cost prog
nodeStatusInferFailureTransition NodeStarted {} = NodeInferredFailure
nodeStatusInferFailureTransition NodeNotYetStarted = NodeInferredFailure

nodeStatusTransition ::
  NodeStatus conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeStatusTransition NodeSucceeded {} = error "Should not happen"
nodeStatusTransition NodeFailed {} = error "Should not happen"
nodeStatusTransition NodeInferredFailure = error "Should not happen"
nodeStatusTransition NodeTerminated {} = error "Should not happen"
nodeStatusTransition NodeNotYetStarted = error "Shouldnot happen"
nodeStatusTransition NodeStarted = nodeStartedTransition
nodeStatusTransition oldStatus@(NodeViable {}) = nodeViableTransition oldStatus
nodeStatusTransition NodeRefining {..} =
  nodeRefiningTransition _curTrack _cost _prog

nodeStartedTransition ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeStartedTransition (Left msg) =
  (NodeTerminated msg, CleanUpAndSplitSketch)
nodeStartedTransition (Right (Failure _ failure)) =
  (NodeFailed failure, MarkFailure)
nodeStartedTransition (Right (Viable curTrack _ cost prog)) =
  (NodeViable curTrack prog, RefineAndSplitSketch cost)
nodeStartedTransition (Right GeneralizationFailure {}) =
  error "Should not happen"
nodeStartedTransition (Right (Success _ curTrack _ cost prog)) =
  (NodeRefining curTrack cost prog, RefineAndSplitSketch $ Just cost)
nodeStartedTransition (Right (GotExample _ cost)) =
  (NodeStarted, Refine False cost)

nodeViableTransition ::
  NodeStatus conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeViableTransition _ (Left msg) =
  (NodeTerminated msg, CleanUpAndSplitSketch)
nodeViableTransition _ (Right (Failure _ failure)) =
  (NodeFailed failure, MarkFailure)
nodeViableTransition _ (Right (Viable curTrack _ cost prog)) =
  (NodeViable curTrack prog, RefineAndSplitSketch cost)
nodeViableTransition oldStatus (Right (GeneralizationFailure _ cost _)) =
  (oldStatus, RefineAndSplitSketch cost)
nodeViableTransition _ (Right (Success _ curTrack _ cost prog)) =
  (NodeRefining curTrack cost prog, Refine True $ Just cost)
nodeViableTransition oldStatus (Right (GotExample _ cost)) =
  (oldStatus, Refine False cost)

nodeRefiningTransition ::
  Int ->
  Int ->
  SymbolTable conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeRefiningTransition _ oldCost oldProg (Left msg) =
  (NodeSucceeded (Just msg) oldCost oldProg, CleanUpAndSplitSketch)
nodeRefiningTransition _ oldCost oldProg (Right (Failure _ Unsat)) =
  (NodeSucceeded Nothing oldCost oldProg, MarkFailure)
nodeRefiningTransition _ oldCost oldProg (Right (Failure _ failure)) =
  ( NodeSucceeded
      (Just $ "Solver finally failed with: " <> showAsText failure)
      oldCost
      oldProg,
    CleanUpAndSplitSketch
  )
nodeRefiningTransition
  _
  oldCost
  oldProg
  (Right (Viable newTrack _ bestKnownCost _)) =
    (NodeRefining newTrack oldCost oldProg, Refine True bestKnownCost)
nodeRefiningTransition
  _
  oldCost
  oldProg
  (Right (GeneralizationFailure newTrack bestKnownCost _)) =
    (NodeRefining newTrack oldCost oldProg, Refine True bestKnownCost)
nodeRefiningTransition
  _
  oldCost
  _
  (Right (Success _ newTrack _ newCost newProg)) =
    _ensureNewCostIsSmaller
      newCost
      oldCost
      (NodeRefining newTrack newCost newProg, Refine True $ Just newCost)
nodeRefiningTransition
  oldTrack
  oldCost
  oldProg
  (Right (GotExample _ bestKnownCost)) =
    (NodeRefining oldTrack oldCost oldProg, Refine True bestKnownCost)

pformatNodeStatusSummary :: NodeStatus conProg -> Doc ann
pformatNodeStatusSummary NodeInferredFailure = "NodeInferredFailure"
pformatNodeStatusSummary (NodeTerminated _) =
  "NodeTerminated"
pformatNodeStatusSummary NodeNotYetStarted = "NodeNotYetStarted"
pformatNodeStatusSummary (NodeRefining track cost _) =
  "NodeRefining (track "
    <> pformat track
    <> ", cost "
    <> pformat cost
    <> ")"
pformatNodeStatusSummary (NodeViable track _) =
  "NodeViable (track: " <> pformat track <> ")"
pformatNodeStatusSummary (NodeSucceeded maybeCrashMessage cost _) =
  "NodeSucceeded (cost "
    <> pformat cost
    <> case maybeCrashMessage of
      Nothing -> ")"
      Just _ -> ", not finished)"
pformatNodeStatusSummary (NodeFailed r) =
  "NodeFailed "
    <> pformat r
pformatNodeStatusSummary NodeStarted =
  "NodeStarted"

instance (ProgPPrint conProg) => PPrint (NodeStatus conProg) where
  pformat NodeInferredFailure = "NodeInferredFailure"
  pformat (NodeTerminated t) =
    "NodeTerminated "
      <> pformat t
  pformat NodeNotYetStarted = "NodeNotYetStarted"
  pformat (NodeRefining track cost m) =
    nest 2 $
      vsep
        [ "NodeRefining (track "
            <> pformat track
            <> ", cost "
            <> pformat cost
            <> ")",
          pformat m
        ]
  pformat (NodeViable track m) =
    nest 2 $
      vsep
        [ "NodeViable (track "
            <> pformat track
            <> ")",
          pformat m
        ]
  pformat (NodeSucceeded maybeCrashMessage cost m) =
    nest 2 $
      vsep $
        concat
          [ [ "NodeSucceeded (cost "
                <> pformat cost
                <> ")"
            ],
            case maybeCrashMessage of
              Nothing -> []
              Just msg -> ["Not finished because " <> pformat msg],
            [pformat m]
          ]
  pformat (NodeFailed r) = "NodeFailed " <> pformat r
  pformat NodeStarted =
    "NodeStarted"
