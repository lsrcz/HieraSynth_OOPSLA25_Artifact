{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HieraSynth.Reasoning.Parallel.Scheduler.Process
  ( ProcessConfig (..),
    NewMinimalCostMessage (..),
    Message (..),
    Process (..),
    ProcessResponse,
    ProcessCostConstraint,
    ProcessConstraint,
    ConProgConstraint,
    runRequestInSubProcess,
    getProcessResponse,
    sendNewMinimalCost,
    processResponseNewCost,
    MessageType (..),
    messageType,
    responseType,
    closeProcessPipes,
  )
where

import Control.Concurrent
  ( newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
    tryPutMVar,
  )
import Control.Concurrent.Async (async, cancelWith)
import Control.Exception (AsyncException (ThreadKilled))
import Control.Monad (unless, when)
import Control.Monad.Except (runExceptT)
import Data.Bytes.Serial (Serial)
import Data.Either (fromRight)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
import GHC.Conc.Signal (setHandler)
import GHC.Generics (Generic)
import Grisette
  ( Doc,
    EvalSym,
    GenSymSimple,
    GrisetteSMTConfig (sbvConfig),
    LogicalOp (true),
    Mergeable,
    PPrint (pformat),
    SMTConfig (transcript),
    Solvable (con),
    Solver,
    SolvingFailure,
    SymEq ((.==)),
    SymOrd ((.<)),
    ToCon,
    allClasses0,
    derive,
    genSymSimple,
    nest,
    simpleMerge,
    viaShow,
    vsep,
    withSolver,
  )
import HieraSynth.Context
  ( AngelicContext,
    ConcreteContext,
    SymbolicContext,
  )
import HieraSynth.Program.Choice.Counting
  ( CountNumProgsEvidence,
    countNumChoicesWithEvidence,
    countNumProgsWithEvidence,
  )
import HieraSynth.Program.Choice.Split (LowestSeqNum, PartitionSpec)
import HieraSynth.Program.Concrete
  ( ProgPPrint,
    eliminateProgTableDeadCode,
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.ProgCost (ProgCost, symbolCost)
import HieraSynth.Program.SymbolTable
  ( ProgReachableSymbols,
    SymbolTable,
    filterByReachableSymbols,
  )
import HieraSynth.Reasoning.Parallel.Scheduler.DCTree (NodeId (NodeId))
import HieraSynth.Reasoning.Parallel.Scheduler.LogConfig
  ( LogConfig (LogConfig, baseDir, progName),
    defaultFormatter,
    logRootDir,
  )
import HieraSynth.Reasoning.Synthesis
  ( Example,
    RunSynthesisTask (solverRunSynthesisTaskExtractCex),
    SomeExample (SomeExample),
    SomeVerifier,
    SynthesisBoundCostTask
      ( SynthesisBoundCostTask,
        synthesisExtraConstraints,
        synthesisInitialExamples,
        synthesisInitialMaxCost,
        synthesisPrecondition,
        synthesisSketchSymbol,
        synthesisSketchTable,
        synthesisSymCostObj,
        synthesisVerifiers
      ),
    SynthesisResult
      ( SynthesisSolverFailure,
        SynthesisSuccess,
        SynthesisVerifierFailure
      ),
  )
import HieraSynth.Util.Logging (logMultiLineDoc)
import HieraSynth.Util.Serialize
  ( byteStringToWord64,
    nonBlockingReadObject,
    readByteString,
    readObject,
    word64ToByteString,
    writeByteString,
    writeObject,
  )
import HieraSynth.VarId (ConcreteVarId)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Log.Handler (LogHandler (setFormatter))
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger
  ( Logger,
    Priority (DEBUG, NOTICE),
    getLogger,
    removeHandler,
    setHandlers,
    setLevel,
    updateGlobalLogger,
  )
import System.Posix
  ( FdOption (NonBlockingRead),
    ProcessStatus (Exited, Stopped, Terminated),
    closeFd,
    createPipe,
    createProcessGroupFor,
    forkProcess,
    getProcessID,
    getProcessStatus,
    sigHUP,
    sigINT,
    sigTERM,
  )
import System.Posix.ByteString (setFdOption)
import System.Posix.Types (CPid (CPid), Fd, ProcessGroupID, ProcessID)

_createLogger :: LogConfig -> NodeId -> IO Logger
_createLogger logConfig@LogConfig {..} (NodeId nodeId) = do
  let loggerName = T.unpack progName <> "-" <> show nodeId
  defaultFormatter <-
    defaultFormatter
      ("[$time : $loggername@" <> show nodeId <> " : $prio] $msg")
  let setDefaultFormatter lh = return $ setFormatter lh defaultFormatter
  let rootDir = logRootDir logConfig
  h <-
    fileHandler
      (rootDir <> "/" <> show nodeId <> ".log")
      DEBUG
      >>= setDefaultFormatter
  updateGlobalLogger
    loggerName
    (setHandlers [h] . setLevel DEBUG . removeHandler)
  getLogger loggerName

type ProcessCostConstraint costObj cost conProg sketch =
  ( ProgCost costObj conProg Int ConcreteContext,
    ProgCost costObj sketch cost SymbolicContext,
    ProgCost costObj sketch cost AngelicContext
  )

type ProcessConstraint
  sketchSpec
  sketch
  conProg
  costObj
  cost
  symSemObj
  symVal
  conSemObj
  conVal
  matcher =
  ( GenSymSimple sketchSpec sketch,
    ProcessCostConstraint costObj cost conProg sketch,
    ProgPPrint sketchSpec,
    ProgPPrint conProg,
    EvalSym sketch,
    ToCon sketch conProg,
    ProgReachableSymbols conProg,
    SymOrd cost,
    Mergeable cost,
    Num cost,
    Serial conProg,
    Serial cost,
    Serial conVal,
    Serial symSemObj,
    Serial conSemObj,
    Serial matcher,
    Typeable sketch,
    Typeable symSemObj,
    Typeable conSemObj,
    Typeable conVal,
    Typeable matcher,
    Typeable symVal,
    PPrint conVal,
    Hashable sketchSpec,
    Eq sketchSpec,
    LowestSeqNum sketchSpec,
    PartitionSpec sketchSpec
  )

type ConProgConstraint conProg conOp conVarId conType =
  ( ConcreteVarId conVarId,
    conProg ~ Concrete.Prog conOp conVarId conType,
    Concrete.OpPPrint conOp,
    Show conOp,
    PPrint conType,
    Show conType
  )

data
  ProcessConfig
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
  ProcessConfig ::
    ( ProcessConstraint
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
      ConProgConstraint conProg conOp conVarId conType
    ) =>
    { costObj :: costObj,
      sketchSpec :: SymbolTable sketchSpec,
      sketchSymbol :: T.Text,
      logConfig :: LogConfig,
      nodeId :: NodeId,
      verifiers :: Logger -> [[SomeVerifier sketch conProg]],
      generalizationSketchFromFastResult ::
        Maybe (SymbolTable conProg -> SymbolTable sketchSpec),
      transcriptSMT :: Bool,
      doDeadCodeElimination :: Bool,
      generalizationTimeout :: Int,
      exactCost :: Maybe Int,
      initialCost :: Maybe Int,
      countNumProgsEvidence ::
        Maybe (CountNumProgsEvidence (SymbolTable sketchSpec))
    } ->
    ProcessConfig
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

newtype NewMinimalCostMessage = NewMinimalCostMessage
  { newMinimalCost :: Maybe Int
  }

derive [''NewMinimalCostMessage] allClasses0

data ProcessState sketch conProg = ProcessState
  { knownMinimalCost :: Maybe Int,
    verifiers :: [[SomeVerifier sketch conProg]],
    wrChild :: Fd,
    rdChild :: Fd,
    logger :: Logger
  }

_readLogger :: IORef (ProcessState sketch conProg) -> IO Logger
_readLogger stateRef = do
  ProcessState {..} <- readIORef stateRef
  return logger

_readKnownMinimalCost :: IORef (ProcessState sketch conProg) -> IO (Maybe Int)
_readKnownMinimalCost stateRef = do
  ProcessState {..} <- readIORef stateRef
  return knownMinimalCost

_readWrChild :: IORef (ProcessState sketch conProg) -> IO Fd
_readWrChild stateRef = do
  ProcessState {..} <- readIORef stateRef
  return wrChild

_readRdChild :: IORef (ProcessState sketch conProg) -> IO Fd
_readRdChild stateRef = do
  ProcessState {..} <- readIORef stateRef
  return rdChild

_readVerifiers :: IORef (ProcessState sketch conProg) -> IO [[SomeVerifier sketch conProg]]
_readVerifiers stateRef = do
  ProcessState {..} <- readIORef stateRef
  return verifiers

_readTrackBound :: IORef (ProcessState sketch conProg) -> IO Int
_readTrackBound stateRef = do
  verifiers <- _readVerifiers stateRef
  return $ length verifiers - 1

data Message conProg symSemObj symVal conSemObj conVal matcher
  = Viable
      { _curTrack :: Int,
        _examples :: [Example symSemObj symVal conSemObj conVal matcher],
        _knownCost :: Maybe Int,
        _prog :: SymbolTable conProg
      }
  | GeneralizationFailure
      { _curTrack :: Int,
        _knownCost :: Maybe Int,
        _examples :: [Example symSemObj symVal conSemObj conVal matcher]
      }
  | GotExample
      { _example :: Example symSemObj symVal conSemObj conVal matcher,
        _knownCost :: Maybe Int
      }
  | Success
      { _isGeneralizationSuccess :: Bool,
        _curTrack :: Int,
        _examples :: [Example symSemObj symVal conSemObj conVal matcher],
        _cost :: Int,
        _prog :: SymbolTable conProg
      }
  | Failure
      { _examples :: [Example symSemObj symVal conSemObj conVal matcher],
        _failure :: SolvingFailure
      }
  deriving (Show, Generic)

data MessageType
  = MessageViable
  | MessageGeneralizationFailure
  | MessageGotExample
  | MessageSuccess {_isGeneralizationSuccess :: Bool}
  | MessageFailure

derive [''MessageType] [''Eq, ''Show, ''Hashable, ''Ord, ''PPrint]

messageType :: Message conProg symSemObj symVal conSemObj conVal matcher -> MessageType
messageType Viable {} = MessageViable
messageType GeneralizationFailure {} = MessageGeneralizationFailure
messageType GotExample {} = MessageGotExample
messageType (Success isGeneralizationSuccess _ _ _ _) = MessageSuccess isGeneralizationSuccess
messageType Failure {} = MessageFailure

pformatMessageSummary ::
  Message conProg symSemObj symVal conSemObj conVal matcher ->
  Doc ann
pformatMessageSummary (Viable curTrack examples cost _) =
  "Viable (with "
    <> pformat (length examples)
    <> " examples, track "
    <> pformat curTrack
    <> ", best known cost: "
    <> pformat cost
    <> ")"
pformatMessageSummary (GeneralizationFailure curTrack cost examples) =
  "GeneralizationFailure (with "
    <> pformat (length examples)
    <> " examples, track "
    <> pformat curTrack
    <> ", best known cost: "
    <> pformat cost
    <> ")"
pformatMessageSummary (Success isGeneralizationSuccess curTrack examples cost _) =
  (if isGeneralizationSuccess then "GeneralizationSuccess" else "Success")
    <> " (with "
    <> pformat (length examples)
    <> " examples, track "
    <> pformat curTrack
    <> ", cost "
    <> pformat cost
    <> ")"
pformatMessageSummary (GotExample _ cost) =
  "GotExample (best known cost: " <> pformat cost <> ")"
pformatMessageSummary (Failure examples result) =
  "Failure (with "
    <> pformat (length examples)
    <> " examples): "
    <> pformat result

instance
  (ProgPPrint conProg, PPrint conVal) =>
  PPrint (Message conProg symSemObj symVal conSemObj conVal matcher)
  where
  pformat message@(Viable _ _ _ prog) =
    nest 2 $
      vsep
        [ pformatMessageSummary message,
          pformat prog
        ]
  pformat message@GeneralizationFailure {} = pformatMessageSummary message
  pformat message@(GotExample example currentCost) =
    nest 2 $
      vsep
        [ pformatMessageSummary message,
          pformat example,
          pformat currentCost
        ]
  pformat message@(Success _ _ _ _ prog) =
    nest 2 $ vsep [pformatMessageSummary message, pformat prog]
  pformat message@(Failure _ _) = pformatMessageSummary message

instance
  ( Serial conProg,
    Serial symSemObj,
    Serial conSemObj,
    Serial conVal,
    Serial matcher
  ) =>
  Serial (Message conProg symSemObj symVal conSemObj conVal matcher)

_updateKnownMinimalCost :: IORef (ProcessState sketch conProg) -> Maybe Int -> IO ()
_updateKnownMinimalCost stateRef newMinimalCost =
  modifyIORef' stateRef $ \s ->
    s
      { knownMinimalCost =
          case (knownMinimalCost s, newMinimalCost) of
            (Nothing, c) -> c
            (Just _, Nothing) -> knownMinimalCost s
            (Just oldMinimalCost, Just newMinimalCost) ->
              Just $ min oldMinimalCost newMinimalCost
      }

_sendAndWaitForNewMinimalCost ::
  ( Serial conProg,
    Serial symSemObj,
    Serial conSemObj,
    Serial conVal,
    Serial matcher
  ) =>
  IORef (ProcessState sketch conProg) ->
  Message conProg symSemObj symVal conSemObj conVal matcher ->
  IO NewMinimalCostMessage
_sendAndWaitForNewMinimalCost stateRef message = do
  wrChild <- wrChild <$> readIORef stateRef
  rdChild <- rdChild <$> readIORef stateRef
  writeObject wrChild message
  request@(NewMinimalCostMessage newMinimalCost) <- readObject rdChild
  _updateKnownMinimalCost stateRef newMinimalCost
  return request

_solverRunSynthRequest ::
  forall
    handle
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher.
  (Solver handle) =>
  handle ->
  Int ->
  Int ->
  ProcessConfig
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
  NewMinimalCostMessage ->
  IORef (ProcessState sketch conProg) ->
  IO
    ( Int,
      [Example symSemObj symVal conSemObj conVal matcher],
      SynthesisResult conProg
    )
_solverRunSynthRequest
  handle
  startTrack
  lastTrack
  ProcessConfig {..}
  NewMinimalCostMessage {..}
  stateRef = do
    logger <- _readLogger stateRef
    let sketch = genSymSimple sketchSpec "sketch" :: SymbolTable sketch
    allVerifiers <- _readVerifiers stateRef
    logMultiLineDoc logger DEBUG $
      nest 2 $
        vsep
          [ "Start synthesizing with start track: ",
            pformat startTrack,
            "and last track: ",
            pformat lastTrack,
            "and minimal cost: ",
            pformat newMinimalCost
          ]
    let task =
          SynthesisBoundCostTask
            { synthesisVerifiers = concat $ drop startTrack $ take (lastTrack + 1) allVerifiers,
              synthesisSymCostObj = costObj,
              synthesisSketchTable = sketch,
              synthesisSketchSymbol = sketchSymbol,
              synthesisPrecondition = true,
              synthesisInitialMaxCost =
                fromIntegral <$> newMinimalCost :: Maybe cost,
              synthesisInitialExamples = [],
              synthesisExtraConstraints = \(SomeExample ex) -> do
                let symProgCost =
                      symbolCost costObj sketch sketchSymbol ::
                        SymbolicContext cost
                let symProgCostLessThanMaxCost maxCost = simpleMerge $ do
                      eitherCost <- runExceptT symProgCost
                      case eitherCost of
                        Left _ -> return $ con False
                        Right cost -> return $ cost .< maxCost
                let symProgCostEqualToCost expectedCost = simpleMerge $ do
                      eitherCost <- runExceptT symProgCost
                      case eitherCost of
                        Left _ -> return $ con False
                        Right cost -> return $ cost .== expectedCost
                case cast ex of
                  Just (ex :: Example symSemObj symVal conSemObj conVal matcher) -> do
                    currentCost <- _readKnownMinimalCost stateRef
                    _sendAndWaitForNewMinimalCost
                      stateRef
                      ( GotExample ex currentCost ::
                          Message conProg symSemObj symVal conSemObj conVal matcher
                      )
                    newCost <- _readKnownMinimalCost stateRef
                    logMultiLineDoc logger DEBUG $
                      nest 2 $
                        vsep ["Got cex: ", pformat ex]
                    case exactCost of
                      Just cost -> return $ symProgCostEqualToCost (fromIntegral cost)
                      Nothing ->
                        if newCost /= currentCost
                          then do
                            logMultiLineDoc logger DEBUG $
                              nest 2 $
                                vsep ["Update cost: ", pformat newCost]
                            return $
                              symProgCostLessThanMaxCost
                                (fromIntegral $ fromJust newCost)
                          else return $ con True
                  _ -> error "Should not happen"
            }
    (example, r) <- solverRunSynthesisTaskExtractCex handle task
    let castExample ::
          SomeExample sketch conProg ->
          Maybe (Example symSemObj symVal conSemObj conVal matcher)
        castExample (SomeExample e) = cast e
    let example' =
          traverse castExample example ::
            Maybe [Example symSemObj symVal conSemObj conVal matcher]
    case example' of
      Nothing -> error "Failed to cast example"
      Just example'' ->
        case r of
          SynthesisSuccess s -> do
            let prog =
                  if doDeadCodeElimination
                    then
                      eliminateProgTableDeadCode $
                        fromRight undefined $
                          filterByReachableSymbols (HS.singleton sketchSymbol) s
                    else s
            return
              ( fromRight (error "Failed to compute symbolCost") $
                  symbolCost costObj prog sketchSymbol,
                example'',
                SynthesisSuccess prog
              )
          _ -> return (-1, example'', r)

data Process = Process
  { pid :: ProcessID,
    pgid :: ProcessGroupID,
    pipeRd :: Fd,
    pipeWr :: Fd
  }

instance Eq Process where
  Process (CPid pid1) _ _ _ == Process (CPid pid2) _ _ _ = pid1 == pid2

instance Show Process where
  show Process {..} = "Process " <> show pid

instance PPrint Process where
  pformat = viaShow

instance Hashable Process where
  hashWithSalt salt Process {pid = CPid pid} = hashWithSalt salt pid

data ProcessStep conProg
  = InitialStep
  | SynthStep {_curTrack :: Int}
  | GeneralizationStep {_curTrack :: Int, _curProg :: SymbolTable conProg}
  | TerminationStep

isTerminationStep :: ProcessStep conProg -> Bool
isTerminationStep TerminationStep = True
isTerminationStep _ = False

runRequestInSubProcess ::
  forall
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher.
  GrisetteSMTConfig ->
  ProcessConfig
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
  IO Process
runRequestInSubProcess config processConfig@ProcessConfig {..} = do
  (rdHost, wrChild) <- createPipe
  (rdChild, wrHost) <- createPipe
  pid <- forkProcess $ do
    setHandler sigINT Nothing
    setHandler sigTERM Nothing
    setHandler sigHUP Nothing
    closeFd rdHost
    closeFd wrHost
    pid <- getProcessID
    pgid <- createProcessGroupFor pid
    writeByteString wrChild $ word64ToByteString $ fromIntegral pgid
    logger <- _createLogger logConfig nodeId
    let allVerifiers = verifiers logger
    stateRef <-
      newIORef $
        ProcessState
          { knownMinimalCost = initialCost,
            wrChild,
            rdChild,
            logger,
            verifiers = allVerifiers
          }
    let loop :: (Solver handle) => handle -> ProcessStep conProg -> IO ()
        loop solver st = do
          nextStep <- case st of
            InitialStep -> initialStep processConfig stateRef
            SynthStep nextTrack ->
              synthStep solver nextTrack processConfig stateRef
            GeneralizationStep nextTrack prog ->
              generalizationStep config nextTrack processConfig stateRef prog
            TerminationStep -> error "Should not happen"
          unless (isTerminationStep nextStep) $ loop solver nextStep
    let NodeId nid = nodeId
    withSolver
      config
        { sbvConfig =
            (sbvConfig config)
              { transcript =
                  if transcriptSMT
                    then
                      Just $
                        logRootDir logConfig <> "/" <> show nid <> ".smt2"
                    else
                      Nothing
              }
        }
      $ \solver ->
        loop solver InitialStep
  closeFd rdChild
  closeFd wrChild
  pgidBs <- readByteString rdHost 8
  setFdOption rdHost NonBlockingRead True
  let pgid = fromIntegral $ byteStringToWord64 pgidBs
  return $ Process pid pgid rdHost wrHost

initialStep ::
  ProcessConfig
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
  IORef (ProcessState sketch conProg) ->
  IO (ProcessStep conProg)
initialStep ProcessConfig {..} stateRef = do
  logger <- _readLogger stateRef

  case countNumProgsEvidence of
    Nothing ->
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep
            [ "Start synthesizing with sketch: ",
              pformat sketchSpec,
              ", entry symbol:",
              pformat sketchSymbol
            ]
    Just evidence -> do
      let numOfChoices = countNumChoicesWithEvidence evidence sketchSpec
      let numOfProgs = countNumProgsWithEvidence evidence sketchSpec
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep
            [ "Start synthesizing with sketch: ",
              pformat sketchSpec,
              ", entry symbol:",
              pformat sketchSymbol,
              "number of choices: ",
              pformat numOfChoices,
              "number of well-typed progs: ",
              pformat numOfProgs
            ]
  return $ SynthStep 0

_currentNewMinimalCostMessage :: IORef (ProcessState sketch conProg) -> IO NewMinimalCostMessage
_currentNewMinimalCostMessage stateRef = do
  ProcessState {..} <- readIORef stateRef
  return $ NewMinimalCostMessage knownMinimalCost

synthStep ::
  forall
    handle
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher.
  (Solver handle) =>
  handle ->
  Int ->
  ProcessConfig
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
  IORef (ProcessState sketch conProg) ->
  IO (ProcessStep conProg)
synthStep handle curTrack processConfig@ProcessConfig {..} stateRef = do
  logger <- _readLogger stateRef
  let allVerifiers = verifiers logger
  minimalCostMessage@NewMinimalCostMessage {..} <-
    _currentNewMinimalCostMessage stateRef
  logMultiLineDoc logger NOTICE $
    "Synth with minimal cost: "
      <> pformat newMinimalCost
      <> ", track: "
      <> pformat curTrack
  (synthedCost, examples, result) <-
    _solverRunSynthRequest
      handle
      0
      curTrack
      processConfig
      minimalCostMessage
      stateRef
  case result of
    SynthesisSuccess prog -> do
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep
            [ "Track "
                <> pformat curTrack
                <> " synth success: ",
              pformat prog
            ]
      cost <- _readKnownMinimalCost stateRef
      let hasGeneralization = isJust generalizationSketchFromFastResult
      let hasNextTrack = length allVerifiers > curTrack + 1
      if hasNextTrack
        then do
          _sendAndWaitForNewMinimalCost stateRef $
            Viable curTrack examples cost prog
          if hasGeneralization
            then return $ GeneralizationStep curTrack prog
            else return $ SynthStep (curTrack + 1)
        else do
          _updateKnownMinimalCost stateRef (Just synthedCost)
          _sendAndWaitForNewMinimalCost stateRef $
            Success False curTrack examples synthedCost prog
          return $ SynthStep curTrack
    SynthesisSolverFailure failure -> do
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep
            [ "Track " <> pformat curTrack <> " synth failure: ",
              pformat failure
            ]
      wrChild <- _readWrChild stateRef
      writeObject
        wrChild
        ( Failure examples failure ::
            Message conProg symSemObj symVal conSemObj conVal matcher
        )
      return TerminationStep
    SynthesisVerifierFailure err ->
      error $
        T.unpack $
          "Verification crashed, please check the code, reason: " <> err

generalizationStep ::
  forall
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher.
  GrisetteSMTConfig ->
  Int ->
  ProcessConfig sketchSpec sketch conProg costObj cost symSemObj symVal conSemObj conVal matcher ->
  IORef (ProcessState sketch conProg) ->
  SymbolTable conProg ->
  IO (ProcessStep conProg)
generalizationStep
  config
  curTrack
  processConfig@ProcessConfig {..}
  stateRef
  conProg = do
    unless (isJust generalizationSketchFromFastResult) $ error "Should not happen"
    let sketchSpec = fromJust generalizationSketchFromFastResult conProg
    minimalCostMessage@NewMinimalCostMessage {..} <-
      _currentNewMinimalCostMessage stateRef
    logger <- _readLogger stateRef
    logMultiLineDoc logger NOTICE $
      nest 2 $
        vsep
          [ "Generalization with minimal cost: " <> pformat newMinimalCost,
            "track: " <> pformat curTrack,
            "sketch: " <> pformat sketchSpec,
            "sketch symbol: " <> pformat sketchSymbol
          ]
    r <- newEmptyMVar
    trackBound <- _readTrackBound stateRef
    a <- async $ do
      res <- withSolver config $ \localHandle ->
        _solverRunSynthRequest
          localHandle
          trackBound
          trackBound
          ( processConfig {sketchSpec = sketchSpec} ::
              ProcessConfig
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
          minimalCostMessage
          stateRef
      putMVar r $ Just res
    _ <- async $ do
      threadDelay (generalizationTimeout * 1000000)
      cancelWith a ThreadKilled
      tryPutMVar r Nothing
    generalizationResult <- takeMVar r
    case generalizationResult of
      Just (cost, examples, SynthesisSuccess r) -> do
        logMultiLineDoc logger NOTICE $
          nest 2 $
            vsep
              [ "Generalization success: ",
                pformat r,
                "cost: " <> pformat cost,
                "track: " <> pformat curTrack
              ]
        _updateKnownMinimalCost stateRef (Just cost)
        _sendAndWaitForNewMinimalCost stateRef $
          Success True curTrack examples cost r
        return $ SynthStep curTrack
      Just (_, examples, _) -> do
        logMultiLineDoc
          logger
          NOTICE
          ( "Generalization failed, proceed to next track (track "
              <> pformat (curTrack + 1)
              <> ")."
          )
        cost <- _readKnownMinimalCost stateRef
        _sendAndWaitForNewMinimalCost
          stateRef
          ( GeneralizationFailure curTrack cost examples ::
              Message conProg symSemObj symVal conSemObj conVal matcher
          )
        return $ SynthStep (curTrack + 1)
      _ -> do
        logMultiLineDoc
          logger
          NOTICE
          ( "Generalization timed out or crashed, proceed to next track (track "
              <> pformat (curTrack + 1)
              <> ")."
          )
        cost <- _readKnownMinimalCost stateRef
        _sendAndWaitForNewMinimalCost
          stateRef
          ( GeneralizationFailure curTrack cost [] ::
              Message conProg symSemObj symVal conSemObj conVal matcher
          )
        return $ SynthStep (curTrack + 1)

closeProcessPipes :: Process -> IO ()
closeProcessPipes Process {..} = do
  closeFd pipeRd
  closeFd pipeWr

_readProcessResponse ::
  ( Serial conProg,
    Serial symSemObj,
    Serial conSemObj,
    Serial conVal,
    Serial matcher
  ) =>
  Process ->
  ProcessStatus ->
  IO (Either T.Text (Message conProg symSemObj symVal conSemObj conVal matcher))
_readProcessResponse process@Process {..} (Exited ExitSuccess) = do
  msg <- readObject pipeRd
  closeProcessPipes process
  return $ Right msg
_readProcessResponse process@Process {..} (Exited (ExitFailure e)) = do
  closeProcessPipes process
  return $ Left $ "Process exited with error code " <> T.pack (show e)
_readProcessResponse process@Process {..} (Terminated signal dumped) = do
  closeProcessPipes process
  return $
    Left $
      "Process terminated by signal "
        <> T.pack (show signal)
        <> (if dumped then " (core dumped)" else "")
_readProcessResponse _ (Stopped {}) = error "Should not happen"

type ProcessResponse conProg symSemObj symVal conSemObj conVal matcher =
  Either T.Text (Message conProg symSemObj symVal conSemObj conVal matcher)

responseType :: ProcessResponse conProg symSemObj symVal conSemObj conVal matcher -> Either T.Text MessageType
responseType = fmap messageType

processResponseNewCost ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher -> Maybe Int
processResponseNewCost (Right (Success _ _ _ cost _)) = Just cost
processResponseNewCost _ = Nothing

getProcessResponse ::
  ( Serial conProg,
    Serial symSemObj,
    Serial conSemObj,
    Serial conVal,
    Serial matcher
  ) =>
  Bool ->
  Process ->
  IO
    ( Maybe
        (ProcessResponse conProg symSemObj symVal conSemObj conVal matcher)
    )
getProcessResponse blk process@Process {..} = do
  r <- getProcessStatus blk False pid
  case r of
    Nothing -> do
      when blk $ error "Should not happen"
      msg <- nonBlockingReadObject pipeRd
      case msg of
        Nothing -> return Nothing
        Just msg@(Failure _ _) -> do
          _ <- getProcessStatus True False pid
          closeProcessPipes process
          return $ Just $ Right msg
        Just msg -> return $ Just $ Right msg
    Just status -> Just <$> _readProcessResponse process status

sendNewMinimalCost :: Maybe Int -> Process -> IO ()
sendNewMinimalCost newMinimalCost Process {..} = do
  writeObject pipeWr $ NewMinimalCostMessage newMinimalCost
