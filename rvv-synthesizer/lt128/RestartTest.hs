{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module RestartTest
  ( runTaskAndRefine,
    testRestart,
    Variant (..),
  )
where

import Control.Arrow (Arrow (first))
import Control.Monad.Except (runExceptT)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Grisette
  ( Solvable (con),
    Solver (solverAssert),
    SymBool,
    SymOrd ((.<)),
    SymWordN,
    ToSym (toSym),
    WordN,
    bitwuzla,
    runFreshT,
    simpleMerge,
    withSolver,
  )
import HieraSynth.Context
  ( AngelicContext,
    ConcreteContext,
    SymbolicContext,
  )
import HieraSynth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (PerStmtCostObj),
  )
import HieraSynth.Program.ProgCost (ProgCost, symbolCost)
import HieraSynth.Program.SymbolTable (SymbolTable)
import HieraSynth.Reasoning.Synthesis
  ( SomeVerifier,
    SynthesisResult
      ( SynthesisSolverFailure,
        SynthesisSuccess,
        SynthesisVerifierFailure
      ),
    SynthesisTask (SynthesisTask),
    runSynthesisTask,
    runSynthesisTaskExtractCex,
    solverRunSynthesisTask,
    solverRunSynthesisTaskExtractCex,
    synthesisConstraintFun,
  )
import HieraSynth.Util.Logging (logMultiLine)
import RVV.Synthesizer.CostModel.CostModel (CostModel)
import RVV.Synthesizer.CostModel.X280ApproxModel (x280ApproxModel)
import RVV.Synthesizer.Op (ConProg, SymProg)
import System.Log.Logger
  ( Logger,
    Priority (NOTICE),
  )

symProgCost ::
  (ProgCost (PerStmtCostObj CostModel) prog (SymWordN 8) AngelicContext) =>
  SymbolTable prog ->
  T.Text ->
  SymbolicContext (SymWordN 8)
symProgCost table key =
  flip runFreshT "cost" $
    symbolCost (PerStmtCostObj (x280ApproxModel False)) table key ::
    SymbolicContext (SymWordN 8)

symProgCostLessThanMaxCost ::
  (ProgCost (PerStmtCostObj CostModel) prog (SymWordN 8) AngelicContext) =>
  SymbolTable prog ->
  T.Text ->
  SymWordN 8 ->
  SymBool
symProgCostLessThanMaxCost table key maxCost = simpleMerge $ do
  eitherCost <- runExceptT $ symProgCost table key
  case eitherCost of
    Left _ -> return $ con False
    Right cost -> return $ cost .< maxCost

runTaskAndRefine ::
  [ SomeVerifier
      (SymProg (WordN 8) (SymWordN 8))
      (ConProg (WordN 8))
  ] ->
  SymbolTable (SymProg (WordN 8) (SymWordN 8)) ->
  T.Text ->
  Logger ->
  IO ()
runTaskAndRefine verifiers table key logger =
  withSolver bitwuzla $ \solver -> do
    let task = SynthesisTask verifiers [] table key (con True) (const $ return $ con True)
    logMultiLine logger NOTICE "Run task and refine"
    let refine r =
          case r of
            SynthesisVerifierFailure err ->
              logMultiLine logger NOTICE $
                "Finished, verifier failure: " <> T.unpack err
            SynthesisSolverFailure f ->
              logMultiLine logger NOTICE $ "Finished, solver failure" <> show f
            SynthesisSuccess conTable -> do
              let Right conCost =
                    symbolCost (PerStmtCostObj (x280ApproxModel False)) conTable key ::
                      ConcreteContext (WordN 8)
              logMultiLine logger NOTICE $
                "Got a program with cost " <> show conCost
              solverAssert solver $
                symProgCostLessThanMaxCost table key (toSym conCost)
              r' <- solverRunSynthesisTask solver task
              refine r'
    r <- solverRunSynthesisTask solver task
    refine r

runTaskAndRestart ::
  [ SomeVerifier
      (SymProg (WordN 8) (SymWordN 8))
      (ConProg (WordN 8))
  ] ->
  SymbolTable (SymProg (WordN 8) (SymWordN 8)) ->
  T.Text ->
  Logger ->
  IO ()
runTaskAndRestart verifiers table key logger = do
  let task = SynthesisTask verifiers [] table key (con True) (const $ return $ con True)
  logMultiLine logger NOTICE "Run task and restart"
  let refine r =
        case r of
          SynthesisVerifierFailure err ->
            logMultiLine logger NOTICE $
              "Finished, verifier failure: " <> T.unpack err
          SynthesisSolverFailure f ->
            logMultiLine logger NOTICE $ "Finished, solver failure" <> show f
          SynthesisSuccess conTable -> do
            let Right conCost =
                  symbolCost (PerStmtCostObj (x280ApproxModel False)) conTable key ::
                    ConcreteContext (WordN 8)
            logMultiLine logger NOTICE $
              "Got a program with cost " <> show conCost
            r' <- withSolver bitwuzla $ \solver -> do
              solverAssert solver $
                symProgCostLessThanMaxCost table key (toSym conCost)
              solverRunSynthesisTask solver task
            refine r'
  r <- runSynthesisTask bitwuzla task
  refine r

runTaskAndRestartWithCex ::
  [ SomeVerifier
      (SymProg (WordN 8) (SymWordN 8))
      (ConProg (WordN 8))
  ] ->
  SymbolTable (SymProg (WordN 8) (SymWordN 8)) ->
  T.Text ->
  Logger ->
  IO ()
runTaskAndRestartWithCex verifiers table key logger = do
  let task = SynthesisTask verifiers [] table key (con True) (const $ return $ con True)
  logMultiLine logger NOTICE "Run task and restart with cex"
  let refine r = do
        -- logMultiLine logger NOTICE $ "Cexes: " <> show (fst r)
        case r of
          (_, SynthesisVerifierFailure err) ->
            logMultiLine logger NOTICE $
              "Finished, verifier failure: " <> T.unpack err
          (_, SynthesisSolverFailure f) ->
            logMultiLine logger NOTICE $ "Finished, solver failure" <> show f
          (inputs, SynthesisSuccess conTable) -> do
            let Right conCost =
                  symbolCost (PerStmtCostObj (x280ApproxModel False)) conTable key ::
                    ConcreteContext (WordN 8)
            logMultiLine logger NOTICE $
              "Got a program with cost "
                <> show conCost
                <> ", total num of cex: "
                <> show (length inputs)
            r' <- withSolver bitwuzla $ \solver -> do
              fs <- traverse (synthesisConstraintFun table key) inputs
              traverse_ (solverAssert solver) fs
              solverAssert solver $
                symProgCostLessThanMaxCost table key (toSym conCost)
              solverRunSynthesisTaskExtractCex solver task
            refine $ first (inputs ++) r'
  r <- runSynthesisTaskExtractCex bitwuzla task
  refine r

data Variant = RestartWithCex | Restart | Refine

testRestart ::
  Variant ->
  Logger ->
  [ SomeVerifier
      (SymProg (WordN 8) (SymWordN 8))
      (ConProg (WordN 8))
  ] ->
  SymbolTable (SymProg (WordN 8) (SymWordN 8)) ->
  T.Text ->
  IO ()
testRestart variant logger verifiers table key =
  case variant of
    Refine -> runTaskAndRefine verifiers table key logger
    Restart -> runTaskAndRestart verifiers table key logger
    RestartWithCex -> runTaskAndRestartWithCex verifiers table key logger
