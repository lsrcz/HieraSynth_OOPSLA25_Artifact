{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -ddump-splices -ddump-to-file -ddump-file-prefix=synthesis #-}

module HieraSynth.Reasoning.Synthesis
  ( SynthesisContext (..),
    SynthesisResult (..),
    IsVerifier (..),
    SomeVerifier (..),
    Example (..),
    SymExampleConstraint,
    ConExampleConstraint,
    SomeExample (..),
    synthesisConstraintFun,
    RunSynthesisTask (..),
    solverRunSynthesisTask,
    runSynthesisTask,
    runSynthesisTaskExtractCex,
    SynthesisTask (..),
    SynthesisBoundCostTask (..),
    SynthesisMinimalCostTask (..),
  )
where

import Control.DeepSeq (NFData, NFData1)
import Control.Monad.Except (runExceptT)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize), Serial1)
import Data.Data (Typeable)
import Data.Functor.Classes (Show1)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.Serialize as Cereal
import qualified Data.Text as T
import Data.Typeable (Proxy (Proxy), cast)
import GHC.Generics (Generic)
import Grisette
  ( CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    ConfigurableSolver,
    Default (Default),
    DeriveConfig (unconstrainedPositions),
    EvalSym,
    LogicalOp ((.&&)),
    Mergeable,
    PPrint (pformat),
    Solvable (con),
    Solver,
    SolvingFailure,
    SymBool,
    SymOrd ((.<)),
    SynthesisConstraintFun,
    ToCon,
    ToSym (toSym),
    VerifierFun,
    derive,
    deriveWith,
    evalSymToCon,
    runFreshT,
    simpleMerge,
    solverGenericCEGIS,
    solverGenericCEGISWithRefinement,
    symAnd,
    uniqueIdentifier,
    withSolver,
  )
import HieraSynth.Context
  ( AngelicContext,
    ConcreteContext,
    SymbolicContext,
  )
import HieraSynth.Program.Concrete.Program (ProgPPrint)
import HieraSynth.Program.ProgCost (ProgCost, symbolCost)
import HieraSynth.Program.ProgSemantics (ProgSemantics, runSymbol)
import HieraSynth.Program.ProgTyping (ProgTyping)
import HieraSynth.Program.ProgUtil (ProgUtil)
import HieraSynth.Program.SymbolTable
  ( ProgReachableSymbols,
    SymbolTable,
    filterByReachableSymbols,
  )
import HieraSynth.Reasoning.IOPair
  ( IOPair (ioPairInputs, ioPairOutputs),
  )
import HieraSynth.Reasoning.Matcher (Matcher (match))

class SynthesisContext ctx where
  genSynthesisConstraint ::
    (Matcher matcher SymBool val, Mergeable val) =>
    matcher ->
    ctx [val] ->
    [val] ->
    IO SymBool

instance SynthesisContext SymbolicContext where
  genSynthesisConstraint matcher actual expectedOutputs =
    return $ simpleMerge $ do
      actualVal <- runExceptT actual
      case actualVal of
        Left _ -> return $ con False
        Right actualOutputs ->
          return $ match matcher actualOutputs expectedOutputs

instance SynthesisContext AngelicContext where
  genSynthesisConstraint matcher actual expectedOutputs = do
    ident <- uniqueIdentifier "synth"
    genSynthesisConstraint matcher (runFreshT actual ident) expectedOutputs

data Example symSemObj symVal conSemObj conVal matcher where
  Example ::
    { exampleConSemantics :: conSemObj,
      exampleSymSemantics :: symSemObj,
      exampleSymValType :: Proxy symVal,
      exampleIOPair :: IOPair conVal,
      exampleMatcher :: matcher
    } ->
    Example symSemObj symVal conSemObj conVal matcher
  deriving (Generic)

instance
  (Serial conSemObj, Serial symSemObj, Serial conVal, Serial matcher) =>
  Serial (Example symSemObj symVal conSemObj conVal matcher)
  where
  serialize (Example conSem symSem _ iop matcher) = do
    serialize conSem
    serialize symSem
    serialize iop
    serialize matcher
  deserialize = do
    conSem <- deserialize
    symSem <- deserialize
    iop <- deserialize
    Example conSem symSem Proxy iop <$> deserialize

deriveWith
  (mempty {unconstrainedPositions = [1]})
  [''Example]
  [ ''Cereal.Serialize,
    ''Binary.Binary,
    ''Eq,
    ''NFData,
    ''Hashable
  ]

instance
  (Show conVal) =>
  Show (Example symSemObj symVal conSemObj conVal matcher)
  where
  show (Example _ _ _ iop _) = show iop

instance
  (PPrint conVal) =>
  PPrint (Example symSemObj symVal conSemObj conVal matcher)
  where
  pformat (Example _ _ _ iop _) = pformat iop

type SymExampleConstraint symSemObj symProg symVal matcher =
  ( ProgSemantics symSemObj symProg symVal AngelicContext,
    ProgUtil symProg,
    Matcher matcher SymBool symVal,
    Mergeable symVal,
    Eq symSemObj,
    Eq matcher,
    Typeable symSemObj,
    Typeable symProg,
    Typeable symVal,
    Typeable matcher,
    NFData symSemObj,
    NFData matcher
  )

type ConExampleConstraint conSemObj conProg conVal matcher =
  ( ProgSemantics conSemObj conProg conVal ConcreteContext,
    ProgUtil conProg,
    Matcher matcher Bool conVal,
    Mergeable conVal,
    Eq conSemObj,
    Eq matcher,
    Eq conVal,
    Mergeable conVal,
    Show conVal,
    PPrint conVal,
    Typeable conSemObj,
    Typeable conProg,
    Typeable conVal,
    Typeable matcher,
    NFData conSemObj,
    NFData matcher,
    NFData conVal
  )

data SomeExample symProg conProg where
  SomeExample ::
    forall symSemObj symProg symVal conSemObj conProg conVal matcher.
    ( SymExampleConstraint symSemObj symProg symVal matcher,
      ConExampleConstraint conSemObj conProg conVal matcher,
      ToSym conVal symVal
    ) =>
    Example symSemObj symVal conSemObj conVal matcher ->
    SomeExample symProg conProg

deriveWith
  (mempty {unconstrainedPositions = [0, 1]})
  [''SomeExample]
  [''Show, ''NFData]

instance PPrint (SomeExample symProg conProg) where
  pformat (SomeExample ex) = pformat ex

eqHetero :: (Typeable a, Typeable b, Eq a) => a -> b -> Bool
eqHetero a b = case cast b of
  Just b' -> a == b'
  Nothing -> False

instance Eq (SomeExample symProg conProg) where
  (SomeExample ex1) == (SomeExample ex2) =
    eqHetero ex1 ex2

class
  IsVerifier verifier symProg conProg
    | verifier -> symProg conProg
  where
  toVerifierFuns ::
    verifier ->
    SymbolTable symProg ->
    T.Text ->
    [VerifierFun (SomeExample symProg conProg) T.Text]

data SomeVerifier symProg conProg where
  SomeVerifier ::
    forall verifier symProg conProg.
    (IsVerifier verifier symProg conProg) =>
    verifier ->
    SomeVerifier symProg conProg

data SynthesisTask symProg conProg where
  SynthesisTask ::
    forall symProg conProg.
    ( EvalSym symProg,
      ToCon symProg conProg,
      Typeable symProg,
      ProgReachableSymbols conProg
    ) =>
    { synthesisVerifiers :: [SomeVerifier symProg conProg],
      synthesisInitialExamples :: [SomeExample symProg conProg],
      synthesisSketchTable :: SymbolTable symProg,
      synthesisSketchSymbol :: T.Text,
      synthesisPrecondition :: SymBool,
      synthesisExtraConstraints ::
        SynthesisConstraintFun (SomeExample symProg conProg)
    } ->
    SynthesisTask symProg conProg

data SynthesisBoundCostTask symProg conProg where
  SynthesisBoundCostTask ::
    forall symProg conProg cost symCostObj.
    ( EvalSym symProg,
      ToCon symProg conProg,
      ProgReachableSymbols conProg,
      ProgCost symCostObj symProg cost AngelicContext,
      SymOrd cost,
      Mergeable cost
    ) =>
    { synthesisVerifiers :: [SomeVerifier symProg conProg],
      synthesisInitialExamples :: [SomeExample symProg conProg],
      synthesisSketchTable :: SymbolTable symProg,
      synthesisSketchSymbol :: T.Text,
      synthesisPrecondition :: SymBool,
      synthesisInitialMaxCost :: Maybe cost,
      synthesisSymCostObj :: symCostObj,
      synthesisExtraConstraints ::
        SynthesisConstraintFun (SomeExample symProg conProg)
    } ->
    SynthesisBoundCostTask symProg conProg

data SynthesisMinimalCostTask symProg conProg where
  SynthesisMinimalCostTask ::
    forall symProg conProg cost conCostObj symCostObj.
    ( EvalSym symProg,
      ToCon symProg conProg,
      ProgReachableSymbols conProg,
      ProgCost conCostObj conProg cost ConcreteContext,
      ProgCost symCostObj symProg cost AngelicContext,
      SymOrd cost,
      Mergeable cost
    ) =>
    { synthesisVerifiers :: [SomeVerifier symProg conProg],
      synthesisInitialExamples :: [SomeExample symProg conProg],
      synthesisSketchTable :: SymbolTable symProg,
      synthesisSketchSymbol :: T.Text,
      synthesisPrecondition :: SymBool,
      synthesisInitialMaxCost :: Maybe cost,
      synthesisConCostObj :: conCostObj,
      synthesisSymCostObj :: symCostObj,
      synthesisExtraConstraints ::
        SynthesisConstraintFun (SomeExample symProg conProg)
    } ->
    SynthesisMinimalCostTask symProg conProg

synthesisConstraintFun ::
  forall symProg conProg.
  (Typeable symProg) =>
  SymbolTable symProg ->
  T.Text ->
  SynthesisConstraintFun (SomeExample symProg conProg)
synthesisConstraintFun
  table
  symbol
  ( SomeExample
      ( Example _ symSem _ (iop :: IOPair conVal) matcher ::
          Example symSemObj symVal conSemObj conVal matcher
        )
    ) =
    genSynthesisConstraint
      matcher
      ( runSymbol symSem table symbol (toSym $ ioPairInputs iop :: [symVal]) ::
          AngelicContext [symVal]
      )
      (toSym $ ioPairOutputs iop :: [symVal])

data SynthesisResult conProg
  = SynthesisSuccess (SymbolTable conProg)
  | SynthesisVerifierFailure T.Text
  | SynthesisSolverFailure SolvingFailure
  deriving (Generic)

derive
  [''SynthesisResult]
  [ ''Show,
    ''Show1,
    ''Serial,
    ''Serial1,
    ''NFData,
    ''NFData1,
    ''Cereal.Serialize,
    ''Binary.Binary
  ]

deriving via
  (Default (SynthesisResult conProg))
  instance
    (ProgPPrint conProg, ProgUtil conProg, ProgTyping conProg) =>
    (PPrint (SynthesisResult conProg))

class RunSynthesisTask task symProg conProg | task -> symProg conProg where
  solverRunSynthesisTaskExtractCex ::
    forall solver.
    (Solver solver) =>
    solver ->
    task ->
    IO ([SomeExample symProg conProg], SynthesisResult conProg)
  taskRefinable :: task -> Bool

instance
  (Typeable symProg) =>
  RunSynthesisTask (SynthesisBoundCostTask symProg conProg) symProg conProg
  where
  solverRunSynthesisTaskExtractCex
    solver
    ( SynthesisBoundCostTask
        verifiers
        examples
        table
        symbol
        precond
        (initialMaxCost :: Maybe cost)
        symCostObj
        extraConstraints
      ) = do
      initialExampleConstraints <-
        traverse (synthesisConstraintFun table symbol) examples
      let costConstraint = symProgCostLessThanMaxCost initialMaxCost
      (cex, r) <-
        solverGenericCEGIS
          solver
          True
          (precond .&& symAnd initialExampleConstraints .&& costConstraint)
          ( \cex -> do
              s <- synthesisConstraintFun table symbol cex
              e <- extraConstraints cex
              return $ s .&& e
          )
          ( concatMap
              (\(SomeVerifier verifier) -> toVerifierFuns verifier table symbol)
              verifiers
          )
      let allCexes = examples ++ cex
      case r of
        CEGISSuccess model ->
          return
            ( allCexes,
              SynthesisSuccess $
                case filterByReachableSymbols (HS.singleton symbol) $
                  evalSymToCon model table of
                  Left err ->
                    error $ "BUG: Failed to filter symbols: " <> T.unpack err
                  Right v -> v
            )
        CEGISVerifierFailure err ->
          return (allCexes, SynthesisVerifierFailure err)
        CEGISSolverFailure failure ->
          return (allCexes, SynthesisSolverFailure failure)
      where
        symProgCost =
          flip runFreshT "cost" $ symbolCost symCostObj table symbol ::
            SymbolicContext cost
        symProgCostLessThanMaxCost :: Maybe cost -> SymBool
        symProgCostLessThanMaxCost Nothing = con True
        symProgCostLessThanMaxCost (Just maxCost) = simpleMerge $ do
          eitherCost <- runExceptT symProgCost
          case eitherCost of
            Left _ -> return $ con False
            Right cost -> return $ cost .< maxCost
  taskRefinable _ = True

instance
  (Typeable symProg) =>
  RunSynthesisTask (SynthesisMinimalCostTask symProg conProg) symProg conProg
  where
  solverRunSynthesisTaskExtractCex
    solver
    ( SynthesisMinimalCostTask
        verifiers
        examples
        table
        symbol
        precond
        (initialMaxCost :: Maybe cost)
        conCostObj
        symCostObj
        extraConstraints
      ) = do
      initialExampleConstraints <-
        traverse (synthesisConstraintFun table symbol) examples
      let costConstraint = case initialMaxCost of
            Nothing -> con True
            Just maxCost -> symProgCostLessThanMaxCost maxCost
      (cex, r) <-
        solverGenericCEGISWithRefinement
          solver
          True
          (precond .&& symAnd initialExampleConstraints .&& costConstraint)
          ( \cex -> do
              s <- synthesisConstraintFun table symbol cex
              e <- extraConstraints cex
              return $ s .&& e
          )
          (Just refineFun)
          ( concatMap
              (\(SomeVerifier verifier) -> toVerifierFuns verifier table symbol)
              verifiers
          )
      let allCexes = examples ++ cex
      case r of
        CEGISSuccess model ->
          return
            ( allCexes,
              SynthesisSuccess $
                case filterByReachableSymbols (HS.singleton symbol) $
                  evalSymToCon model table of
                  Left err ->
                    error $ "BUG: Failed to filter symbols: " <> T.unpack err
                  Right v -> v
            )
        CEGISVerifierFailure err ->
          return (allCexes, SynthesisVerifierFailure err)
        CEGISSolverFailure failure ->
          return (allCexes, SynthesisSolverFailure failure)
      where
        symProgCost =
          flip runFreshT "cost" $ symbolCost symCostObj table symbol ::
            SymbolicContext cost
        symProgCostLessThanMaxCost :: cost -> SymBool
        symProgCostLessThanMaxCost maxCost = simpleMerge $ do
          eitherCost <- runExceptT symProgCost
          case eitherCost of
            Left _ -> return $ con False
            Right cost -> return $ cost .< maxCost
        refineFun model = do
          let conTable = evalSymToCon model table :: SymbolTable conProg
          let conCost =
                symbolCost conCostObj conTable symbol :: ConcreteContext cost
          case conCost of
            Left _ -> return $ con False
            Right cost -> return $ symProgCostLessThanMaxCost cost
  taskRefinable _ = False

instance
  (Typeable symProg) =>
  RunSynthesisTask (SynthesisTask symProg conProg) symProg conProg
  where
  solverRunSynthesisTaskExtractCex
    solver
    (SynthesisTask verifiers examples table symbol precond extraConstraints) = do
      initialExampleConstraints <-
        traverse (synthesisConstraintFun table symbol) examples
      (cex, r) <-
        solverGenericCEGIS
          solver
          True
          (precond .&& symAnd initialExampleConstraints)
          ( \cex -> do
              s <- synthesisConstraintFun table symbol cex
              e <- extraConstraints cex
              return $ s .&& e
          )
          ( concatMap
              (\(SomeVerifier verifier) -> toVerifierFuns verifier table symbol)
              verifiers
          )
      let allCexes = examples ++ cex
      case r of
        CEGISSuccess model ->
          return
            ( allCexes,
              SynthesisSuccess $
                case filterByReachableSymbols (HS.singleton symbol) $
                  evalSymToCon model table of
                  Left err ->
                    error $ "BUG: Failed to filter symbols: " <> T.unpack err
                  Right v -> v
            )
        CEGISVerifierFailure err ->
          return (allCexes, SynthesisVerifierFailure err)
        CEGISSolverFailure failure ->
          return (allCexes, SynthesisSolverFailure failure)
  taskRefinable _ = True

solverRunSynthesisTask ::
  ( Solver solver,
    RunSynthesisTask task symProg conProg
  ) =>
  solver ->
  task ->
  IO (SynthesisResult conProg)
solverRunSynthesisTask solver task =
  snd <$> solverRunSynthesisTaskExtractCex solver task

runSynthesisTask ::
  ( ConfigurableSolver config h,
    RunSynthesisTask task symProg conProg
  ) =>
  config ->
  task ->
  IO (SynthesisResult conProg)
runSynthesisTask config task =
  snd <$> runSynthesisTaskExtractCex config task

runSynthesisTaskExtractCex ::
  ( ConfigurableSolver config h,
    RunSynthesisTask task symProg conProg
  ) =>
  config ->
  task ->
  IO ([SomeExample symProg conProg], SynthesisResult conProg)
runSynthesisTaskExtractCex config task =
  withSolver config $ \solver ->
    solverRunSynthesisTaskExtractCex solver task
