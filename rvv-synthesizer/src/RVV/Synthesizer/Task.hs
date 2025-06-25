{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Task
  ( RVVQuickCheckFuzzer (..),
    RVVTestSuite (..),
    RVVSMTVerifier (..),
    RVVCombinedVerifierFuzzer (..),
    CexReductionBehavior (..),
  )
where

import Data.Bits
  ( Bits (complement, shiftL, (.&.), (.|.)),
    FiniteBits (countLeadingZeros, countTrailingZeros, finiteBitSize),
  )
import Data.Either (isLeft)
import qualified Data.Text as T
import Data.Type.Equality (type (:~:) (Refl))
import Data.Typeable (eqT)
import Grisette
  ( BV (bv),
    EvalSym,
    GrisetteSMTConfig,
    PPrint (pformat),
    ToCon,
    VerifierResult
      ( CEGISVerifierFoundCex,
        CEGISVerifierNoCex
      ),
    evalSymToCon,
    nest,
    vsep,
  )
import HieraSynth.Context
  ( AngelicContext,
    ConcreteContext,
    SymbolicContext,
  )
import HieraSynth.Program.Concrete (ProgPPrint)
import HieraSynth.Program.ProgSemantics (ProgSemantics, runSymbol)
import HieraSynth.Program.SymbolTable (SymbolTable)
import HieraSynth.Reasoning.Fuzzing
  ( QuickCheckFuzzer
      ( QuickCheckFuzzer,
        quickCheckFuzzerConSemantics,
        quickCheckFuzzerGenerators,
        quickCheckFuzzerMaxTests,
        quickCheckFuzzerSpec,
        quickCheckFuzzerSymSemantics
      ),
  )
import HieraSynth.Reasoning.IOPair
  ( IOPair (IOPair, ioPairInputs, ioPairOutputs),
  )
import HieraSynth.Reasoning.Matcher (Matcher (match))
import HieraSynth.Reasoning.Synthesis
  ( ConExampleConstraint,
    Example (Example, exampleConSemantics, exampleIOPair, exampleSymSemantics),
    IsVerifier (toVerifierFuns),
    SomeExample (SomeExample),
    SymExampleConstraint,
  )
import HieraSynth.Reasoning.Verification
  ( SMTVerifier
      ( SMTVerifier,
        smtVerifierConSemantics,
        smtVerifierInputs,
        smtVerifierSolverConfig,
        smtVerifierSpec,
        smtVerifierSymSemantics,
        smtVerifierTimeoutSeconds
      ),
  )
import HieraSynth.Util.Logging (logMultiLineDoc)
import Grisette.Unified (EvalModeTag (C, S), GetSomeWordN)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    Vector (Vector),
    VectorReg (VectorReg),
    concatData,
    concatUninitialized,
    fromConcatenated,
  )
import RVV.Synthesizer.Matcher (RegListMatcher)
import RVV.Synthesizer.Value
  ( Value (MaskValue, MemValue, PtrValue, ScalarValue, VLValue, VectorValue),
  )
import System.Log.Logger (Logger, Priority (NOTICE))
import Test.QuickCheck (Gen)

data RVVTestSuite symProg conProg where
  RVVTestSuite ::
    ( SymExampleConstraint MachineConfig symProg (Value 'S) RegListMatcher,
      ConExampleConstraint MachineConfig conProg (Value 'C) RegListMatcher
    ) =>
    { rvvTestSuite ::
        [Example MachineConfig (Value 'S) MachineConfig (Value 'C) RegListMatcher]
    } ->
    RVVTestSuite symProg conProg

instance
  ( EvalSym symProg,
    ToCon symProg conProg
  ) =>
  IsVerifier (RVVTestSuite symProg conProg) symProg conProg
  where
  toVerifierFuns (RVVTestSuite {..}) table key =
    [ \model -> do
        let conTable = evalSymToCon model table :: SymbolTable conProg
        let go [] = return $ CEGISVerifierNoCex False
            go (e@Example {..} : xs) = do
              let inputs = ioPairInputs exampleIOPair
              let expectedOutputs = ioPairOutputs exampleIOPair
              let actualOutputs =
                    runSymbol exampleConSemantics conTable key inputs
              if Right expectedOutputs == actualOutputs
                then go xs
                else return $ CEGISVerifierFoundCex $ SomeExample e
        go rvvTestSuite
    ]

-- Type to specify behavior for counter-example reduction
data CexReductionBehavior = ZeroOut | SetToOne deriving (Show, Eq)

data RVVCombinedVerifierFuzzer symProg conProg where
  RVVCombinedVerifierFuzzer ::
    ( SymExampleConstraint MachineConfig symProg (Value 'S) RegListMatcher,
      ConExampleConstraint MachineConfig conProg (Value 'C) RegListMatcher,
      ProgSemantics MachineConfig conProg (Value 'S) SymbolicContext
    ) =>
    { rvvCombinedVerifierFuzzerSolverConfig :: GrisetteSMTConfig,
      rvvCombinedVerifierFuzzerFastVerifierTimeoutSeconds :: Double,
      rvvCombinedVerifierFuzzerSlowVerifierTimeoutSeconds :: Double,
      rvvCombinedVerifierFuzzerMachineConfigs :: [MachineConfig],
      rvvCombinedVerifierFuzzerVerifierInputs ::
        [Maybe Int -> MachineConfig -> AngelicContext [Value 'S]],
      rvvCombinedVerifierFuzzerVerifierSpec ::
        MachineConfig ->
        [Value 'S] ->
        SymbolicContext ([Value 'S], RegListMatcher),
      rvvCombinedVerifierFuzzerFuzzerMaxTests :: Int,
      rvvCombinedVerifierFuzzerFuzzerGenerators :: [MachineConfig -> Gen [Value 'C]],
      rvvCombinedVerifierFuzzerFuzzerSpec ::
        MachineConfig ->
        [Value 'C] ->
        ConcreteContext ([Value 'C], RegListMatcher),
      rvvCombinedVerifierFuzzerLogger :: Maybe Logger,
      rvvCombinedVerifierFuzzerCexReductionBehavior :: CexReductionBehavior -- Specifies how counter-example bits are reduced
    } ->
    RVVCombinedVerifierFuzzer symProg conProg

instance
  ( EvalSym symProg,
    ToCon symProg conProg,
    ProgPPrint conProg
  ) =>
  IsVerifier (RVVCombinedVerifierFuzzer symProg conProg) symProg conProg
  where
  toVerifierFuns (RVVCombinedVerifierFuzzer {..}) table key = do
    vconst <- rvvCombinedVerifierFuzzerMachineConfigs
    let verifier ::
          Maybe Int -> Maybe Double -> SMTVerifier (Value 'S) (Value 'C) symProg conProg
        verifier nbits timeout =
          SMTVerifier
            { smtVerifierSolverConfig = rvvCombinedVerifierFuzzerSolverConfig,
              smtVerifierTimeoutSeconds = timeout,
              smtVerifierSymSemantics = vconst,
              smtVerifierConSemantics = vconst,
              smtVerifierInputs =
                (\g -> g nbits vconst) <$> rvvCombinedVerifierFuzzerVerifierInputs,
              smtVerifierSpec = rvvCombinedVerifierFuzzerVerifierSpec vconst
            }
        qcFuzzer ::
          QuickCheckFuzzer (Value 'S) (Value 'C) symProg conProg
        qcFuzzer =
          QuickCheckFuzzer
            { quickCheckFuzzerSymSemantics = vconst,
              quickCheckFuzzerConSemantics = vconst,
              quickCheckFuzzerMaxTests = rvvCombinedVerifierFuzzerFuzzerMaxTests,
              quickCheckFuzzerGenerators =
                (\g -> g vconst) <$> rvvCombinedVerifierFuzzerFuzzerGenerators,
              quickCheckFuzzerSpec = rvvCombinedVerifierFuzzerFuzzerSpec vconst
            }
        verifierSlowFun =
          head $
            toVerifierFuns
              (verifier Nothing $ Just rvvCombinedVerifierFuzzerSlowVerifierTimeoutSeconds)
              table
              key
        [fuzzerFun] = toVerifierFuns qcFuzzer table key
        hasCex (CEGISVerifierFoundCex _) = True
        hasCex _ = False
        definitelyNoCex (CEGISVerifierNoCex r) = r
        definitelyNoCex _ = False
        maybeLog Nothing _ _ = return ()
        maybeLog (Just logger) level msg = logMultiLineDoc logger level msg
        numEmptyBits :: Bool -> GetSomeWordN 'C -> Int
        numEmptyBits isHigher v =
          if isHigher
            then countLeadingZeros v
            else countTrailingZeros v
        numBits :: Bool -> Value 'C -> Int
        numBits _ (VLValue {}) = 0
        numBits isHigher (VectorValue v) =
          let concated = concatData v
              uninitialized = concatUninitialized v
           in finiteBitSize concated
                - min (numEmptyBits isHigher concated) (numEmptyBits isHigher uninitialized)
        numBits isHigher (MaskValue (Mask _ (VectorReg d u))) =
          finiteBitSize d - min (numEmptyBits isHigher d) (numEmptyBits isHigher u)
        numBits isHigher (ScalarValue (Scalar v _)) = finiteBitSize v - numEmptyBits isHigher v
        numBits _ (PtrValue _) = error "Not supported"
        numBits _ (MemValue _) = error "Not supported"

        -- Integrated function that handles both masking and bit behavior strategy
        applyBitMaskStrategy :: (FiniteBits a, Bits a, BV a, Num a) => Bool -> Int -> Int -> a -> CexReductionBehavior -> a
        applyBitMaskStrategy isHigher numTotal n value maskedBitBehavior =
          let createMask total num
                | total <= num = bv total (-1)
                | isHigher = (bv total 1 `shiftL` num) - 1
                | otherwise = complement $ (bv total 1 `shiftL` (total - num)) - 1
              mask = createMask numTotal n
              applyStrategy val =
                case maskedBitBehavior of
                  ZeroOut -> val .&. mask -- Bits outside mask become 0
                  SetToOne -> val .|. complement mask -- Bits outside mask become 1
           in applyStrategy value

        reduceBits :: Bool -> MachineConfig -> Int -> Value 'C -> Value 'C
        reduceBits _ _ _ vl@VLValue {} = vl
        reduceBits isHigher machine n (VectorValue v@(Vector config _)) =
          let concated = concatData v
              uninitialized = concatUninitialized v
              numTotal = finiteBitSize concated

              -- Apply the bit masking strategy based on user preference for data bits
              finalValue = applyBitMaskStrategy isHigher numTotal n concated rvvCombinedVerifierFuzzerCexReductionBehavior

              -- For uninitialized bits, always use ZeroOut strategy to make masked-out bits 0
              -- This ensures that uninitialized bits outside the mask are always 0
              finalUninit = applyBitMaskStrategy isHigher numTotal n uninitialized ZeroOut
           in VectorValue $
                fromConcatenated
                  machine
                  config
                  False
                  finalValue
                  finalUninit
        reduceBits isHigher _ n (MaskValue (Mask maskMul (VectorReg d u))) =
          let numTotal = finiteBitSize d

              -- Apply the bit masking strategy based on user preference for data bits
              finalD = applyBitMaskStrategy isHigher numTotal n d rvvCombinedVerifierFuzzerCexReductionBehavior

              -- For uninitialized bits, always use ZeroOut strategy to make masked-out bits 0
              finalU = applyBitMaskStrategy isHigher numTotal n u ZeroOut
           in MaskValue (Mask maskMul (VectorReg finalD finalU))
        reduceBits isHigher _ n (ScalarValue (Scalar v u)) =
          let numTotal = finiteBitSize v

              finalV = applyBitMaskStrategy isHigher numTotal n v rvvCombinedVerifierFuzzerCexReductionBehavior
           in ScalarValue $ Scalar finalV u
        reduceBits _ _ _ _ = error "Not supported"
        numIOPairBits :: Bool -> IOPair (Value 'C) -> Int
        numIOPairBits isHigher IOPair {..} =
          case ioPairInputs of
            [] -> 0
            _ -> maximum $ numBits isHigher <$> ioPairInputs
        reduceExampleBits ::
          Bool ->
          Int ->
          Example MachineConfig (Value 'S) MachineConfig (Value 'C) RegListMatcher ->
          SymbolTable conProg ->
          ConcreteContext (Example MachineConfig (Value 'S) MachineConfig (Value 'C) RegListMatcher)
        reduceExampleBits isHigher n (Example machine _ ps ioPair _) conTable = do
          let inputs = reduceBits isHigher machine n <$> ioPairInputs ioPair
          (outputs, matcher) <- rvvCombinedVerifierFuzzerFuzzerSpec machine inputs
          let progOutput = runSymbol machine conTable key inputs :: ConcreteContext [Value 'C]
          let reduced = Example machine machine ps (IOPair inputs outputs) matcher
          case progOutput of
            Left _ -> return reduced
            Right progOutputs ->
              if match matcher progOutputs outputs
                then Left "Not reduced"
                else return reduced
        binarySearchReduceExampleBits' ::
          Bool ->
          Int ->
          Int ->
          Example MachineConfig (Value 'S) MachineConfig (Value 'C) RegListMatcher ->
          SymbolTable conProg ->
          IO Int
        binarySearchReduceExampleBits' isHigher lo hi e conTable
          | lo >= hi = return lo -- Base case: found first valid index
          | otherwise = do
              let mid = (lo + hi) `div` 2
              let result = reduceExampleBits isHigher mid e conTable -- Get actual IO result
              if isLeft result
                then binarySearchReduceExampleBits' isHigher (mid + 1) hi e conTable -- Search upper if failed
                else binarySearchReduceExampleBits' isHigher lo mid e conTable -- Search lower if succeeded
        numExampleBits ::
          Bool ->
          Example MachineConfig (Value 'S) MachineConfig (Value 'C) RegListMatcher ->
          Int
        numExampleBits isHigher Example {..} =
          numIOPairBits isHigher exampleIOPair

        binarySearchReduceExampleBits ::
          Bool ->
          Example MachineConfig (Value 'S) MachineConfig (Value 'C) RegListMatcher ->
          SymbolTable conProg ->
          IO (Example MachineConfig (Value 'S) MachineConfig (Value 'C) RegListMatcher)
        binarySearchReduceExampleBits isHigher e conTable = do
          let n = numExampleBits isHigher e
          n' <- binarySearchReduceExampleBits' isHigher 0 n e conTable
          let behaviorType = case rvvCombinedVerifierFuzzerCexReductionBehavior of
                ZeroOut -> "zeroed"
                SetToOne -> "set to 1"
          maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
            "Number of kept "
              <> (if isHigher then "trailing" else "leading")
              <> " bits (masked-out bits "
              <> behaviorType
              <> "): "
              <> pformat n'
          let result = reduceExampleBits isHigher n' e conTable
          case result of
            Left err -> do
              maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
                "Failed to reduce example: "
                  <> pformat err
                  <> ". Using original example."
              return e
            Right reducedResult -> return reducedResult

        reduceExample :: SomeExample symProg conProg -> SymbolTable conProg -> IO (SomeExample symProg conProg)
        reduceExample ex@(SomeExample (e :: e)) conTable =
          case eqT @e @(Example MachineConfig (Value 'S) MachineConfig (Value 'C) RegListMatcher) of
            Just Refl -> do
              maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
                "Reducing example: " <> pformat e
              newExample <- binarySearchReduceExampleBits True e conTable
              newExample <- binarySearchReduceExampleBits False newExample conTable
              maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
                "Reduced example: " <> pformat newExample
              let progOutput =
                    runSymbol
                      (exampleConSemantics newExample)
                      conTable
                      key
                      (ioPairInputs (exampleIOPair newExample)) ::
                      ConcreteContext [Value 'C]
              maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
                "Program output: " <> pformat progOutput
              return $ SomeExample newExample
            Nothing -> return ex
        reduceResult ::
          VerifierResult (SomeExample symProg conProg) T.Text ->
          SymbolTable conProg ->
          IO (VerifierResult (SomeExample symProg conProg) T.Text)
        reduceResult (CEGISVerifierFoundCex p) conTable = do
          reduced <- reduceExample p conTable
          return $ CEGISVerifierFoundCex reduced
        reduceResult r _ = return r
     in [ \model -> do
            let conProg = evalSymToCon model table :: SymbolTable conProg
            maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
              nest 2 $
                vsep
                  [ "Verifying program ",
                    pformat conProg
                  ]
            -- r0 <-
            --   if rvvCombinedVerifierFuzzerFastVerifierTimeoutSeconds > 0
            --     then verifierFastFun model
            --     else return $ CEGISVerifierNoCex False

            r0 <-
              if rvvCombinedVerifierFuzzerSlowVerifierTimeoutSeconds > 0
                then do
                  maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
                    "Verifying with "
                      <> pformat vconst
                      <> ", timeout: "
                      <> pformat rvvCombinedVerifierFuzzerSlowVerifierTimeoutSeconds
                  r <- verifierSlowFun model
                  maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
                    "Verification finished with: " <> pformat r
                  return r
                else return $ CEGISVerifierNoCex False
            r2 <-
              if not (definitelyNoCex r0)
                && not (hasCex r0)
                && rvvCombinedVerifierFuzzerFuzzerMaxTests > 0
                then do
                  maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
                    "Fuzzing with "
                      <> pformat vconst
                      <> ", max test num: "
                      <> pformat rvvCombinedVerifierFuzzerFuzzerMaxTests
                  r <- fuzzerFun model
                  maybeLog rvvCombinedVerifierFuzzerLogger NOTICE $
                    "Fuzzing finished with: " <> pformat r
                  return r
                else return r0
            reduceResult r2 conProg
        ]

data RVVSMTVerifier symProg conProg where
  RVVSMTVerifier ::
    ( SymExampleConstraint MachineConfig symProg (Value 'S) RegListMatcher,
      ConExampleConstraint MachineConfig conProg (Value 'C) RegListMatcher,
      ProgSemantics MachineConfig conProg (Value 'S) SymbolicContext
    ) =>
    { rvvSMTVerifierSolverConfig :: GrisetteSMTConfig,
      rvvSMTVerifierTimeoutSeconds :: Maybe Double,
      rvvSMTVerifierMachineConfigs :: [MachineConfig],
      rvvSMTVerifierInputs :: [MachineConfig -> AngelicContext [Value 'S]],
      rvvSMTVerifierSpec ::
        MachineConfig ->
        [Value 'S] ->
        SymbolicContext ([Value 'S], RegListMatcher),
      rvvSMTVerifierLogger :: Maybe Logger
    } ->
    RVVSMTVerifier symProg conProg

instance
  ( EvalSym symProg,
    ToCon symProg conProg
  ) =>
  IsVerifier (RVVSMTVerifier symProg conProg) symProg conProg
  where
  toVerifierFuns (RVVSMTVerifier {..}) table key = do
    vconst <- rvvSMTVerifierMachineConfigs
    let qcFuzzer ::
          SMTVerifier (Value 'S) (Value 'C) symProg conProg
        qcFuzzer =
          SMTVerifier
            { smtVerifierSolverConfig = rvvSMTVerifierSolverConfig,
              smtVerifierTimeoutSeconds = rvvSMTVerifierTimeoutSeconds,
              smtVerifierSymSemantics = vconst,
              smtVerifierConSemantics = vconst,
              smtVerifierInputs = (\g -> g vconst) <$> rvvSMTVerifierInputs,
              smtVerifierSpec = rvvSMTVerifierSpec vconst
            }
        wrapLogger f model = do
          case rvvSMTVerifierLogger of
            Just logger -> do
              logMultiLineDoc logger NOTICE $
                "Verifying with "
                  <> pformat vconst
                  <> ", timeout: "
                  <> pformat rvvSMTVerifierTimeoutSeconds
              r <- f model
              logMultiLineDoc logger NOTICE $
                "Verification finished with: " <> pformat r
              return r
            Nothing -> f model
     in wrapLogger <$> toVerifierFuns qcFuzzer table key

data RVVQuickCheckFuzzer symProg conProg where
  RVVQuickCheckFuzzer ::
    ( SymExampleConstraint MachineConfig symProg (Value 'S) RegListMatcher,
      ConExampleConstraint MachineConfig conProg (Value 'C) RegListMatcher
    ) =>
    { rvvQuickCheckFuzzerMachineConfigs :: [MachineConfig],
      rvvQuickCheckFuzzerMaxTests :: Int,
      rvvQuickCheckFuzzerGenerators :: [MachineConfig -> Gen [Value 'C]],
      rvvQuickCheckFuzzerSpec ::
        MachineConfig ->
        [Value 'C] ->
        ConcreteContext ([Value 'C], RegListMatcher),
      rvvQuickCheckFuzzerLogger :: Maybe Logger
    } ->
    RVVQuickCheckFuzzer symProg conProg

instance
  ( EvalSym symProg,
    ToCon symProg conProg
  ) =>
  IsVerifier (RVVQuickCheckFuzzer symProg conProg) symProg conProg
  where
  toVerifierFuns (RVVQuickCheckFuzzer {..}) table key = do
    vconst <- rvvQuickCheckFuzzerMachineConfigs
    let qcFuzzer ::
          QuickCheckFuzzer (Value 'S) (Value 'C) symProg conProg
        qcFuzzer =
          QuickCheckFuzzer
            { quickCheckFuzzerSymSemantics = vconst,
              quickCheckFuzzerConSemantics = vconst,
              quickCheckFuzzerMaxTests = rvvQuickCheckFuzzerMaxTests,
              quickCheckFuzzerGenerators =
                (\g -> g vconst) <$> rvvQuickCheckFuzzerGenerators,
              quickCheckFuzzerSpec = rvvQuickCheckFuzzerSpec vconst
            }
        wrapLogger f model = do
          case rvvQuickCheckFuzzerLogger of
            Just logger -> do
              logMultiLineDoc logger NOTICE $
                "Fuzzing with "
                  <> pformat vconst
                  <> ", max tests: "
                  <> pformat rvvQuickCheckFuzzerMaxTests
              r <- f model
              logMultiLineDoc logger NOTICE $
                "Fuzzing finished with: " <> pformat r
              return r
            Nothing -> f model
     in wrapLogger <$> toVerifierFuns qcFuzzer table key
