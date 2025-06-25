{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HieraSynth.Reasoning.Synthesis.ByteCodeSketchTest
  ( byteCodeSketchTest,
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Typeable)
import Grisette (Solvable (con), SymBool, SymInteger, Union, mrgIf, z3)
import HieraSynth.Context (ConcreteContext)
import HieraSynth.Program.ByteCodeSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.ProgSemantics (evalSymbolTable)
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.Reasoning.Fuzzing
  ( QuickCheckFuzzer
      ( QuickCheckFuzzer,
        quickCheckFuzzerConSemantics,
        quickCheckFuzzerGenerators,
        quickCheckFuzzerMaxTests,
        quickCheckFuzzerSpec,
        quickCheckFuzzerSymSemantics
      ),
    fuzzingTestProg,
  )
import HieraSynth.Reasoning.Matcher (Matcher)
import HieraSynth.Reasoning.Synthesis
  ( SomeVerifier (SomeVerifier),
    SynthesisResult (SynthesisSuccess),
    SynthesisTask
      ( SynthesisTask,
        synthesisExtraConstraints,
        synthesisInitialExamples,
        synthesisPrecondition,
        synthesisSketchSymbol,
        synthesisSketchTable,
        synthesisVerifiers
      ),
    runSynthesisTask,
  )
import HieraSynth.Reasoning.Synthesis.Problem
  ( addThenDoubleGen,
    addThenDoubleReverseSpec,
    addThenDoubleSpec,
    divModTwiceGen,
    divModTwiceSpec,
  )
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.QuickCheck.Counterexamples (Gen)

type ConVal = Integer

type SymVal = SymInteger

type ConProg = Concrete.Prog TestSemanticsOp Integer TestSemanticsType

type SymProg =
  Prog (Union TestSemanticsOp) Integer SymInteger TestSemanticsType

sharedSketch :: SymProg
sharedSketch =
  Prog
    [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
    [ Stmt
        (mrgIf "stmt0'op" (return Add) (return DivMod))
        ["stmt0'arg1", "stmt0'arg2"]
        "stmt0'numArg"
        [2, 3]
        "stmt0'numRes",
      Stmt
        (mrgIf "stmt1'op" (return Add) (return DivMod))
        ["stmt1'arg1", "stmt1'arg2"]
        "stmt1'numArg"
        [4, 5]
        "stmt1'numRes"
    ]
    [ProgRes "res0" IntType, ProgRes "res1" IntType]

data ByteCodeSynthesisTestCase where
  ByteCodeSynthesisTestCase ::
    forall matcher.
    ( Matcher matcher Bool Integer,
      Matcher matcher SymBool SymInteger,
      Typeable matcher,
      NFData matcher,
      Eq matcher
    ) =>
    { byteCodeSynthesisTestCaseName :: String,
      byteCodeSynthesisTestCaseSketch :: SymProg,
      byteCodeSynthesisTestCaseSpec ::
        [Integer] ->
        ConcreteContext ([Integer], matcher),
      byteCodeSynthesisTestCaseGen :: Gen [Integer]
    } ->
    ByteCodeSynthesisTestCase

byteCodeSketchTest :: Test
byteCodeSketchTest =
  testGroup "ByteCodeSketch" $ do
    ByteCodeSynthesisTestCase
      name
      sketch
      (spec :: [Integer] -> ConcreteContext ([Integer], matcher))
      gen <-
      [ ByteCodeSynthesisTestCase
          { byteCodeSynthesisTestCaseName = "Add then double",
            byteCodeSynthesisTestCaseSketch = sharedSketch,
            byteCodeSynthesisTestCaseSpec = addThenDoubleSpec,
            byteCodeSynthesisTestCaseGen = addThenDoubleGen
          },
        ByteCodeSynthesisTestCase
          { byteCodeSynthesisTestCaseName = "Add then double/reverse",
            byteCodeSynthesisTestCaseSketch = sharedSketch,
            byteCodeSynthesisTestCaseSpec = addThenDoubleReverseSpec,
            byteCodeSynthesisTestCaseGen = addThenDoubleGen
          },
        ByteCodeSynthesisTestCase
          { byteCodeSynthesisTestCaseName = "DivMod twice",
            byteCodeSynthesisTestCaseSketch = sharedSketch,
            byteCodeSynthesisTestCaseSpec = divModTwiceSpec,
            byteCodeSynthesisTestCaseGen = divModTwiceGen
          }
      ]
    let verifier =
          QuickCheckFuzzer
            { quickCheckFuzzerSymSemantics = TestSemanticsObj,
              quickCheckFuzzerConSemantics = TestSemanticsObj,
              quickCheckFuzzerMaxTests = 100,
              quickCheckFuzzerGenerators = [gen],
              quickCheckFuzzerSpec = spec
            } ::
            QuickCheckFuzzer SymVal ConVal SymProg ConProg
    let task =
          SynthesisTask
            { synthesisVerifiers = [SomeVerifier verifier],
              synthesisInitialExamples = [],
              synthesisSketchTable = SymbolTable [("test", sketch)],
              synthesisSketchSymbol = "test",
              synthesisPrecondition = con True,
              synthesisExtraConstraints = const $ return $ con True
            }
    return $ testCase name $ do
      SynthesisSuccess (result :: SymbolTable ConProg) <- runSynthesisTask z3 task
      fuzzingResult <-
        fuzzingTestProg
          gen
          spec
          100
          (evalSymbolTable TestSemanticsObj result)
          "test"
      fst <$> fuzzingResult @?= Nothing
