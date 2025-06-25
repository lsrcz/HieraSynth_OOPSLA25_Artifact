{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HieraSynth.Reasoning.Synthesis.ComponentSketchTest
  ( ConProg,
    SymProg,
    componentSketchTest,
    ComponentSynthesisTestCase (..),
    task,
    sharedSketchTable,
    fuzzResult,
    times4SketchTable,
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Typeable)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Grisette
  ( LogicalOp (true),
    Mergeable,
    Solvable (con),
    SolvingFailure (Unsat),
    SymBool,
    SymInteger,
    Union,
    mrgIf,
    mrgReturn,
    z3,
  )
import HieraSynth.Context (ConcreteContext)
import HieraSynth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.Concrete.Flatten (flattenSymbolTable)
import HieraSynth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (PerStmtCostObj),
  )
import HieraSynth.Program.ProgSemantics (ProgSemantics, evalSymbolTable)
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
import HieraSynth.Reasoning.IOPair (IOPair (IOPair))
import HieraSynth.Reasoning.Matcher (EqMatcher (EqMatcher), Matcher)
import HieraSynth.Reasoning.Synthesis
  ( Example
      ( Example,
        exampleConSemantics,
        exampleIOPair,
        exampleMatcher,
        exampleSymSemantics,
        exampleSymValType
      ),
    SomeExample (SomeExample),
    SomeVerifier (SomeVerifier),
    SynthesisMinimalCostTask
      ( SynthesisMinimalCostTask,
        synthesisConCostObj,
        synthesisExtraConstraints,
        synthesisInitialExamples,
        synthesisInitialMaxCost,
        synthesisPrecondition,
        synthesisSketchSymbol,
        synthesisSketchTable,
        synthesisSymCostObj,
        synthesisVerifiers
      ),
    SynthesisResult (SynthesisSolverFailure, SynthesisSuccess),
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
  ( addThenDivModGen,
    addThenDivModSpec,
    addThenDoubleGen,
    addThenDoubleReverseSpec,
    addThenDoubleSpec,
    divModTwiceGen,
    divModTwiceSpec,
    times4Gen,
    times4Spec,
  )
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsCost (TestSemanticsCost),
    TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod, Double),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.QuickCheck.Counterexamples (Gen)

type ConVal = Integer

type SymVal = SymInteger

type ConProg = Concrete.Prog TestSemanticsOp Integer TestSemanticsType

type SymProg = Prog (Union TestSemanticsOp) SymInteger TestSemanticsType

sharedSketchTable :: SymbolTable SymProg
sharedSketchTable =
  SymbolTable
    [ ( "test",
        Prog
          [ProgArg "x" IntType, ProgArg "y" IntType]
          [ Stmt
              (mrgReturn Add)
              ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
              "stmt0'arg_num"
              ["stmt0'ret0", "stmt0'ret1"]
              "stmt0'ret_num"
              "stmt0'dis"
              [],
            Stmt
              (mrgReturn Add)
              ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
              "stmt1'arg_num"
              ["stmt1'ret0"]
              "stmt1'ret_num"
              "stmt1'dis"
              [],
            Stmt
              (mrgReturn DivMod)
              ["stmt2'arg0", "stmt2'arg1"]
              "stmt2'arg_num"
              ["stmt2'ret0", "stmt2'ret1", "stmt2'ret2"]
              "stmt2'ret_num"
              "stmt2'dis"
              [],
            Stmt
              (mrgReturn DivMod)
              ["stmt3'arg0", "stmt3'arg1"]
              "stmt3'arg_num"
              ["stmt3'ret0", "stmt3'ret1"]
              "stmt3'ret_num"
              "stmt3'dis"
              []
          ]
          [ProgRes "res0" IntType, ProgRes "res1" IntType]
      )
    ]

sharedSketchUnionTable :: SymbolTable SymProg
sharedSketchUnionTable =
  SymbolTable
    [ ( "test",
        Prog
          [ProgArg "x" IntType, ProgArg "y" IntType]
          [ Stmt
              (mrgIf "stmt0'sel" (return Add) (return DivMod))
              ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
              "stmt0'arg_num"
              ["stmt0'ret0", "stmt0'ret1", "stmt0'ret2"]
              "stmt0'ret_num"
              "stmt0'dis"
              [],
            Stmt
              (mrgIf "stmt1'sel" (return Add) (return DivMod))
              ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
              "stmt1'arg_num"
              ["stmt1'ret0", "stmt1'ret1", "stmt1'ret2"]
              "stmt1'ret_num"
              "stmt1'dis"
              []
          ]
          [ProgRes "res0" IntType, ProgRes "res1" IntType]
      )
    ]

addThenDivModSketchTable :: SymbolTable SymProg
addThenDivModSketchTable =
  SymbolTable
    [ ( "test",
        Prog
          [ProgArg "x" IntType, ProgArg "y" IntType, ProgArg "z" IntType]
          [ Stmt
              (mrgReturn Add)
              ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
              "stmt0'arg_num"
              ["stmt0'ret0", "stmt0'ret1", "stmt0'ret2"]
              "stmt0'ret_num"
              "stmt0'dis"
              [],
            Stmt
              (mrgReturn DivMod)
              ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
              "stmt1'arg_num"
              ["stmt1'ret0", "stmt1'ret1", "stmt1'ret2"]
              "stmt1'ret_num"
              "stmt1'dis"
              ["stmt0'ret0", "stmt0'ret1", "stmt0'ret2"]
          ]
          [ProgRes "res0" IntType, ProgRes "res1" IntType]
      )
    ]

addThenDivModSketchBadMustBeAfterTable :: SymbolTable SymProg
addThenDivModSketchBadMustBeAfterTable =
  SymbolTable
    [ ( "test",
        Prog
          [ProgArg "x" IntType, ProgArg "y" IntType, ProgArg "z" IntType]
          [ Stmt
              (return Add)
              ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
              "stmt0'arg_num"
              ["stmt0'ret0", "stmt0'ret1", "stmt0'ret2"]
              "stmt0'ret_num"
              "stmt0'dis"
              ["stmt1'ret0", "stmt1'ret1", "stmt1'ret2"],
            Stmt
              (return DivMod)
              ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
              "stmt1'arg_num"
              ["stmt1'ret0", "stmt1'ret1", "stmt1'ret2"]
              "stmt1'ret_num"
              "stmt1'dis"
              []
          ]
          [ProgRes "res0" IntType, ProgRes "res1" IntType]
      )
    ]

data ComponentSynthesisTestCase where
  ComponentSynthesisTestCase ::
    forall matcher.
    ( Matcher matcher Bool Integer,
      Matcher matcher SymBool SymInteger,
      Typeable matcher,
      NFData matcher,
      Eq matcher
    ) =>
    { componentSynthesisTestCaseName :: String,
      componentSynthesisTestCaseSpec ::
        [Integer] ->
        ConcreteContext ([Integer], matcher),
      componentSynthesisTestCaseInitialExamples ::
        [SomeExample SymProg ConProg],
      componentSynthesisTestCaseSynthGen :: Gen [Integer],
      componentSynthesisTestCaseFuzzGen :: Gen [Integer]
    } ->
    ComponentSynthesisTestCase

fuzzResult ::
  ( Matcher b Bool conVal,
    ProgSemantics TestSemanticsObj conProg conVal ConcreteContext,
    Eq conVal,
    Show conVal,
    Mergeable conVal,
    Show conProg
  ) =>
  SynthesisResult conProg ->
  T.Text ->
  Gen [conVal] ->
  ([conVal] -> ConcreteContext ([conVal], b)) ->
  IO ()
fuzzResult (SynthesisSuccess prog) symbol gen spec = do
  fuzzingResult <-
    fuzzingTestProg
      gen
      spec
      100
      (evalSymbolTable TestSemanticsObj prog)
      symbol
  fst <$> fuzzingResult @?= Nothing
fuzzResult r _ _ _ = fail $ "Unexpected result: " <> show r

example :: IOPair Integer -> SomeExample SymProg ConProg
example iop =
  SomeExample $
    Example
      { exampleConSemantics = TestSemanticsObj,
        exampleSymSemantics = TestSemanticsObj,
        exampleSymValType = Proxy :: Proxy SymInteger,
        exampleIOPair = iop,
        exampleMatcher = EqMatcher
      }

verifier ::
  ( Matcher matcher SymBool SymVal,
    Matcher matcher Bool ConVal,
    Typeable matcher,
    NFData matcher,
    Eq matcher
  ) =>
  ([ConVal] -> ConcreteContext ([ConVal], matcher)) ->
  Gen [ConVal] ->
  QuickCheckFuzzer SymVal ConVal SymProg ConProg
verifier spec gen =
  QuickCheckFuzzer
    { quickCheckFuzzerSymSemantics = TestSemanticsObj,
      quickCheckFuzzerConSemantics = TestSemanticsObj,
      quickCheckFuzzerMaxTests = 100,
      quickCheckFuzzerGenerators = [gen],
      quickCheckFuzzerSpec = spec
    }

task ::
  ( Matcher matcher SymBool SymVal,
    Matcher matcher Bool ConVal,
    Typeable matcher,
    NFData matcher,
    Eq matcher
  ) =>
  ([ConVal] -> ConcreteContext ([ConVal], matcher)) ->
  Gen [ConVal] ->
  [SomeExample SymProg ConProg] ->
  SymbolTable SymProg ->
  T.Text ->
  SymBool ->
  SynthesisTask SymProg ConProg
task spec gen initialExamples table symbol precond =
  SynthesisTask
    { synthesisVerifiers = [SomeVerifier $ verifier spec gen],
      synthesisInitialExamples = initialExamples,
      synthesisSketchTable = table,
      synthesisSketchSymbol = symbol,
      synthesisPrecondition = precond,
      synthesisExtraConstraints = const $ return true
    }

componentSketchTest :: Test
componentSketchTest =
  testGroup
    "ComponentSketch"
    [ testGroup "general" $
        ( do
            (sketchTable, namePostFix) <-
              [ (sharedSketchTable, ""),
                (sharedSketchUnionTable, "/union")
              ]
            ComponentSynthesisTestCase
              name
              (spec :: [Integer] -> ConcreteContext ([Integer], matcher))
              examples
              synthGen
              fuzzGen <-
              [ ComponentSynthesisTestCase
                  { componentSynthesisTestCaseName = "Add then double",
                    componentSynthesisTestCaseSpec = addThenDoubleSpec,
                    componentSynthesisTestCaseInitialExamples = [],
                    componentSynthesisTestCaseSynthGen = addThenDoubleGen,
                    componentSynthesisTestCaseFuzzGen = addThenDoubleGen
                  },
                ComponentSynthesisTestCase
                  { componentSynthesisTestCaseName =
                      "Add then double, initial examples",
                    componentSynthesisTestCaseSpec = addThenDoubleSpec,
                    componentSynthesisTestCaseInitialExamples =
                      [ example $ IOPair [102, 50] [152, 304]
                      ],
                    componentSynthesisTestCaseSynthGen = return [0, 0],
                    componentSynthesisTestCaseFuzzGen = addThenDoubleGen
                  },
                ComponentSynthesisTestCase
                  { componentSynthesisTestCaseName = "Add then double/reverse",
                    componentSynthesisTestCaseSpec = addThenDoubleReverseSpec,
                    componentSynthesisTestCaseInitialExamples = [],
                    componentSynthesisTestCaseSynthGen = addThenDoubleGen,
                    componentSynthesisTestCaseFuzzGen = addThenDoubleGen
                  },
                ComponentSynthesisTestCase
                  { componentSynthesisTestCaseName = "DivMod twice",
                    componentSynthesisTestCaseSpec = divModTwiceSpec,
                    componentSynthesisTestCaseInitialExamples = [],
                    componentSynthesisTestCaseSynthGen = divModTwiceGen,
                    componentSynthesisTestCaseFuzzGen = divModTwiceGen
                  }
              ]

            return $ testCase (name <> namePostFix) $ do
              result <-
                runSynthesisTask z3 $
                  task spec synthGen examples sketchTable "test" (con True)
              fuzzResult result "test" fuzzGen spec
        )
          ++ [ testCase "Add then DivMod with must be after constraint" $ do
                 SynthesisSuccess result <-
                   runSynthesisTask z3 $
                     task
                       addThenDivModSpec
                       addThenDivModGen
                       []
                       addThenDivModSketchTable
                       "test"
                       (con True)
                 fuzzingResult <-
                   fuzzingTestProg
                     addThenDivModGen
                     addThenDivModSpec
                     100
                     (evalSymbolTable TestSemanticsObj result)
                     "test"
                 fst <$> fuzzingResult @?= Nothing,
               testCase "Add then DivMod with bad must be after constraint" $ do
                 SynthesisSolverFailure Unsat <-
                   runSynthesisTask z3 $
                     task
                       addThenDivModSpec
                       addThenDivModGen
                       []
                       addThenDivModSketchBadMustBeAfterTable
                       "test"
                       (con True)
                 return ()
             ],
      testGroup
        "With cost model"
        [ testCase "refinement" $ do
            let task =
                  SynthesisMinimalCostTask
                    { synthesisVerifiers =
                        [SomeVerifier $ verifier times4Spec times4Gen],
                      synthesisInitialExamples = [],
                      synthesisSketchTable = times4SketchTable,
                      synthesisSketchSymbol = "test",
                      synthesisPrecondition = con True,
                      synthesisInitialMaxCost =
                        Nothing :: Maybe SymInteger,
                      synthesisConCostObj =
                        PerStmtCostObj TestSemanticsCost,
                      synthesisSymCostObj =
                        PerStmtCostObj TestSemanticsCost,
                      synthesisExtraConstraints = const $ return true
                    }
            let expectedSynthesizedProg =
                  Concrete.Prog
                    [Concrete.ProgArg "x" (0 :: Integer) IntType]
                    [ Concrete.Stmt Double [0] [1],
                      Concrete.Stmt Double [1] [2]
                    ]
                    [Concrete.ProgRes 2 IntType]
            result <- runSynthesisTask z3 task
            case result of
              SynthesisSuccess result ->
                flattenSymbolTable result
                  @?= Right (SymbolTable [("test", expectedSynthesizedProg)])
              _ -> fail "Unexpected result",
          testCase "initial" $ do
            let task =
                  SynthesisMinimalCostTask
                    { synthesisVerifiers =
                        [SomeVerifier $ verifier times4Spec times4Gen],
                      synthesisInitialExamples = [],
                      synthesisSketchTable = times4SketchTable,
                      synthesisSketchSymbol = "test",
                      synthesisPrecondition = con True,
                      synthesisInitialMaxCost =
                        Just 2 :: Maybe SymInteger,
                      synthesisConCostObj =
                        PerStmtCostObj TestSemanticsCost,
                      synthesisSymCostObj =
                        PerStmtCostObj TestSemanticsCost,
                      synthesisExtraConstraints = const $ return true
                    }
            result <- runSynthesisTask z3 task
            case result of
              SynthesisSolverFailure Unsat -> return ()
              _ -> fail "Unexpected result"
        ]
    ]

times4SketchTable :: SymbolTable SymProg
times4SketchTable =
  SymbolTable
    [ ( "test",
        Prog
          [ProgArg "x" IntType]
          [ Stmt
              (mrgReturn Add)
              ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
              "stmt0'arg_num"
              ["stmt0'ret0", "stmt0'ret1"]
              "stmt0'ret_num"
              "stmt0'dis"
              [],
            Stmt
              (mrgReturn Add)
              ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
              "stmt1'arg_num"
              ["stmt1'ret0"]
              "stmt1'ret_num"
              "stmt1'dis"
              [],
            Stmt
              (mrgReturn Double)
              ["stmt2'arg0", "stmt2'arg1"]
              "stmt2'arg_num"
              ["stmt2'ret0", "stmt2'ret1", "stmt2'ret2"]
              "stmt2'ret_num"
              "stmt2'dis"
              [],
            Stmt
              (mrgReturn Double)
              ["stmt3'arg0", "stmt3'arg1"]
              "stmt3'arg_num"
              ["stmt3'ret0", "stmt3'ret1"]
              "stmt3'ret_num"
              "stmt3'dis"
              []
          ]
          [ProgRes "res0" IntType]
      )
    ]
