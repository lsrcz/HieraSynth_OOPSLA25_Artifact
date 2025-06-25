{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Operator.OpToDotTest (opToDotTest) where

import Data.GraphViz (DotEdge (DotEdge))
import Data.GraphViz.Attributes.Complete
  ( Attribute (HeadPort, TailPort),
    PortName (PN),
    PortPos (LabelledPort),
    RecordField (LabelledTarget),
  )
import qualified Data.HashMap.Lazy as HM
import HieraSynth.Program.Concrete
  ( OpPPrintError
      ( IncorrectNumberOfArguments,
        IncorrectNumberOfResults,
        RedefinedResult,
        UndefinedArgument
      ),
  )
import HieraSynth.Program.Concrete.OpToDot
  ( VarIdToLabel,
    argumentsToFieldEdges,
    resultsToFieldEdges,
  )
import HieraSynth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (PrettyOp0, PrettyOp1, PrettyOp2),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

env :: VarIdToLabel Int
env =
  HM.fromList
    [ (0, ("prog_args", PN "x: PrettyType1")),
      (1, ("prog_args", PN "y: PrettyType2"))
    ]

opToDotTest :: Test
opToDotTest =
  testGroup
    "HieraSynth.Operator.OpToDot"
    [ testGroup
        "argumentsToFieldEdges"
        [ testGroup
            "Errors"
            [ testCase "IncorrectNumberOfArguments" $ do
                let actual =
                      argumentsToFieldEdges "prog_stmt1" PrettyOp1 [0, 1] env
                let expected = Left (IncorrectNumberOfArguments PrettyOp1 1 2)
                actual @?= expected,
              testCase "UndefinedArgument" $ do
                let actual =
                      argumentsToFieldEdges "prog_stmt1" PrettyOp2 [1, 2] env
                let expected = Left (UndefinedArgument 1 2)
                actual @?= expected
            ],
          testGroup
            "Success"
            [ testCase "0 arguments" $ do
                argumentsToFieldEdges "prog_stmt1" PrettyOp0 [] env
                  @?= Right ([], []),
              testCase "1 arguments" $ do
                let actual =
                      argumentsToFieldEdges "prog_stmt1" PrettyOp1 [1] env
                let expected =
                      Right
                        ( [LabelledTarget (PN "arg0") "op1"],
                          [ DotEdge
                              "prog_args"
                              "prog_stmt1"
                              [ HeadPort $ LabelledPort (PN "arg0") Nothing,
                                TailPort $
                                  LabelledPort (PN "y: PrettyType2") Nothing
                              ]
                          ]
                        )
                actual @?= expected,
              testCase "2 arguments" $ do
                let actual =
                      argumentsToFieldEdges "prog_stmt2" PrettyOp2 [1, 0] env
                let expected =
                      Right
                        ( [ LabelledTarget (PN "arg0") "op2'2'0'arg",
                            LabelledTarget (PN "arg1") "arg1"
                          ],
                          [ DotEdge
                              "prog_args"
                              "prog_stmt2"
                              [ HeadPort $ LabelledPort (PN "arg0") Nothing,
                                TailPort $
                                  LabelledPort (PN "y: PrettyType2") Nothing
                              ],
                            DotEdge
                              "prog_args"
                              "prog_stmt2"
                              [ HeadPort $ LabelledPort (PN "arg1") Nothing,
                                TailPort $
                                  LabelledPort (PN "x: PrettyType1") Nothing
                              ]
                          ]
                        )
                actual @?= expected
            ]
        ],
      testGroup
        "resultsToFieldEdges"
        [ testGroup
            "Errors"
            [ testCase "IncorrectNumberOfResults" $ do
                let actual =
                      resultsToFieldEdges "prog_stmt1" PrettyOp1 [2, 3] env
                let expected = Left (IncorrectNumberOfResults PrettyOp1 1 2)
                actual @?= expected,
              testCase "RedefinedResult" $ do
                let actual =
                      resultsToFieldEdges "prog_stmt1" PrettyOp2 [0, 2] env
                let expected = Left (RedefinedResult 0 0)
                actual @?= expected
            ],
          testGroup
            "Success"
            [ testCase "1 results" $ do
                let actual = resultsToFieldEdges "prog_stmt1" PrettyOp1 [3] env
                let expected =
                      Right
                        ( HM.fromList
                            [ (0, ("prog_args", PN "x: PrettyType1")),
                              (1, ("prog_args", PN "y: PrettyType2")),
                              (3, ("prog_stmt1", PN "res0"))
                            ],
                          [LabelledTarget (PN "res0") "t1_"]
                        )
                actual @?= expected,
              testCase "2 results" $ do
                let actual =
                      resultsToFieldEdges "prog_stmt1" PrettyOp2 [2, 3] env
                let expected =
                      Right
                        ( HM.fromList
                            [ (0, ("prog_args", PN "x: PrettyType1")),
                              (1, ("prog_args", PN "y: PrettyType2")),
                              (2, ("prog_stmt1", PN "res0")),
                              (3, ("prog_stmt1", PN "res1"))
                            ],
                          [ LabelledTarget (PN "res0") "op2_",
                            LabelledTarget (PN "res1") "op2'_"
                          ]
                        )
                actual @?= expected
            ]
        ]
    ]
