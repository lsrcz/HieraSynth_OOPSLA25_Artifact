{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HieraSynth.Program.Concrete.ToDotTest (toDotTest) where

import Control.Monad.State (StateT (runStateT))
import Data.GraphViz
  ( DotEdge (DotEdge),
    DotNode (DotNode, nodeAttributes, nodeID),
    DotStatements (DotStmts, attrStmts, edgeStmts, nodeStmts, subGraphs),
    DotSubGraph (DotSG),
    GlobalAttributes (GraphAttrs),
    GraphID (Str),
    Shape (Record),
    textLabel,
  )
import Data.GraphViz.Attributes.Complete
  ( Attribute (HeadPort, Label, Shape, TailPort),
    Label (RecordLabel),
    PortName (PN),
    PortPos (LabelledPort),
    RecordField (FieldLabel, FlipFields, LabelledTarget),
  )
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import HieraSynth.Program.Concrete
  ( OpPPrintError (RedefinedResult, UndefinedArgument),
    Prog (Prog),
    ProgArg (ProgArg),
    ProgPPrintError (StmtPPrintError),
    ProgRes (ProgRes),
    Stmt (Stmt),
    progToDotSubGraph,
    stmtToDotNode,
  )
import HieraSynth.Program.Concrete.OpToDot (VarIdToLabel)
import HieraSynth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (PrettyOp1, PrettyOp2),
    TestPrettyType (PrettyType1, PrettyType2),
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

data StmtToDotTestCase = StmtToDotTestCase
  { testStmtName :: String,
    testStmt :: Stmt TestPrettyOp Int,
    testStmtIndex :: Int,
    testStmtExpectedResult ::
      Either
        (ProgPPrintError Int TestPrettyOp)
        (DotNode T.Text, [DotEdge T.Text]),
    testStmtNewMap :: VarIdToLabel Int
  }

data ProgToDotTestCase = ProgToDotTestCase
  { testProgName :: String,
    testProg :: Prog TestPrettyOp Int TestPrettyType,
    testProgExpectedResult ::
      Either
        (ProgPPrintError Int TestPrettyOp)
        (DotSubGraph T.Text)
  }

toDotTest :: Test
toDotTest =
  testGroup
    "ToDot"
    [ testGroup "stmtToDotNode" $ do
        (StmtToDotTestCase name stmt index expected newMap) <-
          [ StmtToDotTestCase
              { testStmtName = "1 ret, 1 arg",
                testStmt = Stmt PrettyOp1 [0] [2],
                testStmtIndex = 3,
                testStmtExpectedResult =
                  Right
                    ( DotNode
                        "prog_stmt3"
                        [ Label . RecordLabel $
                            [ FlipFields
                                [ FlipFields [LabelledTarget (PN "arg0") "op1"],
                                  FieldLabel "op1",
                                  FlipFields [LabelledTarget (PN "res0") "t1_"]
                                ]
                            ],
                          Shape Record
                        ],
                      [ DotEdge
                          "prog_args"
                          "prog_stmt3"
                          [ HeadPort $ LabelledPort (PN "arg0") Nothing,
                            TailPort $
                              LabelledPort (PN "x: PrettyType1") Nothing
                          ]
                      ]
                    ),
                testStmtNewMap =
                  HM.fromList
                    [ (0, ("prog_args", PN "x: PrettyType1")),
                      (1, ("prog_args", PN "y: PrettyType2")),
                      (2, ("prog_stmt3", PN "res0"))
                    ]
              },
            StmtToDotTestCase
              { testStmtName = "2 ret, 2 arg",
                testStmt = Stmt PrettyOp2 [1, 0] [2, 3],
                testStmtIndex = 3,
                testStmtExpectedResult =
                  Right
                    ( DotNode
                        "prog_stmt3"
                        [ Label . RecordLabel $
                            [ FlipFields
                                [ FlipFields
                                    [ LabelledTarget (PN "arg0") "op2'2'0'arg",
                                      LabelledTarget (PN "arg1") "arg1"
                                    ],
                                  FieldLabel "op2",
                                  FlipFields
                                    [ LabelledTarget (PN "res0") "op2_",
                                      LabelledTarget (PN "res1") "op2'_"
                                    ]
                                ]
                            ],
                          Shape Record
                        ],
                      [ DotEdge
                          "prog_args"
                          "prog_stmt3"
                          [ HeadPort $ LabelledPort (PN "arg0") Nothing,
                            TailPort $
                              LabelledPort (PN "y: PrettyType2") Nothing
                          ],
                        DotEdge
                          "prog_args"
                          "prog_stmt3"
                          [ HeadPort $ LabelledPort (PN "arg1") Nothing,
                            TailPort $
                              LabelledPort (PN "x: PrettyType1") Nothing
                          ]
                      ]
                    ),
                testStmtNewMap =
                  HM.fromList
                    [ (0, ("prog_args", PN "x: PrettyType1")),
                      (1, ("prog_args", PN "y: PrettyType2")),
                      (2, ("prog_stmt3", PN "res0")),
                      (3, ("prog_stmt3", PN "res1"))
                    ]
              },
            StmtToDotTestCase
              { testStmtName = "arg error",
                testStmt = Stmt PrettyOp1 [2] [3],
                testStmtIndex = 3,
                testStmtExpectedResult =
                  Left . StmtPPrintError (Stmt PrettyOp1 [2] [3]) 3 $
                    UndefinedArgument 0 2,
                testStmtNewMap = env
              },
            StmtToDotTestCase
              { testStmtName = "res error",
                testStmt = Stmt PrettyOp2 [0, 1] [1, 2],
                testStmtIndex = 3,
                testStmtExpectedResult =
                  Left . StmtPPrintError (Stmt PrettyOp2 [0, 1] [1, 2]) 3 $
                    RedefinedResult 0 1,
                testStmtNewMap = env
              }
          ]
        return $ testCase name $ do
          let actual = flip runStateT env $ stmtToDotNode "prog" index stmt
          actual @?= (,newMap) <$> expected,
      testGroup "progToDotSubGraph" $ do
        ProgToDotTestCase name prog expected <-
          [ ProgToDotTestCase
              { testProgName = "2 stmt",
                testProg =
                  Prog
                    [ProgArg "x" 0 PrettyType1, ProgArg "y" 1 PrettyType2]
                    [ Stmt PrettyOp2 [0, 1] [2, 3],
                      Stmt PrettyOp1 [3] [4]
                    ]
                    [ProgRes 4 PrettyType1, ProgRes 2 PrettyType2],
                testProgExpectedResult =
                  let argNode =
                        DotNode
                          { nodeID = "prog_args",
                            nodeAttributes =
                              [ Label . RecordLabel . return . FlipFields $
                                  [ FieldLabel "args",
                                    FlipFields
                                      [ LabelledTarget
                                          (PN "x")
                                          "x: PrettyType1",
                                        LabelledTarget
                                          (PN "y")
                                          "y: PrettyType2"
                                      ]
                                  ],
                                Shape Record
                              ]
                          }
                      stmt0Node =
                        DotNode
                          { nodeID = "prog_stmt0",
                            nodeAttributes =
                              [ Label . RecordLabel . return . FlipFields $
                                  [ FlipFields
                                      [ LabelledTarget
                                          (PN "arg0")
                                          "op2'2'0'arg",
                                        LabelledTarget (PN "arg1") "arg1"
                                      ],
                                    FieldLabel "op2",
                                    FlipFields
                                      [ LabelledTarget (PN "res0") "op2_",
                                        LabelledTarget (PN "res1") "op2'_"
                                      ]
                                  ],
                                Shape Record
                              ]
                          }
                      stmt1Node =
                        DotNode
                          { nodeID = "prog_stmt1",
                            nodeAttributes =
                              [ Label . RecordLabel . return . FlipFields $
                                  [ FlipFields
                                      [LabelledTarget (PN "arg0") "op1"],
                                    FieldLabel "op1",
                                    FlipFields
                                      [LabelledTarget (PN "res0") "t1_"]
                                  ],
                                Shape Record
                              ]
                          }
                      resNode =
                        DotNode
                          { nodeID = "prog_res",
                            nodeAttributes =
                              [ Label . RecordLabel . return . FlipFields $
                                  [ FlipFields
                                      [ LabelledTarget
                                          (PN "res0")
                                          "res0: PrettyType1",
                                        LabelledTarget
                                          (PN "res1")
                                          "res1: PrettyType2"
                                      ],
                                    FieldLabel "res"
                                  ],
                                Shape Record
                              ]
                          }
                      edge00 =
                        DotEdge
                          "prog_args"
                          "prog_stmt0"
                          [ HeadPort $ LabelledPort (PN "arg0") Nothing,
                            TailPort $ LabelledPort (PN "x") Nothing
                          ]
                      edge01 =
                        DotEdge
                          "prog_args"
                          "prog_stmt0"
                          [ HeadPort $ LabelledPort (PN "arg1") Nothing,
                            TailPort $ LabelledPort (PN "y") Nothing
                          ]
                      edge10 =
                        DotEdge
                          "prog_stmt0"
                          "prog_stmt1"
                          [ HeadPort $ LabelledPort (PN "arg0") Nothing,
                            TailPort $ LabelledPort (PN "res1") Nothing
                          ]
                      edger0 =
                        DotEdge
                          "prog_stmt1"
                          "prog_res"
                          [ HeadPort $ LabelledPort (PN "res0") Nothing,
                            TailPort $ LabelledPort (PN "res0") Nothing
                          ]
                      edger1 =
                        DotEdge
                          "prog_stmt0"
                          "prog_res"
                          [ HeadPort $ LabelledPort (PN "res1") Nothing,
                            TailPort $ LabelledPort (PN "res0") Nothing
                          ]
                   in Right $
                        DotSG True (Just $ Str "prog") $
                          DotStmts
                            { attrStmts = [GraphAttrs [textLabel "prog"]],
                              subGraphs = [],
                              nodeStmts =
                                [argNode, stmt0Node, stmt1Node, resNode],
                              edgeStmts =
                                [edge00, edge01, edge10, edger0, edger1]
                            }
              }
          ]
        return $ testCase name $ do
          let actual = progToDotSubGraph "prog" prog
          actual @?= expected
    ]
