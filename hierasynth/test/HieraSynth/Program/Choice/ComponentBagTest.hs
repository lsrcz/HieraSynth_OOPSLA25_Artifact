{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Choice.ComponentBagTest
  ( componentBagTest,
  )
where

import Data.Either (fromRight)
import qualified Data.Text as T
import Grisette (SymInteger, Union, genSymSimple, isym, mrgIf, (.||))
import HieraSynth.Program.Choice.ChoiceTree
  ( ChoiceMeta (NoSplit, Split),
    ChoiceTree (Branch, Leaf),
  )
import HieraSynth.Program.Choice.ComponentBag
  ( ComponentBag (ComponentBag),
  )
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
  )
import HieraSynth.Program.ComponentSketch
  ( Prog (Prog, progArgList, progResList, progStmtList),
    ProgArg (ProgArg, progArgName, progArgType),
    ProgRes (ProgRes, progResId, progResType),
    Stmt
      ( Stmt,
        stmtArgIds,
        stmtArgNum,
        stmtDisabled,
        stmtMustBeAfter,
        stmtOp,
        stmtResIds,
        stmtResNum
      ),
  )
import HieraSynth.Program.Concrete (ProgPPrint (pformatProg))
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod, Double, Inc),
    TestSemanticsType (IntType),
  )
import HieraSynth.Util.Pretty (renderDoc)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

componentBagTest :: Test
componentBagTest =
  testGroup
    "ComponentBag"
    [ testCase "lowestSeqNum" $ do
        let bag =
              ComponentBag
                []
                [ (Branch (Split 2 False) [Leaf [Add, DivMod, Inc, Double]], 1),
                  (Branch (Split 1 True) [Leaf [Add, DivMod, Inc, Double]], 1)
                ]
                []
        lowestSeqNum True bag @?= Just 1
        lowestSeqNum False bag @?= Just 2,
      testCase "partitionSpec" $ do
        let getBag components =
              ComponentBag
                [IntType, IntType]
                components
                [IntType]
        let bag =
              getBag
                [ ( Branch
                      (Split 2 False)
                      [Leaf [Add, DivMod], Leaf [Inc, Double]],
                    1
                  ),
                  ( Branch
                      (Split 1 True)
                      [Leaf [Add, DivMod], Leaf [Inc, Double]],
                    2
                  )
                ]
        let bag0 =
              getBag
                [ ( Branch
                      (Split 2 False)
                      [Leaf [Add, DivMod], Leaf [Inc, Double]],
                    1
                  ),
                  (Branch NoSplit [Leaf [Inc, Double]], 2)
                ]
        let bag1 =
              getBag
                [ ( Branch
                      (Split 2 False)
                      [Leaf [Add, DivMod], Leaf [Inc, Double]],
                    1
                  ),
                  (Branch NoSplit [Leaf [Add, DivMod]], 1),
                  (Branch NoSplit [Leaf [Inc, Double]], 1)
                ]
        let bag2 =
              getBag
                [ ( Branch
                      (Split 2 False)
                      [Leaf [Add, DivMod], Leaf [Inc, Double]],
                    1
                  ),
                  (Branch NoSplit [Leaf [Add, DivMod]], 2)
                ]
        partitionSpec 1 bag @?= [bag0, bag1, bag2],
      testCase "PPrint" $ do
        let bag =
              ComponentBag
                [IntType, IntType]
                [ ( Branch
                      (Split 2 False)
                      [Leaf [Add, DivMod], Leaf [Inc, Double]],
                    1
                  ),
                  ( Branch
                      (Split 1 True)
                      [Leaf [Add, DivMod], Leaf [Inc, Double]],
                    1
                  )
                ]
                [IntType]
        renderDoc 80 (fromRight undefined (pformatProg "bag" bag))
          @?= T.intercalate
            "\n"
            [ "sketch bag(IntType, IntType) -> IntType:",
              "  reorder {",
              "    1 * {<add, divmod>, <inc, double>}@2,",
              "    1 * {<add, divmod>, <inc, double>}@1s",
              "  }"
            ],
      testCase "GenSym" $ do
        let bagTable =
              SymbolTable
                [ ( "prog",
                    ComponentBag
                      [IntType, IntType]
                      [ ( Branch
                            (Split 2 False)
                            [Leaf [Add, DivMod], Leaf [Inc, Double]],
                          1
                        ),
                        (Branch NoSplit [Leaf [Inc, Double]], 2)
                      ]
                      [IntType]
                  )
                ]
        let componentProg ::
              SymbolTable
                (Prog (Union TestSemanticsOp) SymInteger TestSemanticsType)
            componentProg =
              SymbolTable
                [ ( "prog",
                    Prog
                      { progArgList =
                          [ ProgArg
                              { progArgName = "arg0",
                                progArgType = IntType
                              },
                            ProgArg
                              { progArgName = "arg1",
                                progArgType = IntType
                              }
                          ],
                        progStmtList =
                          [ Stmt
                              { stmtOp =
                                  mrgIf
                                    (isym "prog" 0)
                                    (return Add)
                                    ( mrgIf
                                        (isym "prog" 1)
                                        (return DivMod)
                                        ( mrgIf
                                            (isym "prog" 2)
                                            (return Inc)
                                            (return Double)
                                        )
                                    ),
                                stmtArgIds = [isym "prog" 3, isym "prog" 4],
                                stmtArgNum = isym "prog" 5,
                                stmtResIds = [isym "prog" 6, isym "prog" 7],
                                stmtResNum = isym "prog" 8,
                                stmtDisabled = isym "prog" 9,
                                stmtMustBeAfter = []
                              },
                            Stmt
                              { stmtOp =
                                  mrgIf
                                    (isym "prog" 10)
                                    (return Inc)
                                    (return Double),
                                stmtArgIds = [isym "prog" 11],
                                stmtArgNum = isym "prog" 12,
                                stmtResIds = [isym "prog" 13],
                                stmtResNum = isym "prog" 14,
                                stmtDisabled = isym "prog" 15,
                                stmtMustBeAfter = []
                              },
                            Stmt
                              { stmtOp =
                                  mrgIf
                                    (isym "prog" 16)
                                    (return Inc)
                                    (return Double),
                                stmtArgIds = [isym "prog" 17],
                                stmtArgNum = isym "prog" 18,
                                stmtResIds = [isym "prog" 19],
                                stmtResNum = isym "prog" 20,
                                stmtDisabled =
                                  isym "prog" 21 .|| isym "prog" 15,
                                stmtMustBeAfter = [isym "prog" 13]
                              }
                          ],
                        progResList =
                          [ ProgRes
                              { progResId = isym "prog" 22,
                                progResType = IntType
                              }
                          ]
                      }
                  )
                ]
        genSymSimple bagTable "prog" .@?= componentProg
    ]
