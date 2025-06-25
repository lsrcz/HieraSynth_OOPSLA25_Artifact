{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Choice.ChoiceTreeTest (choiceTreeTest) where

import qualified Data.Text as T
import Grisette
  ( PPrint (pformat),
    Solvable (isym),
    Union,
    genSym,
    genSymSimple,
    mrgIf,
    mrgSingle,
  )
import HieraSynth.Program.Choice.ChoiceTree
  ( ChoiceMeta (NoSplit, Split),
    ChoiceTree (Branch, Leaf),
  )
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
  )
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod, Double, Inc),
  )
import HieraSynth.Util.Pretty (renderDoc)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

choiceTreeTest :: Test
choiceTreeTest =
  testGroup
    "ChoiceTree"
    [ testGroup
        "lowestSeqNum"
        [ testCase "Leaf" $ do
            let leaf = Leaf [Add, DivMod, Inc, Double]
            lowestSeqNum True leaf @?= Nothing
            lowestSeqNum False leaf @?= Nothing,
          testCase "Branch no split" $ do
            let branch = Branch NoSplit [Leaf [Add, DivMod, Inc, Double]]
            lowestSeqNum True branch @?= Nothing
            lowestSeqNum False branch @?= Nothing,
          testCase "Branch split" $ do
            let branch onlyWhenSuccess =
                  Branch
                    (Split 1 onlyWhenSuccess)
                    [Leaf [Add, DivMod, Inc, Double]]
            lowestSeqNum True (branch True) @?= Just 1
            lowestSeqNum False (branch True) @?= Nothing
            lowestSeqNum True (branch False) @?= Just 1
            lowestSeqNum False (branch False) @?= Just 1,
          testCase "Branch nested" $ do
            let branch =
                  Branch
                    NoSplit
                    [ Branch
                        (Split 3 False)
                        [ Branch
                            (Split 0 True)
                            [ Branch
                                (Split 1 False)
                                [Leaf [Add, DivMod, Inc, Double]]
                            ]
                        ],
                      Branch (Split 2 False) [Leaf [Add, DivMod, Inc, Double]]
                    ]
            lowestSeqNum True branch @?= Just 0
            lowestSeqNum False branch @?= Just 1
        ],
      testGroup
        "partitionSpec"
        [ testCase "Leaf" $ do
            let leaf = Leaf [Add, DivMod, Inc, Double]
            partitionSpec 0 leaf @?= [leaf],
          testCase "NoSplit" $ do
            let branch = Branch NoSplit [Leaf [Add, DivMod, Inc, Double]]
            partitionSpec 0 branch @?= [branch],
          testCase "Unmatched seq num" $ do
            let branch =
                  Branch
                    (Split 1 False)
                    [Leaf [Add, DivMod, Inc, Double]]
            partitionSpec 0 branch @?= [branch],
          testCase "Matched seq num" $ do
            let branch =
                  Branch
                    (Split 1 False)
                    [Leaf [Add, DivMod], Leaf [Inc], Leaf [Double]]
            let result =
                  [ Branch NoSplit [Leaf [Add, DivMod]],
                    Branch NoSplit [Leaf [Inc]],
                    Branch NoSplit [Leaf [Double]]
                  ]
            partitionSpec 1 branch @?= result,
          testCase "Nested" $ do
            let branch =
                  Branch
                    NoSplit
                    [ Branch
                        (Split 0 False)
                        [ Branch
                            (Split 1 False)
                            [ Branch
                                (Split 0 False)
                                [Leaf [Add, DivMod], Leaf [Inc, Double]]
                            ],
                          Leaf [Inc]
                        ],
                      Branch (Split 2 False) [Leaf [Add, DivMod, Inc, Double]]
                    ]
            partitionSpec 0 branch
              @?= [ Branch
                      NoSplit
                      [ Branch
                          NoSplit
                          [ Branch
                              (Split 1 False)
                              [Branch NoSplit [Leaf [Add, DivMod]]]
                          ],
                        Branch (Split 2 False) [Leaf [Add, DivMod, Inc, Double]]
                      ],
                    Branch
                      NoSplit
                      [ Branch
                          NoSplit
                          [ Branch
                              (Split 1 False)
                              [Branch NoSplit [Leaf [Inc, Double]]]
                          ],
                        Branch (Split 2 False) [Leaf [Add, DivMod, Inc, Double]]
                      ],
                    Branch
                      NoSplit
                      [ Branch NoSplit [Leaf [Inc]],
                        Branch (Split 2 False) [Leaf [Add, DivMod, Inc, Double]]
                      ]
                  ]
        ],
      testGroup
        "PPrint"
        [ testCase "Leaf single" $ do
            let leaf = Leaf [Add]
            renderDoc 80 (pformat leaf) @?= "Add"
            renderDoc 0 (pformat leaf) @?= "Add",
          testCase "Leaf multiple" $ do
            let leaf = Leaf [Add, DivMod, Inc, Double]
            renderDoc 80 (pformat leaf) @?= "<Add, DivMod, Inc, Double>"
            renderDoc 0 (pformat leaf)
              @?= T.intercalate
                "\n"
                [ "<",
                  "  Add,",
                  "  DivMod,",
                  "  Inc,",
                  "  Double",
                  ">"
                ],
          testCase "Branch no split single" $ do
            let branch = Branch NoSplit [Leaf [Add, DivMod, Inc, Double]]
            renderDoc 80 (pformat branch) @?= "<Add, DivMod, Inc, Double>"
            renderDoc 0 (pformat branch)
              @?= T.intercalate
                "\n"
                [ "<",
                  "  Add,",
                  "  DivMod,",
                  "  Inc,",
                  "  Double",
                  ">"
                ],
          testCase "Branch no split multiple" $ do
            let branch =
                  Branch
                    NoSplit
                    [ Leaf [Add, DivMod],
                      Leaf [Inc],
                      Leaf [Double]
                    ]
            renderDoc 80 (pformat branch) @?= "<<Add, DivMod>, Inc, Double>"
            renderDoc 0 (pformat branch)
              @?= T.intercalate
                "\n"
                [ "<",
                  "  <",
                  "    Add,",
                  "    DivMod",
                  "  >,",
                  "  Inc,",
                  "  Double",
                  ">"
                ],
          testCase "Branch split single" $ do
            let branch =
                  Branch
                    (Split 1 False)
                    [Leaf [Add, DivMod, Inc, Double]]
            renderDoc 80 (pformat branch) @?= "{<Add, DivMod, Inc, Double>}@1"
            renderDoc 0 (pformat branch)
              @?= T.intercalate
                "\n"
                [ "{",
                  "  <",
                  "    Add,",
                  "    DivMod,",
                  "    Inc,",
                  "    Double",
                  "  >",
                  "}@1"
                ],
          testCase "Branch split multiple" $ do
            let branch =
                  Branch
                    (Split 1 False)
                    [Leaf [Add, DivMod], Leaf [Inc, Double]]
            renderDoc 80 (pformat branch) @?= "{<Add, DivMod>, <Inc, Double>}@1"
            renderDoc 0 (pformat branch)
              @?= T.intercalate
                "\n"
                [ "{",
                  "  <",
                  "    Add,",
                  "    DivMod",
                  "  >,",
                  "  <",
                  "    Inc,",
                  "    Double",
                  "  >",
                  "}@1"
                ],
          testCase "Branch split multiple onlyWhenSuccess" $ do
            let branch =
                  Branch
                    (Split 1 True)
                    [Leaf [Add, DivMod], Leaf [Inc, Double]]
            renderDoc 80 (pformat branch)
              @?= "{<Add, DivMod>, <Inc, Double>}@1s"
            renderDoc 0 (pformat branch)
              @?= T.intercalate
                "\n"
                [ "{",
                  "  <",
                  "    Add,",
                  "    DivMod",
                  "  >,",
                  "  <",
                  "    Inc,",
                  "    Double",
                  "  >",
                  "}@1s"
                ]
        ],
      testCase "GenSym" $ do
        let tree =
              Branch (Split 1 False) [Leaf [Add, DivMod], Leaf [Inc, Double]]
        let expected =
              mrgIf
                (isym "a" 0)
                (return Add)
                ( mrgIf
                    (isym "a" 1)
                    (return DivMod)
                    (mrgIf (isym "a" 2) (return Inc) (return Double))
                )
        let value = genSym tree "a" :: Union TestSemanticsOp
        value .@?= expected
        let value = genSym tree "a" :: Union (Union TestSemanticsOp)
        value .@?= mrgSingle expected
        let value = genSymSimple tree "a" :: Union TestSemanticsOp
        value .@?= expected
        let value = genSymSimple tree "a" :: Union (Union TestSemanticsOp)
        value .@?= mrgSingle expected
    ]
