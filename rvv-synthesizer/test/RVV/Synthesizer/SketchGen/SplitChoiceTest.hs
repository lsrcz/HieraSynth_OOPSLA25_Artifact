{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.Synthesizer.SketchGen.SplitChoiceTest (splitChoiceTest) where

{-
import Data.Ratio ((%))
import Grisette (Solvable (con), WordN)
import qualified HieraSynth.Program.Concrete as Concrete
import Grisette.Unified (EvalModeTag (Sym))
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.VectorConfigConstants (vtypeEF2M1)
import RVV.Synthesizer.Parameter.Destination (Destination (UseProvidedDest))
import RVV.Synthesizer.ElementOp (SingleWidthBinOp (Add, And, Mul, Or, Sub))
import RVV.Synthesizer.Parameter.Masking (Masking (UseProvidedMask))
import RVV.Synthesizer.SketchGen.Sketch
  ( SketchOp
      ( SketchElementWiseMI,
        SketchElementWiseMM,
        SketchElementWiseMX,
        SketchNestedConSketch,
        SketchNestedSketch,
        SketchSingleWidthVV,
        SketchSingleWidthVX,
        SketchVSetVL
      ),
    SketchSpec (SketchSpec),
  )
import RVV.Synthesizer.SketchGen.SplitChoice
  ( Position (Position),
    conSketchMaxScorePosition,
    splitConSketchAtMaxScore,
    splitOpList,
    splitSketchAtMaxScore,
  )
import RVV.Synthesizer.Type (ValueType (VectorType))
-}
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

{-
data MaxScoreConSketchTest = MaxScoreConSketchTest
  { name :: String,
    sketch :: Concrete.Prog [SketchOp (WordN 8)] (WordN 8) (ValueType 'Sym),
    expected :: (Int, Position)
  }
  -}

splitChoiceTest :: Test
splitChoiceTest =
  testGroup
    "SplitChoiceTest"
    []

{-
splitChoiceTest :: Test
splitChoiceTest =
  testGroup
    "SplitChoiceTest"
    [ testGroup "MaxScoreConSketch" $ do
        MaxScoreConSketchTest {..} <-
          [ let getOp op =
                  op vtypeEF2M1 [Add, Sub, Mul] [UseProvidedDest] [UseProvidedMask]
             in MaxScoreConSketchTest
                  { name = "Fully concrete",
                    sketch =
                      Concrete.Prog
                        "test"
                        []
                        [ Concrete.Stmt [getOp SketchSingleWidthVV] [] [],
                          Concrete.Stmt
                            [ getOp SketchSingleWidthVV,
                              getOp SketchSingleWidthVX
                            ]
                            []
                            []
                        ]
                        [],
                    expected = (6, Position 1 Nothing)
                  },
            let inner =
                  SketchSpec
                    "inner"
                    []
                    [ ([SketchSingleWidthVV vtypeEF2M1 [Add] [UseProvidedDest] [UseProvidedMask]], 1),
                      ( [SketchSingleWidthVV vtypeEF2M1 [Add, Sub] [UseProvidedDest] [UseProvidedMask]],
                        2
                      )
                    ]
                    []
             in MaxScoreConSketchTest
                  { name = "Sketch in concrete",
                    sketch =
                      Concrete.Prog
                        "test"
                        []
                        [ Concrete.Stmt
                            [SketchSingleWidthVX vtypeEF2M1 [Add] [UseProvidedDest] [UseProvidedMask]]
                            []
                            [],
                          Concrete.Stmt
                            [ SketchSingleWidthVX vtypeEF2M1 [Add] [UseProvidedDest] [UseProvidedMask],
                              SketchNestedSketch inner
                            ]
                            []
                            []
                        ]
                        [],
                    expected = (4, Position 1 (Just (1, Position 1 Nothing)))
                  },
            let oplist = [SketchSingleWidthVV vtypeEF2M1 [Add, Sub] [UseProvidedDest] [UseProvidedMask]]
                inner =
                  SketchSpec
                    "inner"
                    []
                    [ ( [ SketchNestedSketch $
                            SketchSpec "inner" [] [(oplist, 3)] []
                        ],
                        1
                      ),
                      (oplist, 2)
                    ]
                    []
             in MaxScoreConSketchTest
                  { name = "Sketch in sketch in concrete",
                    sketch =
                      Concrete.Prog
                        "test"
                        []
                        [ Concrete.Stmt
                            [SketchSingleWidthVX vtypeEF2M1 [Add] [UseProvidedDest] [UseProvidedMask]]
                            []
                            [],
                          Concrete.Stmt [SketchNestedSketch inner] [] []
                        ]
                        [],
                    expected =
                      ( 6,
                        Position
                          1
                          (Just (0, Position 0 (Just (0, Position 0 Nothing))))
                      )
                  }
            ]
        return $ testCase name $ do
          let actual = conSketchMaxScorePosition sketch
          actual @?= expected,
      testGroup
        "splitOpList"
        [ testCase "SingleOp" $ do
            let actual =
                  splitOpList
                    [ SketchElementWiseMM (1 % 2) 4 [Add, Sub] ::
                        SketchOp (WordN 8)
                    ]
            let expected =
                  ( [SketchElementWiseMM (1 % 2) 4 [Add]],
                    [SketchElementWiseMM (1 % 2) 4 [Sub]]
                  )
            actual @?= expected,
          testCase "SingleOp three choices" $ do
            let actual =
                  splitOpList
                    [ SketchElementWiseMM (1 % 2) 4 [Add, Sub, Mul] ::
                        SketchOp (WordN 8)
                    ]
            let expected =
                  ( [SketchElementWiseMM (1 % 2) 4 [Add, Sub]],
                    [SketchElementWiseMM (1 % 2) 4 [Mul]]
                  )
            actual @?= expected,
          testCase "MultipleOp exact" $ do
            let actual =
                  splitOpList
                    [ SketchElementWiseMM (1 % 2) 4 [Add, Sub],
                      SketchElementWiseMX (1 % 2) 4 [Add, Sub],
                      SketchElementWiseMI
                        (1 % 2)
                        4
                        [Add, Sub, And, Or]
                        Nothing ::
                        SketchOp (WordN 8)
                    ]
            let expected =
                  ( [ SketchElementWiseMM (1 % 2) 4 [Add, Sub],
                      SketchElementWiseMX (1 % 2) 4 [Add, Sub]
                    ],
                    [ SketchElementWiseMI
                        (1 % 2)
                        4
                        [Add, Sub, And, Or]
                        Nothing
                    ]
                  )
            actual @?= expected,
          testCase "MultipleOp middle goes to left" $ do
            let actual =
                  splitOpList
                    [ SketchElementWiseMM (1 % 2) 4 [Add, Sub],
                      SketchElementWiseMX (1 % 2) 4 [Add, Sub],
                      SketchElementWiseMI
                        (1 % 2)
                        4
                        [Add, Sub, And]
                        Nothing ::
                        SketchOp (WordN 8)
                    ]
            let expected =
                  ( [ SketchElementWiseMM (1 % 2) 4 [Add, Sub],
                      SketchElementWiseMX (1 % 2) 4 [Add, Sub]
                    ],
                    [ SketchElementWiseMI
                        (1 % 2)
                        4
                        [Add, Sub, And]
                        Nothing
                    ]
                  )
            actual @?= expected,
          testCase "MultipleOp middle single choice" $ do
            let actual =
                  splitOpList
                    [ SketchElementWiseMM (1 % 2) 4 [Add, Sub],
                      SketchElementWiseMX (1 % 2) 4 [Add],
                      SketchElementWiseMI
                        (1 % 2)
                        4
                        [Add, Sub]
                        Nothing ::
                        SketchOp (WordN 8)
                    ]
            let expected =
                  ( [ SketchElementWiseMM (1 % 2) 4 [Add, Sub],
                      SketchElementWiseMX (1 % 2) 4 [Add]
                    ],
                    [ SketchElementWiseMI
                        (1 % 2)
                        4
                        [Add, Sub]
                        Nothing
                    ]
                  )
            actual @?= expected
        ],
      testGroup "splitConSketchAtMaxScore" $ do
        SplitConSketchAtMaxScoreTest {..} <-
          concat
            [ let getSketch opList =
                    Concrete.Prog
                      "test"
                      [ Concrete.ProgArg
                          "a"
                          0
                          (VectorType (con False) vtypeEF2M1)
                      ]
                      [ Concrete.Stmt
                          [SketchSingleWidthVV vtypeEF2M1 opList [UseProvidedDest] [UseProvidedMask]]
                          [0, 0]
                          [1]
                      ]
                      [Concrete.ProgRes 1 (VectorType (con False) vtypeEF2M1)]
               in [ SplitConSketchAtMaxScoreTest
                      { name = "In Concrete, single op, no split produced",
                        sketch = getSketch [Add],
                        expected = []
                      },
                    SplitConSketchAtMaxScoreTest
                      { name = "In Concrete",
                        sketch = getSketch [Add, Sub],
                        expected = [getSketch [Add], getSketch [Sub]]
                      },
                    SplitConSketchAtMaxScoreTest
                      { name = "In Concrete, three ops",
                        sketch = getSketch [Add, Sub, Mul],
                        expected = [getSketch [Add, Sub], getSketch [Mul]]
                      }
                  ],
              let getSketch opList =
                    Concrete.Prog
                      "test"
                      [ Concrete.ProgArg
                          "a"
                          0
                          (VectorType (con False) vtypeEF2M1)
                      ]
                      [ Concrete.Stmt
                          [SketchSingleWidthVV vtypeEF2M1 [Add] [UseProvidedDest] [UseProvidedMask]]
                          [0, 0]
                          [1],
                        Concrete.Stmt
                          [SketchSingleWidthVV vtypeEF2M1 opList [UseProvidedDest] [UseProvidedMask]]
                          [0, 1]
                          [2],
                        Concrete.Stmt
                          [SketchSingleWidthVV vtypeEF2M1 [Add] [UseProvidedDest] [UseProvidedMask]]
                          [1, 2]
                          [3]
                      ]
                      [Concrete.ProgRes 3 (VectorType (con False) vtypeEF2M1)]
               in [ SplitConSketchAtMaxScoreTest
                      { name = "In Concrete, multiple ops",
                        sketch = getSketch [Add, Sub, Mul],
                        expected = [getSketch [Add, Sub], getSketch [Mul]]
                      }
                  ],
              let constructOpList (ops, n) =
                    ([SketchSingleWidthVV vtypeEF2M1 ops [UseProvidedDest] [UseProvidedMask]], n)
                  getSketch opsListWithN =
                    Concrete.Prog
                      "test"
                      [ Concrete.ProgArg
                          "a"
                          0
                          (VectorType (con False) vtypeEF2M1)
                      ]
                      [ Concrete.Stmt
                          [ SketchNestedSketch $
                              SketchSpec
                                "inner"
                                [VectorType (con False) vtypeEF2M1]
                                ( ([SketchVSetVL 4 [nonePolicy]], 1)
                                    : (constructOpList <$> opsListWithN)
                                    ++ [([SketchVSetVL 4 [nonePolicy]], 1)]
                                )
                                [VectorType (con False) vtypeEF2M1]
                          ]
                          [0, 0]
                          [1]
                      ]
                      [Concrete.ProgRes 1 (VectorType (con False) vtypeEF2M1)]
               in [ SplitConSketchAtMaxScoreTest
                      { name = "In sub symbolic sketch in concrete",
                        sketch = getSketch [([Add, Sub, Mul], 3)],
                        expected =
                          [ getSketch [([Mul], 3)],
                            getSketch [([Add, Sub], 1), ([Mul], 2)],
                            getSketch [([Add, Sub], 2), ([Mul], 1)],
                            getSketch [([Add, Sub], 3)]
                          ]
                      }
                  ]
            ]
        return $ testCase name $ do
          let actual = splitConSketchAtMaxScore sketch
          actual @?= expected,
      testGroup "splitSketchAtMaxScore" $ do
        SplitSketchAtMaxScoreTest {..} <-
          concat
            [ let constructOpList (ops, n) =
                    ([SketchSingleWidthVV vtypeEF2M1 ops [UseProvidedDest] [UseProvidedMask]], n)
                  getSketch opsListWithN =
                    SketchSpec
                      "inner"
                      [VectorType (con False) vtypeEF2M1]
                      (constructOpList <$> opsListWithN)
                      [VectorType (con False) vtypeEF2M1]
               in [ SplitSketchAtMaxScoreTest
                      { name = "In Symbolic, single op, no split produced",
                        sketch = getSketch [([Add], 1)],
                        expected = []
                      },
                    SplitSketchAtMaxScoreTest
                      { name =
                          "In Symbolic, single op, no split produced, multiple",
                        sketch = getSketch [([Add], 2)],
                        expected = []
                      },
                    SplitSketchAtMaxScoreTest
                      { name = "In Symbolic",
                        sketch = getSketch [([Add, Sub], 2)],
                        expected =
                          [ getSketch [([Sub], 2)],
                            getSketch [([Add], 1), ([Sub], 1)],
                            getSketch [([Add], 2)]
                          ]
                      },
                    SplitSketchAtMaxScoreTest
                      { name = "In Symbolic, three ops",
                        sketch = getSketch [([Add, Mul, Sub], 2)],
                        expected =
                          [ getSketch [([Sub], 2)],
                            getSketch [([Add, Mul], 1), ([Sub], 1)],
                            getSketch [([Add, Mul], 2)]
                          ]
                      }
                  ],
              let constructOpList (ops, n) =
                    ([SketchSingleWidthVV vtypeEF2M1 ops [UseProvidedDest] [UseProvidedMask]], n)
                  getSketch opsListWithN =
                    SketchSpec
                      "inner"
                      [VectorType (con False) vtypeEF2M1]
                      ( ([SketchVSetVL 4 [nonePolicy]], 1)
                          : (constructOpList <$> opsListWithN)
                          ++ [([SketchVSetVL 4 [nonePolicy]], 1)]
                      )
                      [VectorType (con False) vtypeEF2M1]
               in [ SplitSketchAtMaxScoreTest
                      { name = "In Symbolic, three ops",
                        sketch = getSketch [([Add, Mul, Sub], 2)],
                        expected =
                          [ getSketch [([Sub], 2)],
                            getSketch [([Add, Mul], 1), ([Sub], 1)],
                            getSketch [([Add, Mul], 2)]
                          ]
                      }
                  ],
              let getConSketch opList =
                    Concrete.Prog
                      "test"
                      [ Concrete.ProgArg
                          "a"
                          0
                          (VectorType (con False) vtypeEF2M1)
                      ]
                      [ Concrete.Stmt
                          [SketchSingleWidthVV vtypeEF2M1 [Add] [UseProvidedDest] [UseProvidedMask]]
                          [0, 0]
                          [1],
                        Concrete.Stmt
                          [SketchSingleWidthVV vtypeEF2M1 opList [UseProvidedDest] [UseProvidedMask]]
                          [0, 1]
                          [2],
                        Concrete.Stmt
                          [SketchSingleWidthVV vtypeEF2M1 [Add] [UseProvidedDest] [UseProvidedMask]]
                          [1, 2]
                          [3]
                      ]
                      [Concrete.ProgRes 3 (VectorType (con False) vtypeEF2M1)]
                  getSketch opList =
                    SketchSpec
                      "inner"
                      [VectorType (con False) vtypeEF2M1]
                      [([SketchNestedConSketch $ getConSketch opList], 1)]
                      [VectorType (con False) vtypeEF2M1]
               in [ SplitSketchAtMaxScoreTest
                      { name = "In sub concrete sketch in symbolic",
                        sketch = getSketch [Add, Mul, Sub],
                        expected = [getSketch [Add, Mul], getSketch [Sub]]
                      }
                  ]
            ]
        return $ testCase name $ do
          let actual = splitSketchAtMaxScore sketch
          actual @?= expected
    ]

data SplitConSketchAtMaxScoreTest = SplitConSketchAtMaxScoreTest
  { name :: String,
    sketch ::
      Concrete.Prog [SketchOp (WordN 8)] (WordN 8) (ValueType 'Sym),
    expected ::
      [Concrete.Prog [SketchOp (WordN 8)] (WordN 8) (ValueType 'Sym)]
  }

data SplitSketchAtMaxScoreTest = SplitSketchAtMaxScoreTest
  { name :: String,
    sketch :: SketchSpec (WordN 8),
    expected :: [SketchSpec (WordN 8)]
  }
-}
