{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ComponentSketch.BuilderTest
  ( builderTest,
  )
where

import Grisette
  ( Fresh,
    GenSymSimple (simpleFresh),
    LogicalOp ((.||)),
    Solvable (isym),
    SymInteger,
    Union,
    chooseFresh,
    mrgIf,
    mrgReturn,
    runFresh,
  )
import HieraSynth.Program.ComponentSketch
  ( MkFreshProg (mkFreshProg),
    MkProg (mkProg),
    Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
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
    StmtExtraConstraint (StmtExtraConstraint, stmtMustBeAfterStmts),
    freshStmt,
    freshStmt',
    freshStmts',
  )
import HieraSynth.Program.ComponentSketch.Builder
  ( fromConcrete,
    simpleFreshStmt,
    simpleFreshStmt',
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.SymbolicAssertion ((.@?=))

builderTest :: Test
builderTest =
  testGroup
    "Builder"
    [ testCase "simpleFreshStmt" $ do
        let actual =
              simpleFreshStmt Add :: Fresh [Stmt TestSemanticsOp SymInteger]
        let expected =
              Stmt
                { stmtOp = Add,
                  stmtArgIds = [isym "x" 0, isym "x" 1],
                  stmtArgNum = isym "x" 2,
                  stmtResIds = [isym "x" 3],
                  stmtResNum = isym "x" 4,
                  stmtDisabled = isym "x" 5,
                  stmtMustBeAfter = []
                }
        runFresh actual "x" .@?= [expected],
      testCase "simpleFreshStmt'" $ do
        let actual = do
              precursor <-
                simpleFreshStmt DivMod ::
                  Fresh [Stmt TestSemanticsOp SymInteger]
              precursor2 <- simpleFreshStmt DivMod
              simpleFreshStmt'
                Add
                ( StmtExtraConstraint
                    { stmtMustBeAfterStmts = precursor ++ precursor2
                    }
                )
        let expected =
              Stmt
                { stmtOp = Add,
                  stmtArgIds = [isym "x" 14, isym "x" 15],
                  stmtArgNum = isym "x" 16,
                  stmtResIds = [isym "x" 17],
                  stmtResNum = isym "x" 18,
                  stmtDisabled = isym "x" 19,
                  stmtMustBeAfter =
                    [isym "x" 3, isym "x" 4, isym "x" 10, isym "x" 11]
                }
        runFresh actual "x" .@?= [expected],
      testCase "freshStmt" $ do
        let actual =
              freshStmt (chooseFresh [Add, DivMod]) ::
                Fresh [Stmt (Union TestSemanticsOp) SymInteger]
        let expected =
              Stmt
                { stmtOp = mrgIf (isym "x" 0) (return Add) (return DivMod),
                  stmtArgIds = [isym "x" 1, isym "x" 2],
                  stmtArgNum = isym "x" 3,
                  stmtResIds = [isym "x" 4, isym "x" 5],
                  stmtResNum = isym "x" 6,
                  stmtDisabled = isym "x" 7,
                  stmtMustBeAfter = []
                }
        runFresh actual "x" .@?= [expected],
      testCase "freshStmt'" $ do
        let actual = do
              precursor <-
                simpleFreshStmt $ mrgReturn DivMod ::
                  Fresh [Stmt (Union TestSemanticsOp) SymInteger]
              precursor2 <- simpleFreshStmt $ mrgReturn DivMod
              freshStmt'
                (chooseFresh [Add, DivMod])
                ( StmtExtraConstraint
                    { stmtMustBeAfterStmts = precursor ++ precursor2
                    }
                )
        let expected =
              Stmt
                { stmtOp = mrgIf (isym "x" 14) (return Add) (return DivMod),
                  stmtArgIds = [isym "x" 15, isym "x" 16],
                  stmtArgNum = isym "x" 17,
                  stmtResIds = [isym "x" 18, isym "x" 19],
                  stmtResNum = isym "x" 20,
                  stmtDisabled = isym "x" 21,
                  stmtMustBeAfter =
                    [isym "x" 3, isym "x" 4, isym "x" 10, isym "x" 11]
                }
        runFresh actual "x" .@?= [expected],
      testCase "freshStmts'" $ do
        let actual = do
              precursor <-
                simpleFreshStmt (mrgReturn DivMod) ::
                  Fresh [Stmt (Union TestSemanticsOp) SymInteger]
              precursor2 <- simpleFreshStmt (mrgReturn DivMod)
              freshStmts'
                3
                (chooseFresh [Add, DivMod])
                ( StmtExtraConstraint
                    { stmtMustBeAfterStmts = precursor ++ precursor2
                    }
                )
        let expected =
              [ Stmt
                  { stmtOp = mrgIf (isym "x" 14) (return Add) (return DivMod),
                    stmtArgIds = [isym "x" 15, isym "x" 16],
                    stmtArgNum = isym "x" 17,
                    stmtResIds = [isym "x" 18, isym "x" 19],
                    stmtResNum = isym "x" 20,
                    stmtDisabled = isym "x" 21,
                    stmtMustBeAfter =
                      [isym "x" 3, isym "x" 4, isym "x" 10, isym "x" 11]
                  },
                Stmt
                  { stmtOp = mrgIf (isym "x" 22) (return Add) (return DivMod),
                    stmtArgIds = [isym "x" 23, isym "x" 24],
                    stmtArgNum = isym "x" 25,
                    stmtResIds = [isym "x" 26, isym "x" 27],
                    stmtResNum = isym "x" 28,
                    stmtDisabled = isym "x" 29 .|| isym "x" 21,
                    stmtMustBeAfter =
                      [ isym "x" 3,
                        isym "x" 4,
                        isym "x" 10,
                        isym "x" 11,
                        isym "x" 18,
                        isym "x" 19
                      ]
                  },
                Stmt
                  { stmtOp = mrgIf (isym "x" 30) (return Add) (return DivMod),
                    stmtArgIds = [isym "x" 31, isym "x" 32],
                    stmtArgNum = isym "x" 33,
                    stmtResIds = [isym "x" 34, isym "x" 35],
                    stmtResNum = isym "x" 36,
                    stmtDisabled =
                      isym "x" 37 .|| (isym "x" 29 .|| isym "x" 21),
                    stmtMustBeAfter =
                      [ isym "x" 3,
                        isym "x" 4,
                        isym "x" 10,
                        isym "x" 11,
                        isym "x" 18,
                        isym "x" 19,
                        isym "x" 26,
                        isym "x" 27
                      ]
                  }
              ]
        runFresh actual "x" .@?= expected,
      testGroup
        "MkProg"
        [ testCase "Non-fresh" $ do
            let actual =
                  mkProg
                    [("x", IntType), ("y", IntType)]
                    [ Stmt Add ["a", "b"] "c" ["d"] "e" "f" [],
                      Stmt DivMod ["g", "h"] "i" ["j", "k"] "l" "m" []
                    ]
                    [("n", IntType), ("o", IntType)] ::
                    Prog TestSemanticsOp SymInteger TestSemanticsType
            let expected =
                  Prog
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt Add ["a", "b"] "c" ["d"] "e" "f" [],
                      Stmt DivMod ["g", "h"] "i" ["j", "k"] "l" "m" []
                    ]
                    [ProgRes "n" IntType, ProgRes "o" IntType]
            actual .@?= expected,
          testCase "fresh" $ do
            let actual =
                  flip runFresh "x" $
                    mkProg
                      [("x", IntType), ("y", IntType)]
                      [simpleFreshStmt Add, simpleFreshStmt DivMod]
                      [ (simpleFresh (), IntType),
                        (simpleFresh (), IntType)
                      ]
            let expected =
                  Prog
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt
                        Add
                        [isym "x" 0 :: SymInteger, isym "x" 1]
                        (isym "x" 2)
                        [isym "x" 3]
                        (isym "x" 4)
                        (isym "x" 5)
                        [],
                      Stmt
                        DivMod
                        [isym "x" 6, isym "x" 7]
                        (isym "x" 8)
                        [isym "x" 9, isym "x" 10]
                        (isym "x" 11)
                        (isym "x" 12)
                        []
                    ]
                    [ ProgRes (isym "x" 13) IntType,
                      ProgRes (isym "x" 14) IntType
                    ]
            actual .@?= expected
        ],
      testCase "MkFreshProg" $ do
        let actual =
              flip runFresh "x" $
                mkFreshProg
                  [IntType, IntType]
                  [simpleFreshStmt Add, simpleFreshStmt DivMod]
                  [IntType, IntType] ::
                Prog TestSemanticsOp SymInteger TestSemanticsType
        let expected =
              Prog
                [ProgArg "arg0" IntType, ProgArg "arg1" IntType]
                [ Stmt
                    Add
                    [isym "x" 0 :: SymInteger, isym "x" 1]
                    (isym "x" 2)
                    [isym "x" 3]
                    (isym "x" 4)
                    (isym "x" 5)
                    [],
                  Stmt
                    DivMod
                    [isym "x" 6, isym "x" 7]
                    (isym "x" 8)
                    [isym "x" 9, isym "x" 10]
                    (isym "x" 11)
                    (isym "x" 12)
                    []
                ]
                [ ProgRes (isym "x" 13) IntType,
                  ProgRes (isym "x" 14) IntType
                ]
        actual .@?= expected,
      testCase "fromConcrete" $ do
        let conProg =
              Concrete.Prog
                [Concrete.ProgArg "x" 0 IntType, Concrete.ProgArg "y" 1 IntType]
                [ Concrete.Stmt Add [0, 1] [2],
                  Concrete.Stmt DivMod [2, 0] [3, 4]
                ]
                [ Concrete.ProgRes 3 IntType,
                  Concrete.ProgRes 4 IntType,
                  Concrete.ProgRes 2 IntType
                ]
        let actual =
              fromConcrete conProg ::
                Fresh (Prog TestSemanticsOp SymInteger TestSemanticsType)
        let expected =
              Prog
                [ProgArg "x" IntType, ProgArg "y" IntType]
                [ Stmt
                    Add
                    [isym "x" 0, isym "x" 1]
                    (isym "x" 2)
                    [isym "x" 3]
                    (isym "x" 4)
                    (isym "x" 5)
                    [],
                  Stmt
                    DivMod
                    [isym "x" 6, isym "x" 7]
                    (isym "x" 8)
                    [isym "x" 9, isym "x" 10]
                    (isym "x" 11)
                    (isym "x" 12)
                    []
                ]
                [ ProgRes (isym "x" 13) IntType,
                  ProgRes (isym "x" 14) IntType,
                  ProgRes (isym "x" 15) IntType
                ]
        runFresh actual "x" .@?= expected
    ]
