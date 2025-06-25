{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Concrete.ProgUtilTest (progUtilTest) where

import qualified Data.Text as T
import Grisette (Solvable (con))
import HieraSynth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.Program.ProgUtil
  ( ProgUtilImpl (getProgArgIds, getProgNumStmts, getProgResIds, getProgStmtAtIdx),
    StmtUtilImpl (getStmtArgIds, getStmtDisabled, getStmtOp, getStmtResIds),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

prog :: Prog T.Text Int T.Text
prog =
  Prog
    [ProgArg "a" 1 "ta", ProgArg "b" 2 "tb"]
    [ Stmt "s1" [1] [3],
      Stmt "s2" [2] [4]
    ]
    [ProgRes 3 "r1", ProgRes 4 "r2"]

stmt :: Stmt T.Text Int
stmt = Stmt "s1" [1] [3]

progUtilTest :: Test
progUtilTest =
  testGroup
    "ProgUtil"
    [ testGroup
        "ProgUtil"
        [ testCase "getProgArgIds" $ getProgArgIds prog @?= [1, 2],
          testCase "getProgResIds" $ getProgResIds prog @?= [3, 4],
          testCase "getProgNumStmts" $ getProgNumStmts prog @?= 2,
          testCase "getProgStmtAtIdx" $
            getProgStmtAtIdx prog 0 @?= Right (Stmt "s1" [1] [3]),
          testCase "getProgStmtAtIdx" $
            getProgStmtAtIdx prog 1 @?= Right (Stmt "s2" [2] [4])
        ],
      testGroup
        "StmtUtil"
        [ testCase "getStmtArgIds" $ getStmtArgIds stmt @?= [1],
          testCase "getStmtResIds" $ getStmtResIds stmt @?= [3],
          testCase "getStmtOp" $ getStmtOp stmt @?= "s1",
          testCase "getStmtDisabled" $ getStmtDisabled stmt .@?= con False
        ]
    ]
