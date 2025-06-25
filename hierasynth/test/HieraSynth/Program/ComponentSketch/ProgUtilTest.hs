{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ComponentSketch.ProgUtilTest
  ( progUtilTest,
  )
where

import qualified Data.Text as T
import Grisette (SymInteger)
import HieraSynth.Program.ComponentSketch
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
import Test.SymbolicAssertion ((.@?=))

prog :: Prog T.Text SymInteger T.Text
prog =
  Prog
    [ProgArg "a" "ta", ProgArg "b" "tb"]
    [ Stmt "s1" [0] 1 [2] 1 "d1" [],
      Stmt "s2" [1] 1 [3] 1 "d2" []
    ]
    [ProgRes 2 "r1", ProgRes 3 "r2"]

stmt :: Stmt T.Text Int
stmt = Stmt "s1" [0] 1 [2] 1 "d1" []

progUtilTest :: Test
progUtilTest =
  testGroup
    "ProgUtil"
    [ testGroup
        "ProgUtil"
        [ testCase "getProgArgIds" $ getProgArgIds prog .@?= [0, 1],
          testCase "getProgResIds" $ getProgResIds prog .@?= [2, 3],
          testCase "getProgNumStmts" $ getProgNumStmts prog .@?= 2,
          testCase "getProgStmtAtIdx" $
            getProgStmtAtIdx prog 0 .@?= Right (Stmt "s1" [0] 1 [2] 1 "d1" []),
          testCase "getProgStmtAtIdx" $
            getProgStmtAtIdx prog 1 .@?= Right (Stmt "s2" [1] 1 [3] 1 "d2" [])
        ],
      testGroup
        "StmtUtil"
        [ testCase "getStmtArgIds" $ getStmtArgIds stmt .@?= [0],
          testCase "getStmtResIds" $ getStmtResIds stmt .@?= [2],
          testCase "getStmtOp" $ getStmtOp stmt .@?= "s1",
          testCase "getStmtDisabled" $ getStmtDisabled stmt .@?= "d1"
        ]
    ]
