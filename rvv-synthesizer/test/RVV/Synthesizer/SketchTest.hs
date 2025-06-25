{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.SketchTest (sketchTest) where

import Control.Monad.Except (runExceptT)
import Grisette
  ( BV (bv),
    LogicalOp (false),
    Solvable (con),
    SymEq ((./=), (.==)),
    SymWordN,
    runFreshT,
    simpleMerge,
    solve,
    z3,
  )
import HieraSynth.Context (SymbolicContext)
import HieraSynth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Unified (EvalModeTag (C, S))
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.Value (Scalar (Scalar), Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigEF2M2)
import RVV.Synthesizer.Op
  ( ConOp,
  )
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (VectorRHS),
  )
import RVV.Synthesizer.Operator.SetVectorLength (setVectorLength)
import RVV.Synthesizer.Operator.SingleWidthIntBinary
  ( singleWidthIntBinary,
  )
import RVV.Synthesizer.Parameter.Destination (ud)
import RVV.Synthesizer.Parameter.Masking (fm)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode (mkAdd)
import RVV.Synthesizer.Type (ValueType (ScalarType, VectorType))
import RVV.Synthesizer.Value (Value (ScalarValue, VectorValue))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

-- add(vint4m2_t lhs, vint4m2_t rhs, int8_t avl) {
-- vsetvli t0, avl, e4, m2, tu
-- vadd.vv v2, lhs, rhs -- v2 starts with undefined value
addProg :: Prog ConOp (SymWordN 8) ValueType
addProg =
  Prog
    [ ProgArg "lhs" (VectorType vectorConfigEF2M2),
      ProgArg "rhs" (VectorType vectorConfigEF2M2),
      ProgArg "avl" (ScalarType 1)
    ]
    [ Stmt
        (setVectorLength @'C 4 nonePolicy)
        [2]
        1
        [3]
        1
        (con False)
        [],
      Stmt
        (singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd ud fm VectorRHS)
        [3, 0, 1]
        3
        [4]
        1
        (con False)
        []
    ]
    [ProgRes 4 (VectorType vectorConfigEF2M2)]

sketchTest :: Test
sketchTest =
  testGroup
    "Sketch"
    [ testCase "ConcreteOp + component semantics" $ do
        let lhs =
              VectorValue . Vector vectorConfigEF2M2 $
                [ VectorReg (bv 16 0x1234) (bv 16 0),
                  VectorReg (bv 16 0xab3d) (bv 16 0)
                ]
        let rhs =
              VectorValue . Vector vectorConfigEF2M2 $
                [ VectorReg (bv 16 0x845d) (bv 16 0),
                  VectorReg (bv 16 0x2fda) (bv 16 0)
                ]
        let result :: SymbolicContext [Value 'S]
            result =
              flip runFreshT "x" $
                runProg
                  vconst8162
                  mempty
                  addProg
                  [lhs, rhs, ScalarValue $ Scalar (bv 8 5) false]
        let condition = simpleMerge $ do
              v <- runExceptT result
              case v of
                Left _ -> return $ con False
                Right r ->
                  return $
                    [ VectorValue
                        ( Vector
                            vectorConfigEF2M2
                            [ VectorReg (bv 16 0x9681) (bv 16 0),
                              VectorReg (bv 16 0x0007) (bv 16 0xfff0)
                            ]
                        )
                    ]
                      ./= r
        r <- solve z3 condition
        case r of
          Left _ -> return ()
          Right m -> fail $ "Can be not equal: " <> show m
        let conditionEqual = simpleMerge $ do
              v <- runExceptT result
              case v of
                Left _ -> return $ con False
                Right r ->
                  return $
                    [ VectorValue
                        ( Vector
                            vectorConfigEF2M2
                            [ VectorReg (bv 16 0x9681) (bv 16 0),
                              VectorReg (bv 16 0x0007) (bv 16 0xfff0)
                            ]
                        )
                    ]
                      .== r
        r <- solve z3 conditionEqual
        case r of
          Left err -> fail $ "Cannot be equal: " <> show err
          Right _ -> return ()
    ]
