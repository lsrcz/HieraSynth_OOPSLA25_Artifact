{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.InterpreterTest (interpreterTest) where

import Grisette
  ( BV (bv, bvConcat),
    LogicalOp (false),
    WordN,
    ssymBV,
  )
import Grisette.Lib.Base (mrgReturn)
import HieraSynth.Context (ConcreteContext, SymbolicContext)
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Unified (EvalModeTag (C, S))
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.Value
  ( Scalar (Scalar),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigEF2M2)
import RVV.Synthesizer.Op
  ( ConOp,
  )
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (VectorRHS),
  )
import RVV.Synthesizer.Operator.SetVectorLength (setVectorLength)
import RVV.Synthesizer.Operator.SingleWidthIntBinary (singleWidthIntBinary)
import RVV.Synthesizer.Parameter.Destination (ud)
import RVV.Synthesizer.Parameter.Masking (fm)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode (mkAdd)
import RVV.Synthesizer.Type (ValueType (ScalarType, VectorType))
import RVV.Synthesizer.Value (Value (ScalarValue, VectorValue))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import TestUtil.SymbolicAssertion ((.@?=))

-- add(vint4m2_t lhs, vint4m2_t rhs, int8_t avl) {
-- vsetvli t0, avl, e4, m2, tu
-- vadd.vv v2, lhs, rhs -- v2 starts with undefined value
addProg :: Concrete.Prog ConOp (WordN 8) ValueType
addProg =
  Concrete.Prog
    [ Concrete.ProgArg "lhs" 0 (VectorType vectorConfigEF2M2),
      Concrete.ProgArg "rhs" 1 (VectorType vectorConfigEF2M2),
      Concrete.ProgArg "avl" 2 (ScalarType 1)
    ]
    [ Concrete.Stmt (setVectorLength @'C 4 nonePolicy) [2] [3],
      Concrete.Stmt (singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd ud fm VectorRHS) [3, 0, 1] [4]
    ]
    [Concrete.ProgRes 4 (VectorType vectorConfigEF2M2)]

interpreterTest :: Test
interpreterTest =
  testGroup
    "Interpreter"
    [ testCase "Concrete program concrete inputs" $ do
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
        let actual :: ConcreteContext [Value 'C] =
              runProg
                vconst8162
                mempty
                addProg
                [lhs, rhs, ScalarValue $ Scalar (bv 8 5) false]
        let expected =
              Right
                [ VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9681) (bv 16 0),
                        VectorReg (bv 16 0x0007) (bv 16 0xfff0)
                      ]
                ]
        actual @?= expected,
      testCase "Concrete program symbolic inputs" $ do
        let lhs =
              VectorValue . Vector vectorConfigEF2M2 $
                [ VectorReg
                    (bvConcat (ssymBV 4 "x") (bv 12 0x234))
                    (bv 16 0),
                  VectorReg (bv 16 0xab3d) (bv 16 0)
                ]
        let rhs =
              VectorValue . Vector vectorConfigEF2M2 $
                [ VectorReg
                    (bvConcat (ssymBV 4 "y") (bv 12 0x845d))
                    (bv 16 0),
                  VectorReg (bv 16 0x2fda) (bv 16 0)
                ]
        let actual :: SymbolicContext [Value 'S] =
              runProg
                vconst8162
                mempty
                addProg
                [lhs, rhs, ScalarValue $ Scalar (bv 8 5) false]
        let expected =
              mrgReturn
                [ VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg
                          (bvConcat (ssymBV 4 "x" + ssymBV 4 "y") (bv 12 0x681))
                          (bv 16 0),
                        VectorReg (bv 16 0x0007) (bv 16 0xfff0)
                      ]
                ] ::
                SymbolicContext [Value 'S]
        actual .@?= expected
    ]
