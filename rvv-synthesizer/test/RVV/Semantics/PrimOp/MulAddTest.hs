{-# LANGUAGE DataKinds #-}

module RVV.Semantics.PrimOp.MulAddTest (mulAddTest) where

import Grisette (BV (bv, bvSext), LogicalOp (false), mrgReturn)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (tuPolicy)
import RVV.Semantics.PrimOp.MulAdd (mulAdd)
import RVV.Semantics.Value
  ( Mask (Mask),
    VL (VL),
    Vector (Vector),
    VectorReg (VectorReg),
    VLValue (VLNum)
  )
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigE1M4, vectorConfigEF2M2)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

mulAddTest :: Test
mulAddTest =
  testGroup
    "MulAdd"
    [ testCase "SingleWidthMulAdd" $ do
        let actual =
              mulAdd
                id
                id
                (\dest lhs rhs -> mrgReturn $ dest + lhs * rhs)
                vconst8162
                vectorConfigEF2M2
                vectorConfigEF2M2
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x4321) (bv 16 0),
                      VectorReg (bv 16 0x8765) (bv 16 0x0f00)
                    ]
                )
                (Mask 4 $ VectorReg (bv 16 0x0067) (bv 16 0))
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0x2345) (bv 16 0x0000)
                    ]
                )
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                      VectorReg (bv 16 0x4cf3) (bv 16 0x0000)
                    ]
                )
                (VL 4 (VLNum (bv 8 0x7)) tuPolicy false :: VL 'C)
        let expected =
              Right
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x0d09) (bv 16 0xf000),
                      VectorReg (bv 16 0x8020) (bv 16 0x0f0f)
                    ]
                )
        actual @?= expected,
      testCase "WideningMulAdd" $ do
        let actual =
              mulAdd
                (bvSext 8)
                (bvSext 8)
                (\dest lhs rhs -> mrgReturn $ dest + lhs * rhs)
                vconst8162
                vectorConfigEF2M2
                vectorConfigE1M4
                ( Vector
                    vectorConfigE1M4
                    [ VectorReg (bv 16 0x4321) (bv 16 0),
                      VectorReg (bv 16 0x8765) (bv 16 0),
                      VectorReg (bv 16 0xcba9) (bv 16 0),
                      VectorReg (bv 16 0x0fed) (bv 16 0xff)
                    ]
                )
                (Mask 4 $ VectorReg (bv 16 0x0067) (bv 16 0))
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0x2345) (bv 16 0x0000)
                    ]
                )
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                      VectorReg (bv 16 0x4cf3) (bv 16 0x0000)
                    ]
                )
                (VL 4 (VLNum (bv 8 0x7)) tuPolicy false :: VL 'C)
        let expected =
              Right
                ( Vector
                    vectorConfigE1M4
                    [ VectorReg (bv 16 0x3119) (bv 16 0x0000),
                      VectorReg (bv 16 0x006f) (bv 16 0xff00),
                      VectorReg (bv 16 0xc700) (bv 16 0x00ff),
                      VectorReg (bv 16 0x0f00) (bv 16 0x00ff)
                    ]
                )
        actual @?= expected
    ]
