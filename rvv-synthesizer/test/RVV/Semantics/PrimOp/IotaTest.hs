{-# LANGUAGE DataKinds #-}

module RVV.Semantics.PrimOp.IotaTest (iotaTest) where

import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (tuPolicy, tumuPolicy)
import RVV.Semantics.PrimOp.Iota (vid, viota)
import RVV.Semantics.Value (Mask (Mask), VL (VL), VLValue (VLNum), Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigEF2M2)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

iotaTest :: Test
iotaTest =
  testGroup
    "Iota"
    [ testCase "viota" $ do
        let actual =
              viota
                vconst8162
                vectorConfigEF2M2
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0xcccc) (bv 16 0xffff),
                      VectorReg (bv 16 0xcccc) (bv 16 0xffff)
                    ]
                )
                (Mask 4 $ VectorReg (bv 16 0x00ef) (bv 16 0))
                (Mask 4 $ VectorReg (bv 16 0x0063) (bv 16 0))
                (VL 4 (VLNum (bv 8 7)) tuPolicy false :: VL 'C)
        let expected =
              Right
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x2210 :: SomeWordN) (bv 16 0x0000),
                      VectorReg (bv 16 0xc320) (bv 16 0xf00f)
                    ]
                )
        actual @?= expected,
      testCase "viota-spec-example" $ do
        let actual =
              viota
                vconst8162
                vectorConfigEF2M2
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x6789) (bv 16 0),
                      VectorReg (bv 16 0x2345) (bv 16 0)
                    ]
                )
                (Mask 4 $ VectorReg (bv 16 0x00eb) (bv 16 0))
                (Mask 4 $ VectorReg (bv 16 0x0091) (bv 16 0))
                (VL 4 (VLNum (bv 8 8)) tumuPolicy false :: VL 'C)
        let expected =
              Right
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1710 :: SomeWordN) (bv 16 0),
                      VectorReg (bv 16 0x1115) (bv 16 0)
                    ]
                )
        actual @?= expected,
      testCase "vid" $ do
        let actual =
              vid
                vconst8162
                vectorConfigEF2M2
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0xcccc) (bv 16 0xffff),
                      VectorReg (bv 16 0xcccc) (bv 16 0xffff)
                    ]
                )
                (Mask 4 $ VectorReg (bv 16 0x00e9) (bv 16 0))
                (VL 4 (VLNum (bv 8 7)) tuPolicy false :: VL 'C)
        let expected =
              Right
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x3000 :: SomeWordN) (bv 16 0x0ff0),
                      VectorReg (bv 16 0xc650) (bv 16 0xf00f)
                    ]
                )
        actual @?= expected
    ]
