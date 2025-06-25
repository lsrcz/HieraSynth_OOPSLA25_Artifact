{-# LANGUAGE DataKinds #-}

module RVV.Semantics.PrimOp.ElementWiseTest (elementWiseTest) where

import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (tuPolicy)
import RVV.Semantics.PrimOp.ElementWise (elementWise)
import RVV.Semantics.Value (Mask (Mask), VL (VL), VLValue (VLNum), Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigE1M8, vectorConfigEF2M4)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (Add),
    interpretSingleWidthBinaryOpCode,
  )
import RVV.Synthesizer.Parameter.WideningIntBinaryOpCode
  ( WideningIntBinaryOpCode (WAdd),
    interpretWideningIntBinaryOpCode,
    wideningIntBinaryOpCodeWidenLhs,
    wideningIntBinaryOpCodeWidenRhs,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

elementWiseTest :: Test
elementWiseTest =
  testGroup
    "ElementWise"
    [ testCase "SingleWidthElementWise" $ do
        let actual =
              elementWise
                id
                id
                id
                (\l r -> Right $ interpretSingleWidthBinaryOpCode Add l r)
                vconst8162
                vectorConfigEF2M4
                vectorConfigEF2M4
                vectorConfigEF2M4
                ( Vector
                    vectorConfigEF2M4
                    [ VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000)
                    ]
                )
                (Mask 8 $ VectorReg (bv 16 0x567f) (bv 16 0x0000))
                ( Vector
                    vectorConfigEF2M4
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0x2345) (bv 16 0x0000),
                      VectorReg (bv 16 0x3456) (bv 16 0x0000),
                      VectorReg (bv 16 0x4567) (bv 16 0x0000)
                    ]
                )
                ( Vector
                    vectorConfigEF2M4
                    [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                      VectorReg (bv 16 0x4cf3) (bv 16 0x0000),
                      VectorReg (bv 16 0x26ab) (bv 16 0x0000),
                      VectorReg (bv 16 0x0589) (bv 16 0x0000)
                    ]
                )
                (VL 8 (VLNum (bv 8 0x9 :: SomeWordN)) tuPolicy false :: VL 'C)
        let expected =
              Right
                ( Vector
                    vectorConfigEF2M4
                    [ VectorReg (bv 16 0x27d2) (bv 16 0x0000),
                      VectorReg (bv 16 0x0f38) (bv 16 0xf000),
                      VectorReg (bv 16 0xccc0) (bv 16 0x000f),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000)
                    ]
                )
        actual @?= expected,
      testCase "wideningElementWise" $ do
        let actual =
              elementWise
                (wideningIntBinaryOpCodeWidenLhs WAdd)
                (wideningIntBinaryOpCodeWidenRhs WAdd)
                id
                (\l r -> Right $ interpretWideningIntBinaryOpCode WAdd l r)
                vconst8162
                vectorConfigEF2M4
                vectorConfigEF2M4
                vectorConfigE1M8
                ( Vector
                    vectorConfigE1M8
                    [ VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000)
                    ]
                )
                (Mask 8 $ VectorReg (bv 16 0x56ef) (bv 16 0x0000))
                ( Vector
                    vectorConfigEF2M4
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0xabcd) (bv 16 0x0000),
                      VectorReg (bv 16 0x3456) (bv 16 0x0000),
                      VectorReg (bv 16 0x4567) (bv 16 0x0000)
                    ]
                )
                ( Vector
                    vectorConfigEF2M4
                    [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                      VectorReg (bv 16 0x4cf3) (bv 16 0x0000),
                      VectorReg (bv 16 0x26ab) (bv 16 0x0000),
                      VectorReg (bv 16 0x0589) (bv 16 0x0000)
                    ]
                )
                (VL 8 (VLNum (bv 8 0x9 :: SomeWordN)) tuPolicy false :: VL 'C)
        let expected =
              Right
                ( Vector
                    vectorConfigE1M8
                    [ VectorReg (bv 16 0xfd02) (bv 16 0x0000),
                      VectorReg (bv 16 0x0207) (bv 16 0x0000),
                      VectorReg (bv 16 0xfb00) (bv 16 0x00ff),
                      VectorReg (bv 16 0xfef7) (bv 16 0x0000),
                      VectorReg (bv 16 0xcc00) (bv 16 0x00ff),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000),
                      VectorReg (bv 16 0xcccc) (bv 16 0x0000)
                    ]
                )
        actual @?= expected
    ]
