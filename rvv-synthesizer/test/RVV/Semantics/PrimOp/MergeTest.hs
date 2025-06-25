{-# LANGUAGE DataKinds #-}

module RVV.Semantics.PrimOp.MergeTest (mergeTest) where

import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (tuPolicy)
import RVV.Semantics.PrimOp.Merge (mergeVector)
import RVV.Semantics.Value (Mask (Mask), VL (VL), VLValue (VLNum), Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigEF2M2, vectorConfigEF4MF2)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

mergeTest :: Test
mergeTest =
  testGroup
    "Merge"
    [ testCase "merge" $ do
        let actual =
              mergeVector
                vconst8162
                vectorConfigEF2M2
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0xcccc) (bv 16 0xffff),
                      VectorReg (bv 16 0xcccc) (bv 16 0xffff)
                    ]
                )
                (Mask 4 $ VectorReg (bv 16 0x00aa) (bv 16 0))
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0xffff),
                      VectorReg (bv 16 0x5678) (bv 16 0)
                    ]
                )
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0),
                      VectorReg (bv 16 0x0def) (bv 16 0xffff)
                    ]
                )
                (VL 4 (VLNum (bv 8 6)) tuPolicy false :: VL 'C)
        let expected =
              Right
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x92b4 :: SomeWordN) (bv 16 0x0f0f),
                      VectorReg (bv 16 0xcce8) (bv 16 0xfff0)
                    ]
                )
        actual @?= expected,
      testCase "lmul=1/2" $ do
        let actual =
              mergeVector
                vconst8162
                vectorConfigEF4MF2
                ( Vector
                    vectorConfigEF4MF2
                    [ VectorReg (bv 16 0xcccc) (bv 16 0xffff)
                    ]
                )
                (Mask 2 $ VectorReg (bv 16 0x000a) (bv 16 0))
                ( Vector
                    vectorConfigEF4MF2
                    [ VectorReg (bv 16 0x1234) (bv 16 0xffff)
                    ]
                )
                ( Vector
                    vectorConfigEF4MF2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0)
                    ]
                )
                (VL 2 (VLNum (bv 8 3)) tuPolicy false :: VL 'C)
        let expected =
              Right
                ( Vector
                    vectorConfigEF4MF2
                    [ VectorReg (bv 16 0xccfc :: SomeWordN) (bv 16 0xfff3)
                    ]
                )
        actual @?= expected
    ]
