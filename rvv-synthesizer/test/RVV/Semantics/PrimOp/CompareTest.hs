module RVV.Semantics.PrimOp.CompareTest (compareTest) where

import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import RVV.Semantics.Policy (tumuPolicy)
import RVV.Semantics.PrimOp.Compare (compareToMask)
import RVV.Semantics.Value (Mask (Mask), VL (VL), VLValue (VLNum), Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigEF2M2)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

compareTest :: Test
compareTest =
  testGroup
    "Compare"
    [ testCase "compareToMask" $ do
        let actual =
              compareToMask
                (<)
                vconst8162
                vectorConfigEF2M2
                (Mask 4 $ VectorReg (bv 16 0x0030) (bv 16 0))
                (Mask 4 $ VectorReg (bv 16 0x0013) (bv 16 0))
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1bd5) (bv 16 0),
                      VectorReg (bv 16 0x4a67) (bv 16 0)
                    ]
                )
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x3ae3) (bv 16 0),
                      VectorReg (bv 16 0x921c) (bv 16 0 :: SomeWordN)
                    ]
                )
                (VL 4 (VLNum (bv 8 0x7)) tumuPolicy false)
        let expected = Right $ Mask 4 $ VectorReg (bv 16 0x0032) (bv 16 0xff80)
        actual @?= expected
    ]
