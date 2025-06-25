{-# LANGUAGE DataKinds #-}

module RVV.Semantics.PrimOp.MaskLogicalTest (maskLogicalTest) where

import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (tumuPolicy)
import RVV.Semantics.PrimOp.MaskLogical (maskLogical)
import RVV.Semantics.Value (Mask (Mask), VL (VL), VLValue (VLNum), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants (vconst8162)
import RVV.Synthesizer.Parameter.MaskLogicalOpCode
  ( MaskLogicalOpCode (MAnd),
    interpretMaskLogicalOpCode,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

maskLogicalTest :: Test
maskLogicalTest =
  testGroup
    "MaskLogical"
    [ testCase "maskLogical" $ do
        let actual =
              maskLogical
                (interpretMaskLogicalOpCode MAnd)
                vconst8162
                4
                (Mask 4 $ VectorReg (bv 16 0x00aa) (bv 16 0))
                (Mask 4 $ VectorReg (bv 16 0x002f :: SomeWordN) (bv 16 0))
                (VL 4 (VLNum (bv 8 6)) tumuPolicy false :: VL 'C)
        let expected = Right (Mask 4 $ VectorReg (bv 16 0x002a) (bv 16 0xffc0))
        actual @?= expected
    ]
