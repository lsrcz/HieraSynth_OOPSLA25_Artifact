{-# LANGUAGE DataKinds #-}

module RVV.Semantics.PrimOp.FullMaskTest
  ( fullMaskTest,
  )
where

import Grisette (BV (bv))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.PrimOp.FullMask (fullMask)
import RVV.Semantics.Value (Mask (Mask), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants (vconst32128)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

fullMaskTest :: Test
fullMaskTest =
  testCase "fullMask" $ do
    let actual = fullMask vconst32128 2 :: Mask 'C
    actual @?= Mask 2 (VectorReg (bv 128 (-1)) (bv 128 0))
