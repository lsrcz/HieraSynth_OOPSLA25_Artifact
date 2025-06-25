{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.Synthesizer.TypeTest (typeTest) where

import Grisette (PPrint (pformat))
import HieraSynth.Util.Pretty (renderDoc)
import RVV.Semantics.VectorConfigConstants (vectorConfigEF2M2)
import RVV.Synthesizer.Type
  ( ValueType
      ( MaskType,
        MemType,
        PtrType,
        ScalarType,
        VLType,
        VectorType
      ),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

typeTest :: Test
typeTest =
  testGroup
    "Type"
    [ testCase "PPrint" $ do
        renderDoc 80 (pformat (MaskType 4)) @?= "mask<mmul=4>"
        renderDoc 80 (pformat (VectorType vectorConfigEF2M2))
          @?= "vec<ef2m2>"
        renderDoc 80 (pformat (VLType 2 :: ValueType)) @?= "vl<mmul=2>"
        renderDoc 80 (pformat (ScalarType (1 / 4) :: ValueType))
          @?= "scalar<xmul=f4>"
        renderDoc 80 (pformat (PtrType 4 0 :: ValueType))
          @?= "ptr<xmul=4, block=0>"
        renderDoc 80 (pformat (MemType :: ValueType)) @?= "mem"
    ]
