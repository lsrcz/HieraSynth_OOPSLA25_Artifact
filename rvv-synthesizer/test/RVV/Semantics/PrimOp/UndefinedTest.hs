{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Semantics.PrimOp.UndefinedTest (undefinedTest) where

import Grisette (BV (bv))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Undefined (undefinedVector, undefinedVectorReg)
import RVV.Semantics.Value (Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Semantics.VectorConfigConstants
  ( vconst32128,
    vectorConfigE1M1,
    vectorConfigE1MF8,
    vectorConfigEF2M4,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data UndefinedVectorTest = UndefinedVectorTest
  { undefinedVectorTestName :: String,
    undefinedVectorMachineConfig :: MachineConfig,
    undefinedVectorVectorConfig :: VectorConfig,
    undefinedVectorExpected :: Vector 'C
  }

undefinedTest :: Test
undefinedTest =
  testGroup
    "Undefined"
    [ testCase "undefinedVectorReg" $ do
        let actual = undefinedVectorReg vconst32128 :: VectorReg 'C
        actual @?= VectorReg (bv 128 0) (bv 128 (-1)),
      testGroup "undefinedVector" $ do
        UndefinedVectorTest {..} <-
          [ UndefinedVectorTest
              "EF2M4"
              vconst32128
              vectorConfigEF2M4
              ( Vector vectorConfigEF2M4 $
                  replicate 4 $
                    VectorReg (bv 128 0) (bv 128 (-1))
              ),
            UndefinedVectorTest
              "E1M1"
              vconst32128
              vectorConfigE1M1
              (Vector vectorConfigE1M1 [VectorReg (bv 128 0) (bv 128 (-1))]),
            UndefinedVectorTest
              "E1MF8"
              vconst32128
              vectorConfigE1MF8
              (Vector vectorConfigE1MF8 [VectorReg (bv 128 0) (bv 128 (-1))])
            ]
        return $ testCase undefinedVectorTestName $ do
          let actual =
                undefinedVector
                  undefinedVectorMachineConfig
                  undefinedVectorVectorConfig ::
                  Vector 'C
          actual @?= undefinedVectorExpected
    ]
