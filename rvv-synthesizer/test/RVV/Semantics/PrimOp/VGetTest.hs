{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Semantics.PrimOp.VGetTest (vgetTest) where

import Data.Either (isLeft)
import Grisette (BV (bv))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.VGet (lmulTruncate, vget)
import RVV.Semantics.Value (Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigE1M1,
    vectorConfigEF2M1,
    vectorConfigEF2M2,
    vectorConfigEF2M4,
    vectorConfigEF2MF4,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

data VGetTest = VGetTest
  { vgetTestName :: String,
    vgetTestMachineConfig :: MachineConfig,
    vgetTestPartVectorConfig :: VectorConfig,
    vgetTestSrcVectorConfig :: VectorConfig,
    vgetTestIdx :: Int,
    vgetTestSrc :: Vector 'C,
    vgetTestExpected :: Maybe (Vector 'C)
  }

e4m4Vector :: Vector 'C
e4m4Vector =
  Vector
    vectorConfigEF2M4
    [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
      VectorReg (bv 16 0x4567) (bv 16 0x0123),
      VectorReg (bv 16 0x7890) (bv 16 0x3456),
      VectorReg (bv 16 0x1562) (bv 16 0x612d)
    ]

m4tom2Test :: VGetTest
m4tom2Test =
  VGetTest
    { vgetTestName = undefined,
      vgetTestMachineConfig = vconst8162,
      vgetTestPartVectorConfig = vectorConfigEF2M2,
      vgetTestSrcVectorConfig = vectorConfigEF2M4,
      vgetTestIdx = undefined,
      vgetTestSrc = e4m4Vector,
      vgetTestExpected = undefined
    }

m4tom1Test :: VGetTest
m4tom1Test =
  VGetTest
    { vgetTestName = undefined,
      vgetTestMachineConfig = vconst8162,
      vgetTestPartVectorConfig = vectorConfigEF2M1,
      vgetTestSrcVectorConfig = vectorConfigEF2M4,
      vgetTestIdx = undefined,
      vgetTestSrc = e4m4Vector,
      vgetTestExpected = undefined
    }

m4tomf4Test :: VGetTest
m4tomf4Test =
  VGetTest
    { vgetTestName = undefined,
      vgetTestMachineConfig = vconst8162,
      vgetTestPartVectorConfig = vectorConfigEF2MF4,
      vgetTestSrcVectorConfig = vectorConfigEF2M4,
      vgetTestIdx = undefined,
      vgetTestSrc = e4m4Vector,
      vgetTestExpected = undefined
    }

vgetTest :: Test
vgetTest =
  testGroup
    "VGet"
    [ testGroup "VGet" $ do
        VGetTest {..} <-
          [ m4tom2Test
              { vgetTestName = "M4 to M2, 0",
                vgetTestIdx = 0,
                vgetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                        VectorReg (bv 16 0x4567) (bv 16 0x0123)
                      ]
              },
            m4tom2Test
              { vgetTestName = "M4 to M2, 1",
                vgetTestIdx = 1,
                vgetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x7890) (bv 16 0x3456),
                        VectorReg (bv 16 0x1562) (bv 16 0x612d)
                      ]
              },
            m4tom1Test
              { vgetTestName = "M4 to M1, 0",
                vgetTestIdx = 0,
                vgetTestExpected =
                  Just $
                    Vector vectorConfigEF2M1 [VectorReg (bv 16 0x1234) (bv 16 0x5678)]
              },
            m4tom1Test
              { vgetTestName = "M4 to M1, 1",
                vgetTestIdx = 1,
                vgetTestExpected =
                  Just $
                    Vector vectorConfigEF2M1 [VectorReg (bv 16 0x4567) (bv 16 0x0123)]
              },
            m4tom1Test
              { vgetTestName = "M4 to M1, 3",
                vgetTestIdx = 3,
                vgetTestExpected =
                  Just $
                    Vector vectorConfigEF2M1 [VectorReg (bv 16 0x1562) (bv 16 0x612d)]
              },
            m4tomf4Test
              { vgetTestName = "M4 to MF4, 0",
                vgetTestIdx = 0,
                vgetTestExpected =
                  Just $
                    Vector vectorConfigEF2MF4 [VectorReg (bv 16 0x0004) (bv 16 0xfff8)]
              },
            m4tomf4Test
              { vgetTestName = "M4 to MF4, 3",
                vgetTestIdx = 3,
                vgetTestExpected =
                  Just $
                    Vector vectorConfigEF2MF4 [VectorReg (bv 16 0x0001) (bv 16 0xfff5)]
              },
            m4tomf4Test
              { vgetTestName = "M4 to MF4, 6",
                vgetTestIdx = 6,
                vgetTestExpected =
                  Just $
                    Vector vectorConfigEF2MF4 [VectorReg (bv 16 0x0005) (bv 16 0xfff1)]
              },
            m4tomf4Test
              { vgetTestName = "M4 to MF4, 15",
                vgetTestIdx = 15,
                vgetTestExpected =
                  Just $
                    Vector vectorConfigEF2MF4 [VectorReg (bv 16 0x0001) (bv 16 0xfff6)]
              },
            VGetTest
              { vgetTestName = "different eew",
                vgetTestMachineConfig = vconst8162,
                vgetTestPartVectorConfig = vectorConfigEF2MF4,
                vgetTestSrcVectorConfig = vectorConfigE1M1,
                vgetTestIdx = 0,
                vgetTestSrc = Vector vectorConfigE1M1 [VectorReg (bv 16 0) (bv 16 0)],
                vgetTestExpected =
                  Just $ Vector vectorConfigEF2MF4 [VectorReg (bv 16 0) (bv 16 0xfff0)]
              },
            VGetTest
              { vgetTestName = "src smaller than part",
                vgetTestMachineConfig = vconst8162,
                vgetTestPartVectorConfig = vectorConfigEF2M4,
                vgetTestSrcVectorConfig = vectorConfigEF2M1,
                vgetTestIdx = 0,
                vgetTestSrc = Vector vectorConfigEF2M1 [VectorReg (bv 16 0) (bv 16 0)],
                vgetTestExpected = Nothing
              },
            VGetTest
              { vgetTestName = "dest has the same size as part",
                vgetTestMachineConfig = vconst8162,
                vgetTestPartVectorConfig = vectorConfigEF2M4,
                vgetTestSrcVectorConfig = vectorConfigEF2M4,
                vgetTestIdx = 0,
                vgetTestSrc = e4m4Vector,
                vgetTestExpected = Nothing
              },
            m4tom1Test
              { vgetTestName = "Index out of bound",
                vgetTestIdx = 4,
                vgetTestExpected = Nothing
              }
            ]
        return $ testCase vgetTestName $ do
          let actual =
                vget
                  vgetTestMachineConfig
                  vgetTestPartVectorConfig
                  vgetTestSrcVectorConfig
                  vgetTestIdx
                  vgetTestSrc
          case vgetTestExpected of
            Nothing -> assertBool "Expected an error" $ isLeft actual
            Just expected -> actual @?= Right expected,
      testCase "lmulTruncate" $ do
        let actual =
              lmulTruncate
                vconst8162
                vectorConfigEF2M1
                vectorConfigEF2M4
                e4m4Vector
        let expected =
              Vector vectorConfigEF2M1 [VectorReg (bv 16 0x1234) (bv 16 0x5678)]
        actual @?= Right expected
    ]
