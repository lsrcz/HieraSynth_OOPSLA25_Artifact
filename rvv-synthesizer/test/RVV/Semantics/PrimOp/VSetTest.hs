{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Semantics.PrimOp.VSetTest (vsetTest) where

import Data.Either (isLeft)
import Grisette (BV (bv))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.VSet (lmulExtend, vset)
import RVV.Semantics.Value (Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigE1M1,
    vectorConfigEF2M1,
    vectorConfigEF2M2,
    vectorConfigEF2M4,
    vectorConfigEF2MF2,
    vectorConfigEF2MF4,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)
import Test.HUnit.Base ((@?=))

data VSetTest = VSetTest
  { vsetTestName :: String,
    vsetTestMachineConfig :: MachineConfig,
    vsetTestDestVectorConfig :: VectorConfig,
    vsetTestPartVectorConfig :: VectorConfig,
    vsetTestIdx :: Int,
    vsetTestDest :: Vector 'C,
    vsetTestPart :: Vector 'C,
    vsetTestExpected :: Maybe (Vector 'C)
  }

e4mf4Vector :: Vector 'C
e4mf4Vector = Vector vectorConfigEF2MF4 [VectorReg (bv 16 0xcccd) (bv 16 0xdddc)]

e4mf2Vector :: Vector 'C
e4mf2Vector = Vector vectorConfigEF2MF2 [VectorReg (bv 16 0xeeff) (bv 16 0xffee)]

e4m4Vector :: Vector 'C
e4m4Vector =
  Vector
    vectorConfigEF2M4
    [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
      VectorReg (bv 16 0x4567) (bv 16 0x0123),
      VectorReg (bv 16 0x7890) (bv 16 0x3456),
      VectorReg (bv 16 0x1562) (bv 16 0x612d)
    ]

e4m2Vector :: Vector 'C
e4m2Vector =
  Vector
    vectorConfigEF2M2
    [ VectorReg (bv 16 0x4321) (bv 16 0x8765),
      VectorReg (bv 16 0x7654) (bv 16 0x3210)
    ]

e4m1Vector :: Vector 'C
e4m1Vector = Vector vectorConfigEF2M1 [VectorReg (bv 16 0xabcd) (bv 16 0xdcba)]

m1tom2Test :: VSetTest
m1tom2Test =
  VSetTest
    { vsetTestName = undefined,
      vsetTestMachineConfig = vconst8162,
      vsetTestDestVectorConfig = vectorConfigEF2M2,
      vsetTestPartVectorConfig = vectorConfigEF2M1,
      vsetTestIdx = undefined,
      vsetTestDest = e4m2Vector,
      vsetTestPart = e4m1Vector,
      vsetTestExpected = undefined
    }

m1tom4Test :: VSetTest
m1tom4Test =
  VSetTest
    { vsetTestName = undefined,
      vsetTestMachineConfig = vconst8162,
      vsetTestDestVectorConfig = vectorConfigEF2M4,
      vsetTestPartVectorConfig = vectorConfigEF2M1,
      vsetTestIdx = undefined,
      vsetTestDest = e4m4Vector,
      vsetTestPart = e4m1Vector,
      vsetTestExpected = undefined
    }

m2tom4Test :: VSetTest
m2tom4Test =
  VSetTest
    { vsetTestName = undefined,
      vsetTestMachineConfig = vconst8162,
      vsetTestDestVectorConfig = vectorConfigEF2M4,
      vsetTestPartVectorConfig = vectorConfigEF2M2,
      vsetTestIdx = undefined,
      vsetTestDest = e4m4Vector,
      vsetTestPart = e4m2Vector,
      vsetTestExpected = undefined
    }

mf2tom2Test :: VSetTest
mf2tom2Test =
  VSetTest
    { vsetTestName = undefined,
      vsetTestMachineConfig = vconst8162,
      vsetTestDestVectorConfig = vectorConfigEF2M2,
      vsetTestPartVectorConfig = vectorConfigEF2MF2,
      vsetTestIdx = undefined,
      vsetTestDest = e4m2Vector,
      vsetTestPart = e4mf2Vector,
      vsetTestExpected = undefined
    }

mf4tomf2Test :: VSetTest
mf4tomf2Test =
  VSetTest
    { vsetTestName = undefined,
      vsetTestMachineConfig = vconst8162,
      vsetTestDestVectorConfig = vectorConfigEF2MF2,
      vsetTestPartVectorConfig = vectorConfigEF2MF4,
      vsetTestIdx = undefined,
      vsetTestDest = e4mf2Vector,
      vsetTestPart = e4mf4Vector,
      vsetTestExpected = undefined
    }

vsetTest :: Test
vsetTest =
  testGroup
    "VSet"
    [ testGroup "VSet" $ do
        VSetTest {..} <-
          [ m1tom2Test
              { vsetTestName = "M1 to M2, 0",
                vsetTestIdx = 0,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0xabcd) (bv 16 0xdcba),
                        VectorReg (bv 16 0x7654) (bv 16 0x3210)
                      ]
              },
            m1tom2Test
              { vsetTestName = "M1 to M2, 1",
                vsetTestIdx = 1,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x4321) (bv 16 0x8765),
                        VectorReg (bv 16 0xabcd) (bv 16 0xdcba)
                      ]
              },
            m1tom4Test
              { vsetTestName = "M1 to M4, 0",
                vsetTestIdx = 0,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0xabcd) (bv 16 0xdcba),
                        VectorReg (bv 16 0x4567) (bv 16 0x0123),
                        VectorReg (bv 16 0x7890) (bv 16 0x3456),
                        VectorReg (bv 16 0x1562) (bv 16 0x612d)
                      ]
              },
            m1tom4Test
              { vsetTestName = "M1 to M4, 2",
                vsetTestIdx = 2,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                        VectorReg (bv 16 0x4567) (bv 16 0x0123),
                        VectorReg (bv 16 0xabcd) (bv 16 0xdcba),
                        VectorReg (bv 16 0x1562) (bv 16 0x612d)
                      ]
              },
            m1tom4Test
              { vsetTestName = "M1 to M4, 3",
                vsetTestIdx = 3,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                        VectorReg (bv 16 0x4567) (bv 16 0x0123),
                        VectorReg (bv 16 0x7890) (bv 16 0x3456),
                        VectorReg (bv 16 0xabcd) (bv 16 0xdcba)
                      ]
              },
            m2tom4Test
              { vsetTestName = "M2 to M4, 0",
                vsetTestIdx = 0,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x4321) (bv 16 0x8765),
                        VectorReg (bv 16 0x7654) (bv 16 0x3210),
                        VectorReg (bv 16 0x7890) (bv 16 0x3456),
                        VectorReg (bv 16 0x1562) (bv 16 0x612d)
                      ]
              },
            m2tom4Test
              { vsetTestName = "M2 to M4, 1",
                vsetTestIdx = 1,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                        VectorReg (bv 16 0x4567) (bv 16 0x0123),
                        VectorReg (bv 16 0x4321) (bv 16 0x8765),
                        VectorReg (bv 16 0x7654) (bv 16 0x3210)
                      ]
              },
            mf2tom2Test
              { vsetTestName = "MF2 to M2, 0",
                vsetTestIdx = 0,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x43ff) (bv 16 0x87ee),
                        VectorReg (bv 16 0x7654) (bv 16 0x3210)
                      ]
              },
            mf2tom2Test
              { vsetTestName = "MF2 to M2, 1",
                vsetTestIdx = 1,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0xff21) (bv 16 0xee65),
                        VectorReg (bv 16 0x7654) (bv 16 0x3210)
                      ]
              },
            mf2tom2Test
              { vsetTestName = "MF2 to M2, 2",
                vsetTestIdx = 2,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x4321) (bv 16 0x8765),
                        VectorReg (bv 16 0x76ff) (bv 16 0x32ee)
                      ]
              },
            mf2tom2Test
              { vsetTestName = "MF2 to M2, 3",
                vsetTestIdx = 3,
                vsetTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x4321) (bv 16 0x8765),
                        VectorReg (bv 16 0xff54) (bv 16 0xee10)
                      ]
              },
            mf4tomf2Test
              { vsetTestName = "MF4 to MF2, 0",
                vsetTestIdx = 0,
                vsetTestExpected =
                  Just $
                    Vector vectorConfigEF2MF2 [VectorReg (bv 16 0xeefd) (bv 16 0xffec)]
              },
            mf4tomf2Test
              { vsetTestName = "MF4 to MF2, 1",
                vsetTestIdx = 1,
                vsetTestExpected =
                  Just $
                    Vector vectorConfigEF2MF2 [VectorReg (bv 16 0xeedf) (bv 16 0xffce)]
              },
            VSetTest
              { vsetTestName = "different eew",
                vsetTestMachineConfig = vconst8162,
                vsetTestDestVectorConfig = vectorConfigE1M1,
                vsetTestPartVectorConfig = vectorConfigEF2MF4,
                vsetTestIdx = 0,
                vsetTestDest = Vector vectorConfigE1M1 [VectorReg (bv 16 0) (bv 16 0)],
                vsetTestPart = e4mf4Vector,
                vsetTestExpected =
                  Just $ Vector vectorConfigE1M1 [VectorReg (bv 16 0x000d) (bv 16 0x000c)]
              },
            VSetTest
              { vsetTestName = "dest smaller than part",
                vsetTestMachineConfig = vconst8162,
                vsetTestDestVectorConfig = vectorConfigEF2M2,
                vsetTestPartVectorConfig = vectorConfigEF2M4,
                vsetTestIdx = 0,
                vsetTestDest = e4m2Vector,
                vsetTestPart = e4m4Vector,
                vsetTestExpected = Nothing
              },
            VSetTest
              { vsetTestName = "dest has the same size as part",
                vsetTestMachineConfig = vconst8162,
                vsetTestDestVectorConfig = vectorConfigEF2M4,
                vsetTestPartVectorConfig = vectorConfigEF2M4,
                vsetTestIdx = 0,
                vsetTestDest = e4m4Vector,
                vsetTestPart = e4m4Vector,
                vsetTestExpected = Nothing
              },
            m1tom2Test
              { vsetTestName = "Index out of bound",
                vsetTestIdx = 2,
                vsetTestExpected = Nothing
              }
            ]
        return $ testCase vsetTestName $ do
          let actual =
                vset
                  vsetTestMachineConfig
                  vsetTestPartVectorConfig
                  vsetTestDestVectorConfig
                  vsetTestIdx
                  vsetTestDest
                  vsetTestPart
          case vsetTestExpected of
            Nothing -> assertBool "Expected an error" $ isLeft actual
            Just expected -> actual @?= Right expected,
      testCase "lmulExtend" $ do
        let actual =
              lmulExtend
                vconst8162
                vectorConfigEF2M1
                vectorConfigEF2M2
                e4m1Vector
        let expected =
              Vector
                vectorConfigEF2M2
                [ VectorReg (bv 16 0xabcd) (bv 16 0xdcba),
                  VectorReg (bv 16 0) (bv 16 0xffff)
                ]
        actual @?= Right expected
    ]
