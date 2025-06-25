{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Semantics.PrimOp.GetVLTest (getVLTest) where

import Data.Either (isLeft)
import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Element (vlValueToVLMask)
import RVV.Semantics.MachineConfig (MachineConfig (useVLMask))
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.Policy (Policy, nonePolicy)
import RVV.Semantics.PrimOp.GetVL (getVL, getVLMax, getVLenB, unsafeComputeVL)
import RVV.Semantics.Value (Scalar (Scalar), VL (VL, vlMaskMul, vlValue), VLValue (VLNum))
import RVV.Semantics.VectorConfigConstants (vconst64512, vconst8162)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

data ComputeVLTest = ComputeVLTest
  { computeVLTestName :: String,
    computeVLTestVLMax :: Int,
    computeVLTestAVL :: Scalar 'C,
    computeVLTestExpected :: SomeWordN
  }

data GetVLTest = GetVLTest
  { getVLTestName :: String,
    getVLTestMachineConfig :: MachineConfig,
    getVLTestMaskMul :: MaskMul,
    getVLTestScalar :: Maybe (Scalar 'C),
    getVLTestPolicy :: Policy 'C,
    getVLExpected :: Either () (VL 'C)
  }

data VLenBTest = VLenBTest
  { vlenbTestName :: String,
    vlenbTestMachineConfig :: MachineConfig,
    vlenbTestExpected :: Scalar 'C
  }

getVLTest :: Test
getVLTest =
  testGroup
    "GetVL"
    [ testGroup "computeVL" $ do
        ComputeVLTest {..} <-
          [ ComputeVLTest
              { computeVLTestName = "AVL == 0",
                computeVLTestVLMax = 8,
                computeVLTestAVL = Scalar (bv 8 0) false,
                computeVLTestExpected = bv 8 0
              },
            ComputeVLTest
              { computeVLTestName = "AVL < VLMAX",
                computeVLTestVLMax = 8,
                computeVLTestAVL = Scalar (bv 8 7) false,
                computeVLTestExpected = bv 8 7
              },
            ComputeVLTest
              { computeVLTestName = "AVL == VLMAX",
                computeVLTestVLMax = 8,
                computeVLTestAVL = Scalar (bv 8 8) false,
                computeVLTestExpected = bv 8 8
              },
            ComputeVLTest
              { computeVLTestName = "AVL == VLMAX + 1",
                computeVLTestVLMax = 8,
                computeVLTestAVL = Scalar (bv 8 9) false,
                computeVLTestExpected = bv 8 5
              },
            ComputeVLTest
              { computeVLTestName = "AVL == VLMAX + 2",
                computeVLTestVLMax = 8,
                computeVLTestAVL = Scalar (bv 8 10) false,
                computeVLTestExpected = bv 8 5
              },
            ComputeVLTest
              { computeVLTestName = "AVL == VLMAX * 2 - 1",
                computeVLTestVLMax = 8,
                computeVLTestAVL = Scalar (bv 8 15) false,
                computeVLTestExpected = bv 8 8
              },
            ComputeVLTest
              { computeVLTestName = "AVL == VLMAX * 2",
                computeVLTestVLMax = 8,
                computeVLTestAVL = Scalar (bv 8 16) false,
                computeVLTestExpected = bv 8 8
              },
            ComputeVLTest
              { computeVLTestName = "AVL == VLMAX * 2 + 1",
                computeVLTestVLMax = 8,
                computeVLTestAVL = Scalar (bv 8 17) false,
                computeVLTestExpected = bv 8 8
              }
            ]
        return $ testCase computeVLTestName $ do
          let actual =
                unsafeComputeVL computeVLTestVLMax computeVLTestAVL
          let expected = computeVLTestExpected
          actual @?= expected,
      testGroup "getVL & getVLMax" $ do
        GetVLTest {..} <-
          [ GetVLTest
              { getVLTestName = "GetVL bad maskMul",
                getVLTestMachineConfig = vconst8162,
                getVLTestMaskMul = 3,
                getVLTestScalar = Just $ Scalar (bv 8 0) false,
                getVLTestPolicy = nonePolicy,
                getVLExpected = Left ()
              },
            GetVLTest
              { getVLTestName = "GetVL bad Scalar",
                getVLTestMachineConfig = vconst8162,
                getVLTestMaskMul = 2,
                getVLTestScalar = Just $ Scalar (bv 7 5) false,
                getVLTestPolicy = nonePolicy,
                getVLExpected = Left ()
              },
            GetVLTest
              { getVLTestName = "GetVL Good",
                getVLTestMachineConfig = vconst8162,
                getVLTestMaskMul = 4,
                getVLTestScalar = Just $ Scalar (bv 8 5) false,
                getVLTestPolicy = nonePolicy,
                getVLExpected = Right $ VL 4 (VLNum (bv 8 5)) nonePolicy false
              },
            GetVLTest
              { getVLTestName = "GetVL capped",
                getVLTestMachineConfig = vconst8162,
                getVLTestMaskMul = 4,
                getVLTestScalar = Just $ Scalar (bv 8 9) false,
                getVLTestPolicy = nonePolicy,
                getVLExpected = Right $ VL 4 (VLNum (bv 8 5)) nonePolicy false
              },
            GetVLTest
              { getVLTestName = "GetVL capped",
                getVLTestMachineConfig = vconst8162,
                getVLTestMaskMul = 4,
                getVLTestScalar = Just $ Scalar (bv 8 19) false,
                getVLTestPolicy = nonePolicy,
                getVLExpected = Right $ VL 4 (VLNum (bv 8 8)) nonePolicy false
              },
            GetVLTest
              { getVLTestName = "GetVLMax bad maskMul",
                getVLTestMachineConfig = vconst8162,
                getVLTestMaskMul = 3,
                getVLTestScalar = Nothing,
                getVLTestPolicy = nonePolicy,
                getVLExpected = Left ()
              },
            GetVLTest
              { getVLTestName = "GetVLMax Good",
                getVLTestMachineConfig = vconst8162,
                getVLTestMaskMul = 4,
                getVLTestScalar = Nothing,
                getVLTestPolicy = nonePolicy,
                getVLExpected = Right $ VL 4 (VLNum (bv 8 8)) nonePolicy false
              }
            ]
        return $ testGroup getVLTestName $ do
          (name, config, expected) <-
            [ ("Original", getVLTestMachineConfig, getVLExpected),
              ( "Use VLMask",
                getVLTestMachineConfig {useVLMask = True},
                ( \x ->
                    x
                      { vlValue =
                          vlValueToVLMask
                            getVLTestMachineConfig {useVLMask = True}
                            (vlMaskMul x)
                            (vlValue x)
                      }
                )
                  <$> getVLExpected
              )
              ]
          return $ testCase name $ do
            let actual =
                  case getVLTestScalar of
                    Nothing ->
                      getVLMax
                        config
                        getVLTestMaskMul
                        getVLTestPolicy
                    Just xreg ->
                      getVL
                        config
                        getVLTestMaskMul
                        xreg
                        getVLTestPolicy
            case expected of
              Left () -> assertBool "Should fail" $ isLeft actual
              Right expected -> actual @?= Right expected,
      testGroup "getVLenB" $ do
        VLenBTest {..} <-
          [ VLenBTest
              { vlenbTestName = "vconst8162",
                vlenbTestMachineConfig = vconst8162,
                vlenbTestExpected = Scalar (bv 8 8) false
              },
            VLenBTest
              { vlenbTestName = "vconst64512",
                vlenbTestMachineConfig = vconst64512,
                vlenbTestExpected = Scalar (bv 64 64) false
              }
            ]
        return $ testCase vlenbTestName $ do
          let actual = getVLenB @'C vlenbTestMachineConfig
          actual @?= Right vlenbTestExpected
    ]
