{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Semantics.PrimOp.UtilTest (utilTest) where

import Data.Either (isLeft)
import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.Context (ConcreteContext)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Element
  ( MaskElement (MaskElement),
    VectorElement (VectorElement),
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Policy (tuPolicy)
import RVV.Semantics.PrimOp.Util
  ( handleMaskTailMasks,
    handleVectorTailMasks,
    scalarToVectorElement,
  )
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigE1M1,
    vectorConfigEF2M2,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

data HandleVectorTailMasksTest = HandleVectorTailMasksTest
  { handleVectorTailMasksTestName :: String,
    handleVectorTailMasksTestMachineConfig :: MachineConfig,
    handleVectorTailMasksTestVL :: VL 'C,
    handleVectorTailMasksTestMask :: Mask 'C,
    handleVectorTailMasksTestDest :: Vector 'C,
    handleVectorTailMasksTestValidResults ::
      [ConcreteContext (VectorElement 'C)],
    handleVectorTailMasksTestExpected :: Maybe (Vector 'C)
  }

data HandleMaskTailMasksTest = HandleMaskTailMasksTest
  { handleMaskTailMasksTestName :: String,
    handleMaskTailMasksTestMachineConfig :: MachineConfig,
    handleMaskTailMasksTestVL :: VL 'C,
    handleMaskTailMasksTestMask :: Mask 'C,
    handleMaskTailMasksTestDest :: Mask 'C,
    handleMaskTailMasksTestValidResults ::
      [ConcreteContext (MaskElement 'C)],
    handleMaskTailMasksTestExpected :: Maybe (Mask 'C)
  }

data ScalarToVectorElementTest = ScalarToVectorElementTest
  { scalarToVectorElementTestName :: String,
    scalarToVectorElementTestMachineConfig :: MachineConfig,
    scalarToVectorElementTestVectorConfig :: VectorConfig,
    scalarToVectorElementTestSigned :: Bool,
    scalarToVectorElementTestScalar :: Scalar 'C,
    scalarToVectorElementTestExpected :: VectorElement 'C
  }

utilTest :: Test
utilTest =
  testGroup
    "Util"
    [ testGroup "handleVectorTailMasks" $ do
        HandleVectorTailMasksTest {..} <-
          [ HandleVectorTailMasksTest
              { handleVectorTailMasksTestName = "valid",
                handleVectorTailMasksTestMachineConfig = vconst8162,
                handleVectorTailMasksTestVL = VL 4 (VLNum $ bv 8 0x7) tuPolicy false,
                handleVectorTailMasksTestMask =
                  Mask 4 $ VectorReg (bv 16 0x1234) (bv 16 0x0),
                handleVectorTailMasksTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x5678) (bv 16 0x0),
                      VectorReg (bv 16 0x9bcd) (bv 16 0x0)
                    ],
                handleVectorTailMasksTestValidResults =
                  (\i -> Right $ VectorElement (bv 4 i) (bv 4 0x0)) <$> [0 ..],
                handleVectorTailMasksTestExpected =
                  Just $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x0200) (bv 16 0xf0ff),
                        VectorReg (bv 16 0x9054) (bv 16 0x0f00)
                      ]
              }
            ]
        return $ testCase handleVectorTailMasksTestName $ do
          let actual =
                handleVectorTailMasks
                  handleVectorTailMasksTestMachineConfig
                  handleVectorTailMasksTestVL
                  handleVectorTailMasksTestMask
                  handleVectorTailMasksTestDest
                  handleVectorTailMasksTestValidResults ::
                  ConcreteContext (Vector 'C)
          case handleVectorTailMasksTestExpected of
            Nothing -> assertBool "Should fail" $ isLeft actual
            Just expected -> actual @?= Right expected,
      testGroup "handleMaskTailMasks" $ do
        HandleMaskTailMasksTest {..} <-
          [ HandleMaskTailMasksTest
              { handleMaskTailMasksTestName = "valid",
                handleMaskTailMasksTestMachineConfig = vconst8162,
                handleMaskTailMasksTestVL = VL 4 (VLNum $ bv 8 0x7) tuPolicy false,
                handleMaskTailMasksTestMask =
                  Mask 4 $ VectorReg (bv 16 0x1234) (bv 16 0x0),
                handleMaskTailMasksTestDest =
                  Mask 4 $ VectorReg (bv 16 0x4321) (bv 16 0x0),
                handleMaskTailMasksTestValidResults =
                  (\i -> Right $ MaskElement (even i) False) <$> [0 .. 7],
                handleMaskTailMasksTestExpected =
                  Just $ Mask 4 $ VectorReg (bv 16 0x0014) (bv 16 0xffcb)
              }
            ]
        return $ testCase handleMaskTailMasksTestName $ do
          let actual =
                handleMaskTailMasks
                  handleMaskTailMasksTestMachineConfig
                  handleMaskTailMasksTestVL
                  handleMaskTailMasksTestMask
                  handleMaskTailMasksTestDest
                  handleMaskTailMasksTestValidResults ::
                  ConcreteContext (Mask 'C)
          case handleMaskTailMasksTestExpected of
            Nothing -> assertBool "Should fail" $ isLeft actual
            Just expected -> actual @?= Right expected,
      testGroup "scalarToVectorElement" $ do
        ScalarToVectorElementTest {..} <-
          [ -- ScalarToVectorElementTest
            --   { scalarToVectorElementTestName = "Unsigned",
            --     scalarToVectorElementTestMachineConfig = vconst8162,
            --     scalarToVectorElementTestVType = vectorConfigE16M1,
            --     scalarToVectorElementTestSigned = False,
            --     scalarToVectorElementTestScalar = Scalar (bv 8 0x8a),
            --     scalarToVectorElementTestExpected =
            --       VectorElement (bv 16 0x8a) (bv 16 0x0)
            --   },
            -- ScalarToVectorElementTest
            --   { scalarToVectorElementTestName = "Signed",
            --     scalarToVectorElementTestMachineConfig = vconst8162,
            --     scalarToVectorElementTestVType = vectorConfigE16M1,
            --     scalarToVectorElementTestSigned = True,
            --     scalarToVectorElementTestScalar = Scalar (bv 8 0x8a),
            --     scalarToVectorElementTestExpected =
            --       VectorElement (bv 16 0xff8a) (bv 16 0x0)
            --   },
            ScalarToVectorElementTest
              { scalarToVectorElementTestName = "Equal bit width",
                scalarToVectorElementTestMachineConfig = vconst8162,
                scalarToVectorElementTestVectorConfig = vectorConfigE1M1,
                scalarToVectorElementTestSigned = True,
                scalarToVectorElementTestScalar = Scalar (bv 8 0x8a) false,
                scalarToVectorElementTestExpected =
                  VectorElement (bv 8 0x8a) (bv 8 0x0)
              }
            ]
        return $ testCase scalarToVectorElementTestName $ do
          let actual =
                scalarToVectorElement
                  scalarToVectorElementTestMachineConfig
                  scalarToVectorElementTestVectorConfig
                  scalarToVectorElementTestSigned
                  1
                  scalarToVectorElementTestScalar ::
                  ConcreteContext (VectorElement 'C)
          let expected = scalarToVectorElementTestExpected
          actual @?= Right expected
    ]
