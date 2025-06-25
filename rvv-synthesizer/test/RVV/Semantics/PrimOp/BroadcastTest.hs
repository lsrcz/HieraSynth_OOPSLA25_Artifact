{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Semantics.PrimOp.BroadcastTest (broadcastTest) where

import Data.Either (isLeft)
import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import HieraSynth.Context (ConcreteContext)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Policy (muPolicy, nonePolicy, tuPolicy, tumuPolicy)
import RVV.Semantics.PrimOp.Broadcast (broadcast, fullBroadcast)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg)
  )
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigEF2M2,
    vectorConfigEF2M4,
    vectorConfigEF2MF2,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

x8v16m4dest :: Vector 'C
x8v16m4dest =
  Vector
    vectorConfigEF2M4
    [ VectorReg (bv 16 0xabcd) (bv 16 0x0ff0),
      VectorReg (bv 16 0xbcde) (bv 16 0x0ff0),
      VectorReg (bv 16 0xcdef) (bv 16 0xf00f),
      VectorReg (bv 16 0x6789) (bv 16 0xf00f)
    ]

x8v16ef2m4fullmask :: Mask 'C
x8v16ef2m4fullmask = Mask 8 $ VectorReg (bv 16 0xffff) (bv 16 0)

x8v16ef2m4partialmask :: Mask 'C
x8v16ef2m4partialmask = Mask 8 $ VectorReg (bv 16 0x5678) (bv 16 0)

x8v16ef2m4partialuninitializedmask :: Mask 'C
x8v16ef2m4partialuninitializedmask =
  Mask 8 $ VectorReg (bv 16 0x5678) (bv 16 0x8765)

data BroadcastTest = BroadcastTest
  { broadcastTestName :: String,
    broadcastTestMachineConfig :: MachineConfig,
    broadcastTestVectorConfig :: VectorConfig,
    broadcastTestDest :: Vector 'C,
    broadcastTestMask :: Mask 'C,
    broadcastTestSrc :: Scalar 'C,
    broadcastTestVL :: VL 'C,
    broadcastTestExpected :: Either () (Vector 'C)
  }

broadcastTest :: Test
broadcastTest =
  testGroup
    "Broadcast"
    [ testGroup "broadcast" $ do
        BroadcastTest {..} <-
          [ BroadcastTest
              { broadcastTestName = "X8V16EF2M4 tail agnostic",
                broadcastTestMachineConfig = vconst8162,
                broadcastTestVectorConfig = vectorConfigEF2M4,
                broadcastTestDest = x8v16m4dest,
                broadcastTestMask = x8v16ef2m4fullmask,
                broadcastTestSrc = Scalar (bv 4 0x05) false,
                broadcastTestExpected =
                  Right $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x5555) (bv 16 0x0000),
                        VectorReg (bv 16 0x5555) (bv 16 0x0000),
                        VectorReg (bv 16 0x0005) (bv 16 0xfff0),
                        VectorReg (bv 16 0x0000) (bv 16 0xffff)
                      ],
                broadcastTestVL = VL 8 (VLNum (bv 8 0x09)) nonePolicy false
              },
            BroadcastTest
              { broadcastTestName = "X8V16EF2M4 tail undisturbed",
                broadcastTestMachineConfig = vconst8162,
                broadcastTestVectorConfig = vectorConfigEF2M4,
                broadcastTestDest = x8v16m4dest,
                broadcastTestMask = x8v16ef2m4fullmask,
                broadcastTestSrc = Scalar (bv 4 0x05) false,
                broadcastTestExpected =
                  Right $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x5555) (bv 16 0x0000),
                        VectorReg (bv 16 0x5555) (bv 16 0x0000),
                        VectorReg (bv 16 0xcde5) (bv 16 0xf000),
                        VectorReg (bv 16 0x6789) (bv 16 0xf00f)
                      ],
                broadcastTestVL = VL 8 (VLNum (bv 8 0x09)) tuPolicy false
              },
            BroadcastTest
              { broadcastTestName = "X8V16EF2M4 mask agnostic",
                broadcastTestMachineConfig = vconst8162,
                broadcastTestVectorConfig = vectorConfigEF2M4,
                broadcastTestDest = x8v16m4dest,
                broadcastTestMask = x8v16ef2m4partialmask,
                broadcastTestSrc = Scalar (bv 4 0x05) false,
                broadcastTestExpected =
                  Right $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x5000) (bv 16 0x0fff),
                        VectorReg (bv 16 0x0555) (bv 16 0xf000),
                        VectorReg (bv 16 0x0550) (bv 16 0xf00f),
                        VectorReg (bv 16 0x0505) (bv 16 0xf0f0)
                      ],
                broadcastTestVL = VL 8 (VLNum (bv 8 0x10)) nonePolicy false
              },
            BroadcastTest
              { broadcastTestName = "X8V16EF2M4 mask undisturbed",
                broadcastTestMachineConfig = vconst8162,
                broadcastTestVectorConfig = vectorConfigEF2M4,
                broadcastTestDest = x8v16m4dest,
                broadcastTestMask = x8v16ef2m4partialmask,
                broadcastTestSrc = Scalar (bv 4 0x05) false,
                broadcastTestExpected =
                  Right $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x5bcd) (bv 16 0x0ff0),
                        VectorReg (bv 16 0xb555) (bv 16 0x0000),
                        VectorReg (bv 16 0xc55f) (bv 16 0xf00f),
                        VectorReg (bv 16 0x6585) (bv 16 0xf000)
                      ],
                broadcastTestVL = VL 8 (VLNum (bv 8 0x10)) muPolicy false
              },
            BroadcastTest
              { broadcastTestName =
                  "X8V16EF2M4 mask undisturbed/partially uninitialized",
                broadcastTestMachineConfig = vconst8162,
                broadcastTestVectorConfig = vectorConfigEF2M4,
                broadcastTestDest = x8v16m4dest,
                broadcastTestMask = x8v16ef2m4partialuninitializedmask,
                broadcastTestSrc = Scalar (bv 4 0x05) false,
                broadcastTestExpected =
                  Right $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x5bcd) (bv 16 0x0fff),
                        VectorReg (bv 16 0xb555) (bv 16 0x0ff0),
                        VectorReg (bv 16 0xc55f) (bv 16 0xffff),
                        VectorReg (bv 16 0x6585) (bv 16 0xf000)
                      ],
                broadcastTestVL = VL 8 (VLNum (bv 8 0x10)) muPolicy false
              },
            BroadcastTest
              { broadcastTestName =
                  "X8V16EF2MF2",
                broadcastTestMachineConfig = vconst8162,
                broadcastTestVectorConfig = vectorConfigEF2MF2,
                broadcastTestDest =
                  Vector vectorConfigEF2MF2 [VectorReg (bv 16 0xabcd) (bv 16 0x0f0f)],
                broadcastTestMask =
                  Mask 1 $ VectorReg (bv 16 0x1245) (bv 16 0xf000),
                broadcastTestSrc = Scalar (bv 4 0x05) false,
                broadcastTestExpected =
                  Right $
                    Vector
                      vectorConfigEF2MF2
                      [ VectorReg (bv 16 0xabc5) (bv 16 0x0f00)
                      ],
                broadcastTestVL = VL 1 (VLNum (bv 8 0x2)) tumuPolicy false
              }
            ]
        return $ testCase broadcastTestName $ do
          let actual =
                broadcast
                  broadcastTestMachineConfig
                  broadcastTestVectorConfig
                  broadcastTestDest
                  broadcastTestMask
                  broadcastTestSrc
                  broadcastTestVL
          case broadcastTestExpected of
            Left () -> assertBool "Should be invalid" $ isLeft actual
            Right expected -> actual @?= Right expected,
      testCase "fullBroadcast" $ do
        let actual =
              fullBroadcast
                vconst8162
                vectorConfigEF2M2
                (Scalar (bv 4 0x05) false) ::
                ConcreteContext (Vector 'C)
        let expected =
              Right $
                Vector
                  vectorConfigEF2M2
                  [ VectorReg (bv 16 0x5555 :: SomeWordN) (bv 16 0x0000),
                    VectorReg (bv 16 0x5555 :: SomeWordN) (bv 16 0x0000)
                  ]
        actual @?= expected
    ]
