{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Semantics.SizeConstraintTest (sizeConstraintTest) where

import Data.Either (isLeft, isRight)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig
      ( MachineBaseConfig,
        machineMemoryBlockIds,
        machinePointerUnit,
        machineScalarImmLength,
        machineScalarLength,
        machineVectorImmLength
      ),
    MachineConfig
      ( MachineConfig,
        baseConfig,
        scalableConfig, allowPartialVL, useVLMask
      ),
    MachineScalableConfig
      ( MachineScalableConfig,
        machineMemoryBlockLengths,
        machineVectorLength
      ), AllowPartialVL (DisallowPartialVL),
  )
import RVV.Semantics.Memory
  ( BlockId,
    Memory (Memory),
    MemoryBlock (MemoryBlock),
    Ptr (Ptr),
  )
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.Policy (tuPolicy)
import RVV.Semantics.SizeConstraint
  ( narrowableVectorConfig,
    validMachineConfig,
    validMask,
    validMaskMul,
    validMemory,
    validMemoryBlock,
    validPtr,
    validScalar,
    validVL,
    validVector,
    validVectorConfig,
    validWidthMul,
  )
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig (VectorConfig (VectorConfig))
import RVV.Semantics.VectorConfigConstants
  ( vconst32128,
    vconst8161,
    vconst8162,
    vconstWithMem,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)

data MachineConfigTest = MachineConfigTest
  { vconstTestName :: String,
    vconstValue :: MachineConfig,
    vconstIsValid :: Bool
  }

data VectorConfigTest = VectorConfigTest
  { vectorConfigTestName :: String,
    vectorConfigTestConst :: MachineConfig,
    vectorConfigTestValue :: VectorConfig,
    vectorConfigTestIsValid :: Bool
  }

data MaskTest = MaskTest
  { vmaskTestName :: String,
    vmaskVconst :: MachineConfig,
    vmaskMaskMul :: MaskMul,
    vmaskValue :: Mask 'C,
    vmaskIsValid :: Bool
  }

data VectorTest = VectorTest
  { vregTestName :: String,
    vregVconst :: MachineConfig,
    vregVectorConfig :: VectorConfig,
    vregValue :: [VectorReg 'C],
    vregIsValid :: Bool
  }

data ScalarTest = ScalarTest
  { xregTestName :: String,
    xregVconst :: MachineConfig,
    xregValue :: Scalar 'C,
    xregWidthMul :: WidthMul,
    xregIsValid :: Bool
  }

data VLTest = VLTest
  { vlregTestName :: String,
    vlregVconst :: MachineConfig,
    vlregMaskMul :: MaskMul,
    vlregValue :: VL 'C,
    vlregIsValid :: Bool
  }

data MemoryBlockTest = MemoryBlockTest
  { memoryBlockTestName :: String,
    memoryBlockVconst :: MachineConfig,
    memoryBlockValue :: MemoryBlock 'C,
    memoryBlockIsValid :: Bool
  }

data MemoryTest = MemoryTest
  { memoryTestName :: String,
    memoryVconst :: MachineConfig,
    memoryValue :: Memory 'C,
    memoryIsValid :: Bool
  }

data PtrTest = PtrTest
  { ptrTestName :: String,
    ptrMachineConfig :: MachineConfig,
    ptrWidthMul :: WidthMul,
    ptrBlockId :: BlockId,
    ptrValue :: Ptr 'C,
    ptrIsValid :: Bool
  }

data WidthMulTest = WidthMulTest
  { widthMulTestName :: String,
    widthMulMachineConfig :: MachineConfig,
    widthMulValue :: WidthMul,
    widthMulIsValid :: Bool
  }

sizeConstraintTest :: Test
sizeConstraintTest =
  testGroup
    "SizeConstraint"
    [ testGroup "validMachineConfig" $ do
        MachineConfigTest {..} <-
          [ MachineConfigTest
              "Negative XLEN"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = -1,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 12,
                          machineVectorImmLength = 5,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "Zero XLEN"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 0,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 12,
                          machineVectorImmLength = 5,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "Negative VLEN"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = -1,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 12,
                          machineVectorImmLength = 5,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "Zero VLEN"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 0,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 12,
                          machineVectorImmLength = 5,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "ptr_unit not power of two"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 10,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 10,
                          machinePointerUnit = 5,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 3,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "xlen not power of two"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 8,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 40,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 3,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "vlen not power of two"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 40,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 40,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 3,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "Vlen not a multiple of xlen"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 16,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 3,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              True,
            MachineConfigTest
              "xlen not multiple of ptr_unit"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 32,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 64,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 3,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "ximmlen larger than xlen"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 33,
                          machineVectorImmLength = 5,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "ximmlen smaller than log2(xlen)"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 4,
                          machineVectorImmLength = 5,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "vimmlen larger than xlen"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 12,
                          machineVectorImmLength = 33,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "vimmlen smaller than log2(xlen)"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.empty
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 4,
                          machineMemoryBlockIds = S.empty
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              True,
            MachineConfigTest
              "unmatched memory block ids"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.fromList [(1, 2)]
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 4,
                          machineMemoryBlockIds = S.fromList [2]
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "zero memory block size"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.fromList [(1, 0)]
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 4,
                          machineMemoryBlockIds = S.fromList [1]
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "negative memory block size"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.fromList [(1, -1)]
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 4,
                          machineMemoryBlockIds = S.fromList [1]
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "memory block size isn't multiple of ptr_unit"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.fromList [(1, 5)]
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 5,
                          machineVectorImmLength = 4,
                          machineMemoryBlockIds = S.fromList [1]
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              False,
            MachineConfigTest
              "valid"
              ( MachineConfig
                  { scalableConfig =
                      MachineScalableConfig
                        { machineVectorLength = 128,
                          machineMemoryBlockLengths = M.fromList [(1, 12)]
                        },
                    baseConfig =
                      MachineBaseConfig
                        { machineScalarLength = 32,
                          machinePointerUnit = 4,
                          machineScalarImmLength = 12,
                          machineVectorImmLength = 5,
                          machineMemoryBlockIds = S.fromList [1]
                        },
                    useVLMask = False,
                    allowPartialVL = DisallowPartialVL
                  }
              )
              True
            ]
        return . testCase vconstTestName $ do
          let actual = validMachineConfig vconstValue
          ( if vconstIsValid
              then case actual of
                Right _ -> return ()
                Left err -> fail $ T.unpack err
              else assertBool "Should be invalid" $ isLeft actual
            ),
      testGroup "validVectorConfig" $ do
        VectorConfigTest {..} <-
          [ VectorConfigTest "Negative xmul" vconst32128 (VectorConfig (-1) 1) False,
            VectorConfigTest "Zero xmul" vconst32128 (VectorConfig 0 1) False,
            VectorConfigTest "Fractional EEW" vconst32128 (VectorConfig (1 / 64) 8) False,
            VectorConfigTest "Fractional EEW 2" vconst32128 (VectorConfig (1 / 5) 8) False,
            VectorConfigTest "EMUL Not power of 2 (1)" vconst32128 (VectorConfig (1 / 8) 3) False,
            VectorConfigTest "EMUL Not power of 2 (2)" vconst32128 (VectorConfig (1 / 8) (1 / 3)) False,
            VectorConfigTest "EMUL Not power of 2 (3)" vconst32128 (VectorConfig (1 / 8) (2 / 3)) False,
            VectorConfigTest
              "EEW not a multiple of EMUL"
              vconst32128
              (VectorConfig (1 / 8) 8)
              False,
            VectorConfigTest
              "Vlen not a multiple of ratio"
              vconst32128
              (VectorConfig (1 / 32) (1 / 8))
              False,
            VectorConfigTest
              "Xlen not a multiple of eew"
              vconst32128
              (VectorConfig (1 / 5) 1)
              False,
            VectorConfigTest
              "EEW not a multiple of ptr_unit"
              vconst32128
              (VectorConfig (1 / 16) 1)
              False,
            VectorConfigTest "valid vectorConfig 1" vconst32128 (VectorConfig (1 / 8) 1) True,
            VectorConfigTest "valid vectorConfig 2" vconst32128 (VectorConfig (1 / 8) 4) True,
            VectorConfigTest "valid vectorConfig 3" vconst32128 (VectorConfig (1 / 8) (1 / 8)) True
            ]
        return . testCase vectorConfigTestName $ do
          let actual = validVectorConfig vectorConfigTestConst vectorConfigTestValue
          ( if vectorConfigTestIsValid
              then case actual of
                Right _ -> return ()
                Left err -> fail $ T.unpack err
              else assertBool "Should be invalid" $ isLeft actual
            ),
      testGroup "validMaskMul" $ do
        (mmul, valid) <-
          [ (1 / 8, False),
            (1 / 5, False),
            (1 / 4, True),
            (1 / 3, False),
            (1 / 2, True),
            (1, True),
            (2, True),
            (3, False),
            (4, True),
            (31, False),
            (32, True),
            (33, False),
            (64, False)
            ]
        return $ testCase (show mmul) $ do
          let actual = validMaskMul vconst32128 mmul
          if valid
            then assertBool "Should be valid" $ isRight actual
            else assertBool "Should be invalid" $ isLeft actual,
      testGroup "validMask" $ do
        MaskTest {..} <-
          [ MaskTest
              { vmaskTestName = "valid",
                vmaskVconst = vconst32128,
                vmaskMaskMul = 1,
                vmaskValue = Mask 1 $ VectorReg (bv 128 0) (bv 128 0),
                vmaskIsValid = True
              },
            MaskTest
              { vmaskTestName = "valid 2",
                vmaskVconst = vconst32128,
                vmaskMaskMul = 32,
                vmaskValue = Mask 32 $ VectorReg (bv 128 0) (bv 128 0),
                vmaskIsValid = True
              },
            MaskTest
              { vmaskTestName = "unmatched maskMul",
                vmaskVconst = vconst32128,
                vmaskMaskMul = 32,
                vmaskValue = Mask 1 $ VectorReg (bv 128 0) (bv 128 0),
                vmaskIsValid = False
              },
            MaskTest
              { vmaskTestName = "invalid maskMul",
                vmaskVconst = vconst32128,
                vmaskMaskMul = 64,
                vmaskValue = Mask 64 $ VectorReg (bv 128 0) (bv 128 0),
                vmaskIsValid = False
              },
            MaskTest
              { vmaskTestName = "invalid data",
                vmaskVconst = vconst32128,
                vmaskMaskMul = 1,
                vmaskValue = Mask 1 $ VectorReg (bv 127 0) (bv 128 0),
                vmaskIsValid = False
              },
            MaskTest
              { vmaskTestName = "invalid vmask uninitialized bits",
                vmaskVconst = vconst32128,
                vmaskMaskMul = 1,
                vmaskValue = Mask 1 $ VectorReg (bv 128 0) (bv 127 0),
                vmaskIsValid = False
              }
            ]
        return . testCase vmaskTestName $ do
          let actual =
                validMask
                  vmaskVconst
                  vmaskMaskMul
                  vmaskValue
          if vmaskIsValid
            then case actual of
              Left err -> fail $ T.unpack err
              Right _ -> return ()
            else assertBool "Should be invalid" $ isLeft actual,
      testCase "validVector with incorrect vectorConfig" $ do
        let actual =
              validVector
                vconst32128
                (VectorConfig (1 / 8) 4)
                ( Vector (VectorConfig (1 / 8) 2) $
                    replicate 4 $
                      VectorReg (bv 128 0) (bv 128 0 :: SomeWordN)
                )
        assertBool "Shouldn't be valid" $ isLeft actual,
      testGroup "validVector" $ do
        VectorTest {..} <-
          [ VectorTest
              { vregTestName = "valid",
                vregVconst = vconst32128,
                vregVectorConfig = VectorConfig (1 / 8) 4,
                vregValue =
                  replicate 4 $ VectorReg (bv 128 0) (bv 128 0),
                vregIsValid = True
              },
            VectorTest
              { vregTestName = "invalid vectorConfig",
                vregVconst = vconst32128,
                vregVectorConfig = VectorConfig 10 1,
                vregValue =
                  replicate 4 $ VectorReg (bv 128 0) (bv 128 0),
                vregIsValid = False
              },
            VectorTest
              { vregTestName = "invalid data",
                vregVconst = vconst32128,
                vregVectorConfig = VectorConfig (1 / 8) 4,
                vregValue =
                  replicate 4 $ VectorReg (bv 127 0) (bv 128 0),
                vregIsValid = False
              },
            VectorTest
              { vregTestName = "invalid vreg uninitialized bits",
                vregVconst = vconst32128,
                vregVectorConfig = VectorConfig (1 / 8) 4,
                vregValue =
                  replicate 4 $ VectorReg (bv 128 0) (bv 127 0),
                vregIsValid = False
              },
            VectorTest
              { vregTestName = "invalid number of registers",
                vregVconst = vconst32128,
                vregVectorConfig = VectorConfig (1 / 8) 4,
                vregValue =
                  replicate 3 $ VectorReg (bv 128 0) (bv 128 0),
                vregIsValid = False
              }
            ]
        return . testCase vregTestName $ do
          let actual =
                validVector
                  vregVconst
                  vregVectorConfig
                  (Vector vregVectorConfig vregValue)
          ( if vregIsValid
              then case actual of
                Right _ -> return ()
                Left err -> fail $ T.unpack err
              else assertBool "Should be invalid" $ isLeft actual
            ),
      testGroup "validScalar" $ do
        ScalarTest {..} <-
          [ ScalarTest
              { xregTestName = "valid 1",
                xregVconst = vconst32128,
                xregValue = Scalar (bv 32 0) false,
                xregWidthMul = 1,
                xregIsValid = True
              },
            ScalarTest
              { xregTestName = "valid 2",
                xregVconst = vconst32128,
                xregValue = Scalar (bv 16 0) false,
                xregWidthMul = 1 / 2,
                xregIsValid = True
              },
            ScalarTest
              { xregTestName = "invalid",
                xregVconst = vconst32128,
                xregValue = Scalar (bv 33 0) false,
                xregWidthMul = 1,
                xregIsValid = False
              },
            ScalarTest
              { xregTestName = "inconsistent xmul",
                xregVconst = vconst32128,
                xregValue = Scalar (bv 16 0) false,
                xregWidthMul = 1,
                xregIsValid = False
              }
            ]
        return . testCase xregTestName $ do
          let actual = validScalar xregVconst xregWidthMul xregValue
          ( if xregIsValid
              then case actual of
                Right _ -> return ()
                Left err -> fail $ T.unpack err
              else assertBool "Should be invalid" $ isLeft actual
            ),
      testGroup "validVL" $ do
        VLTest {..} <-
          [ VLTest
              { vlregTestName = "valid",
                vlregVconst = vconst32128,
                vlregMaskMul = 1,
                vlregValue = VL 1 (VLNum $ bv 32 0) tuPolicy false,
                vlregIsValid = True
              },
            VLTest
              { vlregTestName = "inconsistent maskMul",
                vlregVconst = vconst32128,
                vlregMaskMul = 1,
                vlregValue = VL 2 (VLNum $ bv 32 0) tuPolicy false,
                vlregIsValid = False
              },
            VLTest
              { vlregTestName = "inconsistent xlen",
                vlregVconst = vconst32128,
                vlregMaskMul = 1,
                vlregValue = VL 1 (VLNum $ bv 31 0) tuPolicy false,
                vlregIsValid = False
              },
            VLTest
              { vlregTestName = "maxvl",
                vlregVconst = vconst32128,
                vlregMaskMul = 16,
                vlregValue = VL 16 (VLNum $ bv 32 64) tuPolicy false,
                vlregIsValid = True
              },
            VLTest
              { vlregTestName = "maxvl + 1",
                vlregVconst = vconst32128,
                vlregMaskMul = 16,
                vlregValue = VL 16 (VLNum $ bv 32 65) tuPolicy false,
                vlregIsValid = False
              }
            ]
        return . testCase vlregTestName $ do
          let actual =
                validVL
                  vlregVconst
                  vlregMaskMul
                  vlregValue
          ( if vlregIsValid
              then case actual of
                Right _ -> return ()
                Left err -> fail $ T.unpack err
              else assertBool "Should be invalid" $ isLeft actual
            ),
      testGroup "validMemoryBlock" $ do
        MemoryBlockTest {..} <-
          [ MemoryBlockTest
              { memoryBlockTestName = "valid",
                memoryBlockVconst = vconst32128,
                memoryBlockValue = MemoryBlock (bv 20 0) (bv 20 0),
                memoryBlockIsValid = True
              },
            MemoryBlockTest
              { memoryBlockTestName = "invalid",
                memoryBlockVconst = vconst32128,
                memoryBlockValue = MemoryBlock (bv 32 0) (bv 16 0),
                memoryBlockIsValid = False
              }
            ]
        return . testCase memoryBlockTestName $ do
          let actual =
                validMemoryBlock
                  memoryBlockVconst
                  memoryBlockValue
          ( if memoryBlockIsValid
              then case actual of
                Right _ -> return ()
                Left err -> fail $ T.unpack err
              else assertBool "Should be invalid" $ isLeft actual
            ),
      testGroup "validMemory" $ do
        MemoryTest {..} <-
          [ MemoryTest
              { memoryTestName = "valid",
                memoryVconst = vconstWithMem vconst32128 $ M.fromList [(1, 20)],
                memoryValue =
                  Memory (M.fromList [(1, MemoryBlock (bv 20 0) (bv 20 0))]),
                memoryIsValid = True
              },
            MemoryTest
              { memoryTestName = "invalid memory block",
                memoryVconst = vconstWithMem vconst32128 $ M.fromList [(1, 32)],
                memoryValue =
                  Memory (M.fromList [(1, MemoryBlock (bv 32 0) (bv 16 0))]),
                memoryIsValid = False
              },
            MemoryTest
              { memoryTestName = "invalid memory block size",
                memoryVconst = vconstWithMem vconst32128 $ M.fromList [(1, 22)],
                memoryValue =
                  Memory (M.fromList [(1, MemoryBlock (bv 20 0) (bv 20 0))]),
                memoryIsValid = False
              },
            MemoryTest
              { memoryTestName = "invalid memory block id",
                memoryVconst = vconstWithMem vconst32128 $ M.fromList [(2, 20)],
                memoryValue =
                  Memory (M.fromList [(1, MemoryBlock (bv 20 0) (bv 20 0))]),
                memoryIsValid = False
              }
            ]
        return . testCase memoryTestName $ do
          let actual =
                validMemory
                  memoryVconst
                  memoryValue
          ( if memoryIsValid
              then case actual of
                Right _ -> return ()
                Left err -> fail $ T.unpack err
              else assertBool "Should be invalid" $ isLeft actual
            ),
      testGroup "validPtr" $ do
        PtrTest {..} <-
          [ PtrTest
              { ptrTestName = "valid",
                ptrMachineConfig = vconstWithMem vconst32128 $ M.fromList [(1, 20)],
                ptrWidthMul = 1 / 8,
                ptrBlockId = 1,
                ptrValue = Ptr (1 / 8) 1 (bv 32 0) false,
                ptrIsValid = True
              },
            PtrTest
              { ptrTestName = "invalid width",
                ptrMachineConfig = vconstWithMem vconst32128 $ M.fromList [(1, 20)],
                ptrWidthMul = 1 / 8,
                ptrBlockId = 1,
                ptrValue = Ptr (1 / 4) 1 (bv 32 0) false,
                ptrIsValid = False
              },
            PtrTest
              { ptrTestName = "invalid block id",
                ptrMachineConfig = vconstWithMem vconst32128 $ M.fromList [(1, 20)],
                ptrWidthMul = 1 / 8,
                ptrBlockId = 2,
                ptrValue = Ptr (1 / 8) 1 (bv 32 0) false,
                ptrIsValid = False
              },
            PtrTest
              { ptrTestName = "invalid block id",
                ptrMachineConfig = vconstWithMem vconst32128 $ M.fromList [(1, 20)],
                ptrWidthMul = 1 / 8,
                ptrBlockId = 2,
                ptrValue = Ptr (1 / 8) 2 (bv 32 0) false,
                ptrIsValid = False
              },
            PtrTest
              { ptrTestName = "invalid offset bit width",
                ptrMachineConfig = vconstWithMem vconst32128 $ M.fromList [(1, 20)],
                ptrWidthMul = 1 / 8,
                ptrBlockId = 1,
                ptrValue = Ptr (1 / 8) 1 (bv 64 0) false,
                ptrIsValid = False
              }
            ]
        return . testCase ptrTestName $ do
          let actual =
                validPtr
                  ptrMachineConfig
                  ptrWidthMul
                  ptrBlockId
                  ptrValue
          ( if ptrIsValid
              then case actual of
                Right _ -> return ()
                Left err -> fail $ T.unpack err
              else assertBool "Should be invalid" $ isLeft actual
            ),
      testGroup "validWidthMul" $ do
        WidthMulTest {..} <-
          [ WidthMulTest "valid 1" vconst8162 (1 / 4) True,
            WidthMulTest "valid 2" vconst8162 1 True,
            WidthMulTest "negative" vconst8162 (-1) False,
            WidthMulTest "zero" vconst8162 0 False,
            WidthMulTest "greater than 1" vconst8162 2 False,
            WidthMulTest "not 1/2^n" vconst8162 (1 / 3) False,
            WidthMulTest "not 1/2^n, denominator is 2^n" vconst8162 (3 / 4) False,
            WidthMulTest "width < ptr_unit" vconst8162 (1 / 8) False
            ]
        return . testCase widthMulTestName $ do
          let actual = validWidthMul widthMulMachineConfig widthMulValue
          ( if widthMulIsValid
              then case actual of
                Right _ -> return ()
                Left err -> fail $ T.unpack err
              else assertBool "Should be invalid" $ isLeft actual
            ),
      testGroup
        "narrowableVectorConfig"
        [ testCase "Narrowable" $ do
            let actual = narrowableVectorConfig vconst8161 (VectorConfig 1 8)
            assertBool "Should be narrowable" $ isRight actual,
          testCase "odd eew" $ do
            let actual = narrowableVectorConfig vconst8161 (VectorConfig 5 1)
            assertBool "Should not be narrowable" $ isLeft actual,
          testCase "invalid narrowed" $ do
            let actual = narrowableVectorConfig vconst8162 (VectorConfig (1 / 4) 4)
            assertBool "Should not be narrowable" $ isLeft actual
        ]
    ]
