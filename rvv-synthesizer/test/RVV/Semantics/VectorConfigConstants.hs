module RVV.Semantics.VectorConfigConstants
  ( vconstWithMem,
    vconst8164,
    vconst8162,
    vconst8161,
    vconst32128,
    vectorConfigEF4M4,
    vectorConfigEF4M2,
    vectorConfigEF4M1,
    vectorConfigEF4MF2,
    vectorConfigEF4MF4,
    vectorConfigEF8M8,
    vectorConfigEF8M2,
    vectorConfigEF8M1,
    vectorConfigEF8MF2,
    vectorConfigEF8MF8,
    vectorConfigEF8M4,
    vectorConfigEF2M2,
    vectorConfigEF16M1,
    vconst64512,
    vectorConfigEF4M8,
    vectorConfigEF2MF2,
    vectorConfigEF2M4,
    vectorConfigE1M8,
    vectorConfigE1M4,
    vectorConfigE1M1,
    vectorConfigE1MF8,
    vectorConfigEF2M1,
    vectorConfigEF2MF4,
    vectorConfigE1M2,
    vconst64128,
  )
where

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
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
        scalableConfig,
        useVLMask,
        allowPartialVL
      ),
    MachineScalableConfig
      ( MachineScalableConfig,
        machineMemoryBlockLengths,
        machineVectorLength
      ),
    AllowPartialVL (AllowPartialVL),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (VectorConfig, elementWidthMul, lengthMul),
  )

vconstWithMem :: MachineConfig -> M.HashMap Int Int -> MachineConfig
vconstWithMem vconst blockLengths =
  vconst
    { scalableConfig =
        (scalableConfig vconst)
          { machineMemoryBlockLengths = blockLengths
          },
      baseConfig =
        (baseConfig vconst)
          { machineMemoryBlockIds = M.keysSet blockLengths
          }
    }

vconst8164 :: MachineConfig
vconst8164 =
  MachineConfig
    { scalableConfig =
        MachineScalableConfig
          { machineVectorLength = 16,
            machineMemoryBlockLengths = M.empty
          },
      baseConfig =
        MachineBaseConfig
          { machineScalarLength = 8,
            machinePointerUnit = 4,
            machineScalarImmLength = 5,
            machineVectorImmLength = 3,
            machineMemoryBlockIds = S.empty
          },
      useVLMask = False,
      allowPartialVL = AllowPartialVL
    }

vconst8161 :: MachineConfig
vconst8161 =
  MachineConfig
    { scalableConfig =
        MachineScalableConfig
          { machineVectorLength = 16,
            machineMemoryBlockLengths = M.empty
          },
      baseConfig =
        MachineBaseConfig
          { machineScalarLength = 8,
            machinePointerUnit = 1,
            machineScalarImmLength = 5,
            machineVectorImmLength = 3,
            machineMemoryBlockIds = S.empty
          },
      useVLMask = False,
      allowPartialVL = AllowPartialVL
    }

vconst8162 :: MachineConfig
vconst8162 =
  MachineConfig
    { scalableConfig =
        MachineScalableConfig
          { machineVectorLength = 16,
            machineMemoryBlockLengths = M.empty
          },
      baseConfig =
        MachineBaseConfig
          { machineScalarLength = 8,
            machinePointerUnit = 2,
            machineScalarImmLength = 5,
            machineVectorImmLength = 3,
            machineMemoryBlockIds = S.empty
          },
      useVLMask = False,
      allowPartialVL = AllowPartialVL
    }

vconst64128 :: MachineConfig
vconst64128 =
  MachineConfig
    { scalableConfig =
        MachineScalableConfig
          { machineVectorLength = 128,
            machineMemoryBlockLengths = M.empty
          },
      baseConfig =
        MachineBaseConfig
          { machineScalarLength = 64,
            machinePointerUnit = 8,
            machineScalarImmLength = 12,
            machineVectorImmLength = 5,
            machineMemoryBlockIds = S.empty
          },
      useVLMask = False,
      allowPartialVL = AllowPartialVL
    }

vconst64512 :: MachineConfig
vconst64512 =
  MachineConfig
    { scalableConfig =
        MachineScalableConfig
          { machineVectorLength = 512,
            machineMemoryBlockLengths = M.empty
          },
      baseConfig =
        MachineBaseConfig
          { machineScalarLength = 64,
            machinePointerUnit = 8,
            machineScalarImmLength = 12,
            machineVectorImmLength = 5,
            machineMemoryBlockIds = S.empty
          },
      useVLMask = False,
      allowPartialVL = AllowPartialVL
    }

vconst32128 :: MachineConfig
vconst32128 =
  MachineConfig
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
            machineVectorImmLength = 5,
            machineMemoryBlockIds = S.empty
          },
      useVLMask = False,
      allowPartialVL = AllowPartialVL
    }

vectorConfigE1M8 :: VectorConfig
vectorConfigE1M8 = VectorConfig {elementWidthMul = 1, lengthMul = 8}

vectorConfigE1M4 :: VectorConfig
vectorConfigE1M4 = VectorConfig {elementWidthMul = 1, lengthMul = 4}

vectorConfigE1M2 :: VectorConfig
vectorConfigE1M2 = VectorConfig {elementWidthMul = 1, lengthMul = 2}

vectorConfigE1M1 :: VectorConfig
vectorConfigE1M1 = VectorConfig {elementWidthMul = 1, lengthMul = 1}

vectorConfigE1MF8 :: VectorConfig
vectorConfigE1MF8 = VectorConfig {elementWidthMul = 1, lengthMul = 1 / 8}

vectorConfigEF2MF4 :: VectorConfig
vectorConfigEF2MF4 = VectorConfig {elementWidthMul = 1 / 2, lengthMul = 1 / 4}

vectorConfigEF2MF2 :: VectorConfig
vectorConfigEF2MF2 = VectorConfig {elementWidthMul = 1 / 2, lengthMul = 1 / 2}

vectorConfigEF2M1 :: VectorConfig
vectorConfigEF2M1 = VectorConfig {elementWidthMul = 1 / 2, lengthMul = 1}

vectorConfigEF2M2 :: VectorConfig
vectorConfigEF2M2 = VectorConfig {elementWidthMul = 1 / 2, lengthMul = 2}

vectorConfigEF2M4 :: VectorConfig
vectorConfigEF2M4 = VectorConfig {elementWidthMul = 1 / 2, lengthMul = 4}

vectorConfigEF4M8 :: VectorConfig
vectorConfigEF4M8 = VectorConfig {elementWidthMul = 1 / 4, lengthMul = 8}

vectorConfigEF4M4 :: VectorConfig
vectorConfigEF4M4 = VectorConfig {elementWidthMul = 1 / 4, lengthMul = 4}

vectorConfigEF4M2 :: VectorConfig
vectorConfigEF4M2 = VectorConfig {elementWidthMul = 1 / 4, lengthMul = 2}

vectorConfigEF4M1 :: VectorConfig
vectorConfigEF4M1 = VectorConfig {elementWidthMul = 1 / 4, lengthMul = 1}

vectorConfigEF4MF2 :: VectorConfig
vectorConfigEF4MF2 = VectorConfig {elementWidthMul = 1 / 4, lengthMul = 1 / 2}

vectorConfigEF4MF4 :: VectorConfig
vectorConfigEF4MF4 = VectorConfig {elementWidthMul = 1 / 4, lengthMul = 1 / 4}

vectorConfigEF8M8 :: VectorConfig
vectorConfigEF8M8 = VectorConfig {elementWidthMul = 1 / 8, lengthMul = 8}

vectorConfigEF8M4 :: VectorConfig
vectorConfigEF8M4 = VectorConfig {elementWidthMul = 1 / 8, lengthMul = 4}

vectorConfigEF8M2 :: VectorConfig
vectorConfigEF8M2 = VectorConfig {elementWidthMul = 1 / 8, lengthMul = 2}

vectorConfigEF8M1 :: VectorConfig
vectorConfigEF8M1 = VectorConfig {elementWidthMul = 1 / 8, lengthMul = 1}

vectorConfigEF8MF2 :: VectorConfig
vectorConfigEF8MF2 = VectorConfig {elementWidthMul = 1 / 8, lengthMul = 1 / 2}

vectorConfigEF8MF8 :: VectorConfig
vectorConfigEF8MF8 = VectorConfig {elementWidthMul = 1 / 8, lengthMul = 1 / 8}

vectorConfigEF16M1 :: VectorConfig
vectorConfigEF16M1 = VectorConfig {elementWidthMul = 1 / 16, lengthMul = 1}
