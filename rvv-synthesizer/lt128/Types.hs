module Types
  ( base,
    config,
    vtypeE1M1,
    vtypeEF2MF2,
    vtypeE1MF2,
    vtypeE1MF4,
    base2,
    config2,
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
    MachineScalableConfig
      ( MachineScalableConfig,
        machineMemoryBlockLengths,
        machineVectorLength
      ),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (VectorConfig, elementWidthMul, lengthMul),
  )

base2 :: MachineBaseConfig
base2 =
  MachineBaseConfig
    { machineScalarLength = 2,
      machinePointerUnit = 1,
      machineScalarImmLength = 2,
      machineVectorImmLength = 2,
      machineMemoryBlockIds = S.empty
    }

config2 :: MachineScalableConfig
config2 =
  MachineScalableConfig
    { machineVectorLength = 8,
      machineMemoryBlockLengths = M.empty
    }

base :: MachineBaseConfig
base =
  MachineBaseConfig
    { machineScalarLength = 4,
      machinePointerUnit = 2,
      machineScalarImmLength = 4,
      machineVectorImmLength = 4,
      machineMemoryBlockIds = S.empty
    }

config :: MachineScalableConfig
config =
  MachineScalableConfig
    { machineVectorLength = 16,
      machineMemoryBlockLengths = M.empty
    }

vtypeE1M1 :: VectorConfig
vtypeE1M1 =
  VectorConfig
    { elementWidthMul = 1,
      lengthMul = 1
    }

vtypeE1MF2 :: VectorConfig
vtypeE1MF2 =
  VectorConfig
    { elementWidthMul = 1,
      lengthMul = 1 / 2
    }

vtypeE1MF4 :: VectorConfig
vtypeE1MF4 =
  VectorConfig
    { elementWidthMul = 1,
      lengthMul = 1 / 4
    }

vtypeEF2MF2 :: VectorConfig
vtypeEF2MF2 =
  VectorConfig
    { elementWidthMul = 1 / 2,
      lengthMul = 1 / 2
    }
