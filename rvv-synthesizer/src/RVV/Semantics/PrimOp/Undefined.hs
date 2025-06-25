{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.Undefined
  ( undefinedVectorReg,
    undefinedMask,
    undefinedVector,
  )
where

import Grisette (BV (bv))
import RVV.EvalMode (EvalMode)
import RVV.Semantics.MachineConfig
  ( MachineConfig (MachineConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.Value
  ( Mask (Mask),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    vectorNumRegisters,
  )

undefinedVectorReg :: (EvalMode mode) => MachineConfig -> VectorReg mode
undefinedVectorReg MachineConfig {scalableConfig = MachineScalableConfig {..}} =
  VectorReg (bv machineVectorLength 0) (bv machineVectorLength $ -1)

undefinedMask :: (EvalMode mode) => MachineConfig -> MaskMul -> Mask mode
undefinedMask vconst maskMultiplier = Mask maskMultiplier $ undefinedVectorReg vconst

undefinedVector :: (EvalMode mode) => MachineConfig -> VectorConfig -> Vector mode
undefinedVector vconst vectorConfig =
  Vector vectorConfig $
    replicate (vectorNumRegisters vectorConfig) (undefinedVectorReg vconst)
