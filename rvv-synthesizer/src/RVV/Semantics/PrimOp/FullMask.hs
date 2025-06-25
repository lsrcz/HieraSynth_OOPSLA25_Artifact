{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.FullMask (fullMask) where

import Grisette (BV (bv))
import RVV.EvalMode (EvalMode)
import RVV.Semantics.MachineConfig
  ( MachineConfig (MachineConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.Value (Mask (Mask), VectorReg (VectorReg))

fullMask :: (EvalMode mode) => MachineConfig -> MaskMul -> Mask mode
fullMask MachineConfig {scalableConfig = MachineScalableConfig {..}} maskMul =
  Mask maskMul $ VectorReg (bv machineVectorLength $ -1) (bv machineVectorLength 0)
