{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.GetVL
  ( unsafeComputeVL,
    getVL,
    getVLMax,
    getVLenB,
  )
where

import Data.Bits (Bits (shiftR), FiniteBits (finiteBitSize))
import GHC.Stack (HasCallStack)
import Grisette (BV (bv), LogicalOp (false), mrgReturn)
import Grisette.Unified (GetBool, GetSomeWordN, symIte, (.<), (.<=))
import RVV.EvalMode (EvalMode)
import RVV.Semantics.Element (vlValueToVLMask)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (MachineBaseConfig, machinePointerUnit, machineScalarLength),
    MachineConfig (MachineConfig, baseConfig, scalableConfig, useVLMask),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.Policy (Policy)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validMaskMul, validScalar)
import RVV.Semantics.Value
  ( Scalar (Scalar, scalarUninitialized),
    VL (VL, vlMaskMul, vlPolicy, vlUninitialized, vlValue),
    VLValue (VLNum),
  )
import RVV.Semantics.VectorConfig
  ( maskNumValidElements,
  )

-- Per RVV.Semantics.spec 6.3
-- 1. vl = AVL if AVL <= VLMAX
-- 2. cell(AVL / 2) <= vl <= VLMAX if AVL < 2 * VLMAX
-- 3. vl = VLMAX if AVL >= 2 * VLMAX
-- 4. Deterministic on any given implementation for same input AVL and VLMAX
--    values
-- 5. These specific properties follow from the prior rules:
--    a. vl = 0 if AVL = 0
--    b. vl > 0 if AVL > 0
--    c. vl <= VLMAX
--    d. vl <= AVL
--    e. a value read from vl when used as the AVL argument to vset{i}vl{i}
--       results in the same value in vl, provided the resultant VLMAX equals
--       the value of VLMAX at the time that vl was read.
--
-- This will not handle the case where the scalar is uninitialized
unsafeComputeVL ::
  forall mode.
  (EvalMode mode) =>
  Int ->
  Scalar mode ->
  GetSomeWordN mode
unsafeComputeVL vlmax (Scalar avl _) =
  let vlmaxSomeBV = bv (finiteBitSize avl) vlmax
   in symIte (avl .<= vlmaxSomeBV :: GetBool mode) avl $
        symIte
          (avl .< (vlmaxSomeBV + vlmaxSomeBV) :: GetBool mode)
          ((avl + bv (finiteBitSize avl) 1) `shiftR` 1)
          vlmaxSomeBV

getVL ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  MachineConfig ->
  MaskMul ->
  Scalar mode ->
  Policy mode ->
  ctx (VL mode)
getVL vconst maskMul xreg policy = do
  validMaskMul vconst maskMul
  validScalar vconst 1 xreg
  let vlmax = maskNumValidElements vconst maskMul
  let vlNum = VLNum $ unsafeComputeVL vlmax xreg
  let vl = if useVLMask vconst then vlValueToVLMask vconst maskMul vlNum else vlNum
  mrgReturn $
    VL
      { vlMaskMul = maskMul,
        vlPolicy = policy,
        vlValue = vl,
        vlUninitialized = scalarUninitialized xreg
      }

getVLMax ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  MachineConfig ->
  MaskMul ->
  Policy mode ->
  ctx (VL mode)
getVLMax vconst@MachineConfig {baseConfig = MachineBaseConfig {..}} maskMul = do
  let vlmax = maskNumValidElements vconst maskMul
  getVL vconst maskMul (Scalar (bv machineScalarLength vlmax) false)

getVLenB ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  MachineConfig ->
  ctx (Scalar mode)
getVLenB
  MachineConfig {baseConfig = MachineBaseConfig {..}, scalableConfig = MachineScalableConfig {..}} = do
    return $ Scalar (bv machineScalarLength $ machineVectorLength `div` machinePointerUnit) false
