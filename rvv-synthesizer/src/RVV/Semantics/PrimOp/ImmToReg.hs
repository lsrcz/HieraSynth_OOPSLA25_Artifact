{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.ImmToReg
  ( bvImmToScalar,
    immToScalar,
    bvImmToScalarForVInst,
    immToScalarForVInst,
    bvImmToVector,
    immToVector,
    fullImmToVectorBV,
    fullImmToVector,
  )
where

import Data.Bits (FiniteBits (finiteBitSize))
import Data.Ratio (denominator, numerator)
import Grisette (BV (bvSelect, bvSext, bvZext), LogicalOp (false), mrgReturn)
import Grisette.Unified (GetSomeWordN)
import RVV.Semantics.Imm (Imm, getImm)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig
      ( MachineBaseConfig,
        machineScalarImmLength,
        machineScalarLength,
        machineVectorImmLength
      ),
    MachineConfig (MachineConfig, baseConfig),
  )
import RVV.Semantics.Multiplier (WidthMul (getWidthMul))
import RVV.Semantics.PrimOp.Broadcast (broadcast, fullBroadcast)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validMachineConfig)
import RVV.Semantics.Value (Mask, Scalar (Scalar), VL, Vector)
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    vectorElementBitWidth,
  )
import RVV.Util.Context (assert)

bvImmToScalar ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  Bool ->
  WidthMul ->
  GetSomeWordN mode ->
  ctx (Scalar mode)
bvImmToScalar
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  signed
  xmul
  bv = do
    validMachineConfig vconst
    assert "Imm number for xreg does not match the bit width" $
      finiteBitSize bv == machineScalarImmLength
    let targetBitWidthRatio = fromIntegral machineScalarLength * getWidthMul xmul
    assert "Target bit width must not be fractional" $
      denominator targetBitWidthRatio == 1
    let targetBitWidth = numerator targetBitWidthRatio
    if machineScalarImmLength < targetBitWidth
      then
        mrgReturn $
          Scalar
            ((if signed then bvSext else bvZext) targetBitWidth bv)
            false
      else
        if machineScalarImmLength > targetBitWidth
          then mrgReturn $ Scalar (bvSelect 0 targetBitWidth bv) false
          else mrgReturn $ Scalar bv false

immToScalar ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  Bool ->
  WidthMul ->
  Imm mode ->
  ctx (Scalar mode)
immToScalar vconst signed xmul imm = do
  bv <- getImm (machineScalarImmLength $ baseConfig vconst) imm
  bvImmToScalar vconst signed xmul bv

bvImmToScalarForVInst ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  Bool ->
  WidthMul ->
  GetSomeWordN mode ->
  ctx (Scalar mode)
bvImmToScalarForVInst
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  signed
  xmul
  bv = do
    validMachineConfig vconst
    assert "Imm number for xreg (for vinst) does not match the bit width" $
      finiteBitSize bv == machineVectorImmLength

    let targetBitWidthRatio =
          fromIntegral machineScalarLength * getWidthMul xmul
    assert "Target bit width must not be fractional" $
      denominator targetBitWidthRatio == 1
    let targetBitWidth = numerator targetBitWidthRatio
    if machineVectorImmLength < targetBitWidth
      then
        mrgReturn $
          Scalar
            ((if signed then bvSext else bvZext) targetBitWidth bv)
            false
      else
        if machineVectorImmLength > targetBitWidth
          then mrgReturn $ Scalar (bvSelect 0 targetBitWidth bv) false
          else mrgReturn $ Scalar bv false

immToScalarForVInst ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  Bool ->
  WidthMul ->
  Imm mode ->
  ctx (Scalar mode)
immToScalarForVInst vconst signed xmul imm = do
  bv <- getImm (machineVectorImmLength $ baseConfig vconst) imm
  bvImmToScalarForVInst vconst signed xmul bv

bvImmToVector ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Bool ->
  Vector mode ->
  Mask mode ->
  VL mode ->
  GetSomeWordN mode ->
  ctx (Vector mode)
bvImmToVector
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  vectorConfig
  signed
  dest
  mask
  vl
  bv = do
    validMachineConfig vconst
    assert "Imm number for vreg does not match the bit width" $
      finiteBitSize bv == machineVectorImmLength
    let eew = vectorElementBitWidth vconst vectorConfig
    let xreg
          | machineVectorImmLength < eew =
              Scalar
                ((if signed then bvSext else bvZext) eew bv)
                false
          | machineVectorImmLength > eew = Scalar (bvSelect 0 eew bv) false
          | otherwise = Scalar bv false
    broadcast vconst vectorConfig dest mask xreg vl

immToVector ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Bool ->
  Vector mode ->
  Mask mode ->
  VL mode ->
  Imm mode ->
  ctx (Vector mode)
immToVector vconst vectorConfig signed dest mask vl imm = do
  bv <- getImm (machineVectorImmLength $ baseConfig vconst) imm
  bvImmToVector vconst vectorConfig signed dest mask vl bv

fullImmToVectorBV ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Bool ->
  GetSomeWordN mode ->
  ctx (Vector mode)
fullImmToVectorBV
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  vectorConfig
  signed
  bv = do
    validMachineConfig vconst
    assert "Imm number for vreg does not match the bit width" $
      finiteBitSize bv == machineVectorImmLength
    let eew = vectorElementBitWidth vconst vectorConfig
    let xreg
          | machineVectorImmLength < eew =
              Scalar
                ((if signed then bvSext else bvZext) eew bv)
                false
          | machineVectorImmLength > eew = Scalar (bvSelect 0 eew bv) false
          | otherwise = Scalar bv false
    fullBroadcast vconst vectorConfig xreg

fullImmToVector ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Bool ->
  Imm mode ->
  ctx (Vector mode)
fullImmToVector vconst vectorConfig signed imm = do
  bv <- getImm (machineVectorImmLength $ baseConfig vconst) imm
  fullImmToVectorBV vconst vectorConfig signed bv
