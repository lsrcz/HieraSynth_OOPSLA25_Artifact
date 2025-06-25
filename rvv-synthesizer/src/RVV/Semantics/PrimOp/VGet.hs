{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.VGet (vget, lmulTruncate, extractMask) where

import Data.Ratio (denominator, numerator)
import Grisette (BV (bv, bvConcat, bvSelect), mrgReturn)
import RVV.Semantics.MachineConfig
  ( MachineConfig (MachineConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.Multiplier (LengthMul (LengthMul), MaskMul (getMaskMul))
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint
  ( validMask,
    validMaskMul,
    validVector,
    validVectorConfig,
  )
import RVV.Semantics.Value
  ( Mask (Mask),
    Vector (Vector),
    VectorReg (VectorReg, vectorRegData, vectorRegUninitialized),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (lengthMul),
    maskNumValidElements,
  )
import RVV.Util.Context (assert)

vget ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  Int ->
  Vector mode ->
  ctx (Vector mode)
vget
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}}
  partConfig
  srcConfig
  idx
  src@(Vector _ srcRegs) = do
    validVector vconst srcConfig src
    validVectorConfig vconst partConfig
    -- assert "vset: should have the same eew" $
    --   vtypeEew vconst srcVType == vtypeEew vconst partVType
    assert "vset: part should be smaller than the source" $
      lengthMul srcConfig > lengthMul partConfig
    let LengthMul srcLMul = lengthMul srcConfig
    let LengthMul partLMul = lengthMul partConfig
    let idxBound = numerator $ srcLMul / partLMul
    assert "vset: idx out of bound" $ 0 <= idx && idx < idxBound
    if partLMul >= 1
      then do
        let regIdx = idx * numerator partLMul
        let part = take (numerator partLMul) $ drop regIdx srcRegs
        mrgReturn $ Vector partConfig part
      else do
        let divisor = denominator partLMul
        let regIdx = idx `div` divisor
        let inRegIdx = idx `mod` divisor
        let len = machineVectorLength `div` divisor
        let inRegStart = inRegIdx * len
        let partData =
              bvConcat
                (bv (machineVectorLength - len) 0)
                (bvSelect inRegStart len $ vectorRegData $ srcRegs !! regIdx)
        let partUninitialized =
              bvConcat
                (bv (machineVectorLength - len) $ -1)
                ( bvSelect inRegStart len $
                    vectorRegUninitialized $
                      srcRegs !! regIdx
                )
        mrgReturn $ Vector partConfig [VectorReg partData partUninitialized]

lmulTruncate ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  Vector mode ->
  ctx (Vector mode)
lmulTruncate vconst partConfig srcConfig = vget vconst partConfig srcConfig 0

extractMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  MaskMul ->
  Int ->
  Mask mode ->
  ctx (Mask mode)
extractMask
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}}
  partMaskMul
  srcMaskMul
  idx
  src = do
    validMask vconst srcMaskMul src
    validMaskMul vconst partMaskMul
    assert "extract_mask: source must be greater or equal to part" $
      srcMaskMul >= partMaskMul
    let partElem = maskNumValidElements vconst partMaskMul
    let maxIdx = numerator $ getMaskMul $ srcMaskMul / partMaskMul
    assert "extract_mask: idx should be less than maxIdx" $ idx < maxIdx
    let startBit = idx * partElem
    let Mask _ (VectorReg d m) = src
    let maskData = bvSelect startBit partElem d
    let maskUninitialized = bvSelect startBit partElem m
    if partElem /= machineVectorLength
      then
        mrgReturn $
          Mask partMaskMul $
            VectorReg
              (bvConcat (bv (machineVectorLength - partElem) 0) maskData)
              (bvConcat (bv (machineVectorLength - partElem) $ -1) maskUninitialized)
      else
        mrgReturn $ Mask partMaskMul $ VectorReg maskData maskUninitialized
