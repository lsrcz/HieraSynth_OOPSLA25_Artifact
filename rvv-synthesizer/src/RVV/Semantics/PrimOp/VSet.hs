{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.VSet (vset, lmulExtend, insertMask) where

import Data.Bits (FiniteBits (finiteBitSize))
import Data.Ratio (denominator, numerator)
import Grisette (BV (bvConcat, bvSelect), mrgReturn)
import RVV.Semantics.MachineConfig
  ( MachineConfig (MachineConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.Multiplier
  ( LengthMul (LengthMul),
    MaskMul (getMaskMul),
  )
import RVV.Semantics.PrimOp.Undefined (undefinedVector)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validMask, validVector)
import RVV.Semantics.Value
  ( Mask (Mask),
    Vector (Vector),
    VectorReg (VectorReg, vectorRegData, vectorRegUninitialized),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (lengthMul),
    maskNumValidElements,
    vectorNumRegisters,
  )
import RVV.Util.Context (assert)

vset ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  Int ->
  Vector mode ->
  Vector mode ->
  ctx (Vector mode)
vset
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}}
  partConfig
  destConfig
  idx
  dest@(Vector _ destRegs)
  part@(Vector _ partRegs) = do
    validVector vconst destConfig dest
    validVector vconst partConfig part
    -- assert "vset: should have the same eew" $
    --   vtypeEew vconst destVType == vtypeEew vconst partVType
    let LengthMul destLMul = lengthMul destConfig
    let LengthMul partLMul = lengthMul partConfig
    assert "vset: dest should be larger than the part" $
      destLMul > partLMul
    let idxBound = numerator $ destLMul / partLMul
    assert "vset: idx out of bound" $ 0 <= idx && idx < idxBound
    if partLMul >= 1
      then do
        let regIdx = idx * numerator partLMul
        let newDest =
              take regIdx destRegs
                ++ partRegs
                ++ drop (regIdx + vectorNumRegisters partConfig) destRegs
        mrgReturn $ Vector destConfig newDest
      else do
        let divisor = denominator partLMul
        let regIdx = idx `div` divisor
        let inRegIdx = idx `mod` divisor
        let len = machineVectorLength `div` divisor
        let inRegStart = inRegIdx * len
        let destRegToReplace = destRegs !! regIdx
        let replace destBV partBV =
              let body = bvSelect 0 len partBV
                  withLower =
                    if inRegStart /= 0
                      then bvConcat body (bvSelect 0 inRegStart destBV)
                      else body
                  withHigher =
                    if inRegStart + len == machineVectorLength
                      then withLower
                      else
                        bvConcat
                          ( bvSelect
                              (inRegStart + len)
                              (machineVectorLength - inRegStart - len)
                              destBV
                          )
                          withLower
               in withHigher
        let newDestRegDataToReplace =
              replace
                (vectorRegData destRegToReplace)
                (vectorRegData $ head partRegs)
        let newDestRegUnintializedToReplace =
              replace
                (vectorRegUninitialized destRegToReplace)
                (vectorRegUninitialized $ head partRegs)
        let newDestRegs =
              take regIdx destRegs
                ++ [ VectorReg
                       newDestRegDataToReplace
                       newDestRegUnintializedToReplace
                   ]
                ++ drop (regIdx + 1) destRegs
        mrgReturn $ Vector destConfig newDestRegs

lmulExtend ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  Vector mode ->
  ctx (Vector mode)
lmulExtend vconst partConfig destConfig partVector = do
  let destUndefined = undefinedVector vconst destConfig
  vset vconst partConfig destConfig 0 destUndefined partVector

insertMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  MaskMul ->
  Int ->
  Mask mode ->
  Mask mode ->
  ctx (Mask mode)
insertMask
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}}
  partMaskMul
  destMaskMul
  idx
  dest
  part = do
    validMask vconst destMaskMul dest
    validMask vconst partMaskMul part
    assert "insert_mask: dest must be greater or equal to part" $
      destMaskMul >= partMaskMul
    let partElem = maskNumValidElements vconst partMaskMul
    let maxIdx = numerator $ getMaskMul $ destMaskMul / partMaskMul
    assert "insert_mask: idx should be less than maxIdx" $ idx < maxIdx
    let startBit = idx * partElem
    let Mask _ (VectorReg sd su) = part
    let insertedData = bvSelect 0 partElem sd
    let insertedUninit = bvSelect 0 partElem su
    let Mask _ (VectorReg dd du) = dest
    let destPreData = bvSelect 0 startBit dd
    let destPreUninit = bvSelect 0 startBit du
    let destPostData =
          bvSelect
            (startBit + partElem)
            (finiteBitSize du - startBit - partElem)
            dd
    let destPostUninit =
          bvSelect
            (startBit + partElem)
            (finiteBitSize du - startBit - partElem)
            du
    if
      | finiteBitSize insertedData == finiteBitSize du -> mrgReturn part
      | idx == 0 ->
          mrgReturn $
            Mask destMaskMul $
              VectorReg
                (bvConcat destPostData insertedData)
                (bvConcat destPostUninit insertedUninit)
      | startBit + partElem == finiteBitSize du ->
          mrgReturn $
            Mask destMaskMul $
              VectorReg
                (bvConcat insertedData destPreData)
                (bvConcat insertedUninit destPreUninit)
      | otherwise ->
          mrgReturn $
            Mask destMaskMul $
              VectorReg
                (bvConcat (bvConcat destPostData insertedData) destPreData)
                ( bvConcat
                    (bvConcat destPostUninit insertedUninit)
                    destPreUninit
                )
