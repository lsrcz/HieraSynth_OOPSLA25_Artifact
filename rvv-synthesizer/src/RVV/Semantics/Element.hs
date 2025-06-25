{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.Element
  ( VectorElement (..),
    vectorElementIsInitialized,
    MaskElement (..),
    maskElementIsInitialized,
    decomposeVector,
    composeVector,
    decomposeMask,
    composeMask,
    maskToVectorMask,
    vlToVectorMask,
    buildTreeMux,
    duplicateMask,
    vlToMaskMask,
    vlValueToVLMask,
    vlValueToVLNum,
  )
where

import Data.Bits (Bits (complement), FiniteBits (finiteBitSize))
import Data.Foldable (traverse_)
import Data.List.Split (chunksOf)
import GHC.Stack (HasCallStack)
import Grisette
  ( BV (bv, bvConcat, bvSelect, bvSext),
    LogicalOp (symNot),
    SimpleMergeable,
    ToSym (toSym),
    deriveWith,
    mrgReturn,
  )
import HieraSynth.Context (MonadContext)
import Grisette.Unified
  ( GetBool,
    GetSomeWordN,
    UnifiedSimpleMergeable,
    symBitBlast,
    symFromBits,
    symIte,
    (.==),
  )
import RVV.EvalMode (EvalMode, MonadEvalMode)
import RVV.Semantics.BitSizeUtil (log2)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (machineScalarLength),
    MachineConfig (MachineConfig, baseConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.SizeConstraint (validMask, validVL, validVector, validVectorConfig)
import RVV.Semantics.Value
  ( Mask (Mask),
    VL (VL, vlMaskMul, vlUninitialized, vlValue),
    VLValue (VLMask, VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
    fromConcatenated,
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    maskNumValidElements,
    vectorElementBitWidth,
    vectorMaskMul,
    vectorNumRegisters,
    vectorNumValidBits,
    vectorNumValidElements,
  )
import RVV.Util.Context (assert)
import RVV.Util.Derive (deriveBasic, firstModeDeriveConfig)

data VectorElement mode = VectorElement
  { vectorElementData :: GetSomeWordN mode,
    vectorElementUninitialized :: GetSomeWordN mode
  }

deriveBasic firstModeDeriveConfig [''VectorElement]

vectorElementIsInitialized :: (EvalMode mode) => VectorElement mode -> GetBool mode
vectorElementIsInitialized (VectorElement _ uninitialized) = uninitialized .== 0

data MaskElement mode = MaskElement
  { maskElementData :: GetBool mode,
    maskElementUninitialized :: GetBool mode
  }

deriveBasic firstModeDeriveConfig [''MaskElement]
deriveWith
  firstModeDeriveConfig
  [''MaskElement]
  [''SimpleMergeable, ''UnifiedSimpleMergeable]

maskElementIsInitialized :: (EvalMode mode) => MaskElement mode -> GetBool mode
maskElementIsInitialized (MaskElement _ uninitialized) = symNot uninitialized

decomposeVector ::
  forall mode ctx.
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  Vector mode ->
  ctx [VectorElement mode]
decomposeVector
  vconst@MachineConfig {..}
  group@(Vector vectorConfig regs) = do
    validVector vconst vectorConfig group
    let ungroupedRegs = ungroupVectorReg (vectorElementBitWidth vconst vectorConfig) <$> regs
    mrgReturn $ concat ungroupedRegs
    where
      ungroupBV :: Int -> GetSomeWordN mode -> [GetSomeWordN mode]
      ungroupBV eew v
        | finiteBitSize v == eew = [v]
        | finiteBitSize v < eew = error "BUG"
        | otherwise =
            bvSelect 0 eew v
              : ungroupBV eew (bvSelect eew (finiteBitSize v - eew) v)

      ungroupVectorReg :: Int -> VectorReg mode -> [VectorElement mode]
      ungroupVectorReg eew (VectorReg regData uninitialized) =
        zipWith
          VectorElement
          (ungroupBV eew regData)
          (ungroupBV eew uninitialized)

validVectorElement ::
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorElement mode ->
  ctx ()
validVectorElement
  vconst
  vectorConfig
  VectorElement {..} = do
    assert "validVectorElement: inconsistent vlen" $
      finiteBitSize vectorElementData == vectorElementBitWidth vconst vectorConfig
    assert "validVectorElement: inconsistent vlen and invalid bits" $
      finiteBitSize vectorElementUninitialized == vectorElementBitWidth vconst vectorConfig

composeVector ::
  forall mode ctx.
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  VectorConfig ->
  [VectorElement mode] ->
  ctx (Vector mode)
composeVector
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}}
  vectorConfig
  vs = do
    validVectorConfig vconst vectorConfig
    assert "group: incorrect number of elements" $
      length vs
        == vectorNumRegisters vectorConfig
          * (machineVectorLength `div` vectorElementBitWidth vconst vectorConfig)
    traverse_ (validVectorElement vconst vectorConfig) vs
    mrgReturn . Vector vectorConfig $
      groupVectorReg
        <$> chunksOf (machineVectorLength `div` vectorElementBitWidth vconst vectorConfig) vs
    where
      groupBV :: [GetSomeWordN mode] -> GetSomeWordN mode
      groupBV = foldl1 bvConcat . reverse
      groupVectorReg :: [VectorElement mode] -> VectorReg mode
      groupVectorReg elems =
        VectorReg
          (groupBV $ vectorElementData <$> elems)
          (groupBV $ vectorElementUninitialized <$> elems)

decomposeMask ::
  forall mode ctx.
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  MachineConfig ->
  Mask mode ->
  ctx [MaskElement mode]
decomposeMask
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}}
  mask@(Mask maskMul (VectorReg regData regUninitialized)) = do
    validMask vconst maskMul mask
    mrgReturn $
      take (maskNumValidElements vconst maskMul) $
        zipWith
          MaskElement
          (symBitBlast regData)
          (symBitBlast regUninitialized)

maskToVectorMask ::
  forall mode ctx.
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  MachineConfig ->
  Mask mode ->
  VectorConfig ->
  ctx (Vector mode)
maskToVectorMask vconst mask@(Mask _ (VectorReg regData regUninitialized)) vectorConfig = do
  validMask vconst (vectorMaskMul vectorConfig) mask
  let eew = vectorElementBitWidth vconst vectorConfig
  let vlen = machineVectorLength $ scalableConfig vconst
  let elementAt pos v = bvSext eew (bvSelect pos 1 v)
  let vecElementAt pos =
        VectorElement (elementAt pos regData) (elementAt pos regUninitialized)
  let num = maskNumValidElements vconst (vectorMaskMul vectorConfig)
  let vecElements = vecElementAt <$> [0 .. num - 1]
  let singleRegNumElements = vlen `div` eew
  if length vecElements >= singleRegNumElements
    then
      composeVector vconst vectorConfig vecElements
    else
      composeVector vconst vectorConfig $
        vecElements
          ++ replicate
            (singleRegNumElements - length vecElements)
            (VectorElement (bv eew 0) (bv eew 0))

buildTreeMux ::
  forall mode.
  (EvalMode mode) =>
  (Int -> Int -> Maybe (GetSomeWordN mode)) ->
  (Int -> Int -> Maybe (GetSomeWordN mode)) ->
  (Int -> GetSomeWordN mode) ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  Int ->
  Int ->
  GetSomeWordN mode
buildTreeMux leftPad rightPad final largeResult scalar elementBits numElements =
  buildLargeGuard $ buildTree 0 (numElements * elementBits) (log2 numElements - 1)
  where
    largeStartBit = log2 numElements
    largeNumBits = finiteBitSize scalar - largeStartBit
    buildLargeGuard v =
      if largeNumBits <= 0
        then v
        else
          symIte
            (bvSelect largeStartBit largeNumBits scalar .== bv largeNumBits 0 :: GetBool mode)
            v
            largeResult
    buildTree :: Int -> Int -> Int -> GetSomeWordN mode
    buildTree leftBound rightBound bit
      | bit == -1 = final leftBound
      | otherwise =
          let mid = (leftBound + rightBound) `div` 2
              leftSub = buildTree leftBound mid (bit - 1)
              rightSub = buildTree mid rightBound (bit - 1)
              leftToPad = leftPad mid rightBound
              padLeft v = case leftToPad of
                Nothing -> v
                Just padVal -> bvConcat padVal v
              rightToPad = rightPad leftBound mid -- rightBound
              padRight v = case rightToPad of
                Nothing -> v
                Just padVal -> bvConcat v padVal
           in if bit >= finiteBitSize scalar
                then padLeft leftSub
                else
                  symIte
                    (bvSelect bit 1 scalar .== bv 1 0 :: GetBool mode)
                    (padLeft leftSub)
                    (padRight rightSub)

duplicateMask :: (EvalMode mode) => GetSomeWordN mode -> Int -> GetSomeWordN mode
duplicateMask v n =
  let elementAt pos = bvSext n (bvSelect pos 1 v)
   in foldr1 bvConcat $ reverse (elementAt <$> [0 .. finiteBitSize v - 1])

vlToVectorMask ::
  forall mode ctx.
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  MachineConfig ->
  VL mode ->
  VectorConfig ->
  ctx (Vector mode)
vlToVectorMask vconst vl@VL {vlValue = VLMask vlMask, ..} vectorConfig = do
  validVL vconst vlMaskMul vl
  let eew = vectorElementBitWidth vconst vectorConfig
  let numBits = vectorNumValidBits vconst vectorConfig
  let d = duplicateMask vlMask eew
  let u = symIte vlUninitialized (bv numBits $ -1) (bv numBits 0)
  mrgReturn $ fromConcatenated vconst vectorConfig False d u
vlToVectorMask vconst vl@VL {vlValue = VLNum vlNum, ..} vectorConfig = do
  validVL vconst vlMaskMul vl
  let eew = vectorElementBitWidth vconst vectorConfig
  let numBits = vectorNumValidBits vconst vectorConfig
  let d =
        buildTreeMux
          (\l r -> Just $ bv (r - l) 0)
          (\l r -> Just $ bv (r - l) $ -1)
          (const $ bv eew 0)
          (bv numBits $ -1)
          vlNum
          eew
          (vectorNumValidElements vconst vectorConfig)
  let u = symIte vlUninitialized (bv numBits $ -1) (bv numBits 0)
  mrgReturn $ fromConcatenated vconst vectorConfig False d u

vlToMaskMask ::
  forall mode ctx.
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  MachineConfig ->
  VL mode ->
  ctx (Mask mode)
vlToMaskMask vconst vl@VL {vlValue = VLMask vlMask, ..} = do
  validVL vconst vlMaskMul vl
  let numBits = maskNumValidElements vconst vlMaskMul
  let complemented = complement vlMask
  let u = symIte vlUninitialized (bv numBits $ -1) complemented
  mrgReturn $ Mask vlMaskMul $ VectorReg vlMask u
vlToMaskMask vconst vl@VL {vlValue = VLNum vlNum, ..} = do
  validVL vconst vlMaskMul vl
  let numBits = maskNumValidElements vconst vlMaskMul
  let build invalidValue validValue =
        buildTreeMux
          (\l r -> Just $ bv (r - l) invalidValue)
          (\l r -> Just $ bv (r - l) validValue)
          (const $ bv 1 invalidValue)
          (bv numBits validValue)
          vlNum
          1
          numBits
  let d = build 0 (-1)
  let u0 = build (-1) 0
  let u = symIte vlUninitialized (bv numBits $ -1) u0
  mrgReturn $ Mask vlMaskMul $ VectorReg d u

vlValueToVLMask ::
  forall mode.
  (EvalMode mode) =>
  MachineConfig ->
  MaskMul ->
  VLValue mode ->
  VLValue mode
vlValueToVLMask _ _ (VLMask vlMask) = VLMask vlMask
vlValueToVLMask vconst maskMul (VLNum vlNum) =
  let numBits = maskNumValidElements vconst maskMul
      build invalidValue validValue =
        buildTreeMux
          (\l r -> Just $ bv (r - l) invalidValue)
          (\l r -> Just $ bv (r - l) validValue)
          (const $ bv 1 invalidValue)
          (bv numBits validValue)
          vlNum
          1
          numBits
      d = build 0 (-1)
   in VLMask d

vlValueToVLNum ::
  forall mode.
  (EvalMode mode) =>
  MachineConfig ->
  VLValue mode ->
  VLValue mode
vlValueToVLNum vconst (VLMask vlMask) =
  let xlen = machineScalarLength $ baseConfig vconst
      bits = symBitBlast @mode vlMask
      num = sum $ fmap (\d -> symIte d (bv xlen 1) (bv xlen 0)) bits
   in VLNum num
vlValueToVLNum _ (VLNum vlNum) = VLNum vlNum

-- 5.3 mask are always tail agnostic
composeMask ::
  forall mode ctx.
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  MachineConfig ->
  MaskMul ->
  [MaskElement mode] ->
  ctx (Mask mode)
composeMask vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}} maskMul elems = do
  assert
    "composeMask: incorrect number of elements"
    (length elems == maskNumValidElements vconst maskMul)
  let paddedElements =
        elems
          ++ replicate
            (machineVectorLength - length elems)
            (MaskElement (toSym False) (toSym True))
  let resultData = symFromBits $ maskElementData <$> paddedElements
  let resultUninitialized = symFromBits $ maskElementUninitialized <$> paddedElements
  return $ Mask maskMul $ VectorReg resultData resultUninitialized
