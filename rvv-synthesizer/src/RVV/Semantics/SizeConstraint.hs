{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.SizeConstraint
  ( validMachineBaseConfig,
    validMachineConfig,
    validVectorConfig,
    validMaskMul,
    validMask,
    validVector,
    validScalar,
    validVL,
    validWidthMul,
    validMemoryBlock,
    validMemory,
    narrowableVectorConfig,
    validPtr,
  )
where

import Data.Bits (FiniteBits (finiteBitSize))
import qualified Data.HashMap.Lazy as M
import Data.Ratio (Ratio, denominator, numerator, (%))
import GHC.Stack (HasCallStack)
import Grisette (SomeBV (SomeBV, SomeBVLit))
import Grisette.Lib.Base (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Data.Foldable (mrgTraverse_)
import HieraSynth.Context (MonadContext)
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified (GetBool, GetSomeWordN, symBitBlast, (.<=), (.==))
import RVV.Circuit.ParallelFoldable
  ( parallelAnd,
    parallelAndl,
    parallelOrr,
    ParallelFoldMethod (Sklansky),
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Semantics.BitSizeUtil (isPower2, log2)
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
        useVLMask
      ),
    MachineScalableConfig
      ( MachineScalableConfig,
        machineMemoryBlockLengths,
        machineVectorLength
      ),
  )
import RVV.Semantics.Memory
  ( BlockId,
    Memory (Memory),
    MemoryBlock (MemoryBlock, memoryBlockData, memoryBlockUninitialized),
    Ptr (Ptr, ptrBlockId, ptrOffset, ptrWidthXlenMultiplier),
  )
import RVV.Semantics.Multiplier
  ( LengthMul (LengthMul, getLengthMul),
    MaskMul (getMaskMul),
    WidthMul (WidthMul, getWidthMul),
  )
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL),
    VLValue (VLMask, VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (VectorConfig, elementWidthMul, lengthMul),
    maskNumValidElements,
    narrowVectorConfig,
    scalarBitWidth,
    vectorElementBitWidth,
    vectorMaskMul,
    vectorNumRegisters,
  )
import RVV.Util.Context (assert)

validMachineBaseConfig :: (MonadContext ctx, HasCallStack) => MachineBaseConfig -> ctx ()
validMachineBaseConfig
  MachineBaseConfig {..}
    | machineScalarLength <= 0 = mrgThrowError "validMachineBaseConfig: xlen must be positive"
    | not $ isPower2 machinePointerUnit =
        mrgThrowError "validMachineBaseConfig: ptr_unit must be power of two"
    | not $ isPower2 machineScalarLength =
        mrgThrowError "validMachineBaseConfig: xlen must be power of two"
    | machineScalarLength `mod` machinePointerUnit /= 0 =
        mrgThrowError "validMachineBaseConfig: xlen must be a multiple of ptr_unit"
    | machineVectorImmLength > machineScalarLength =
        mrgThrowError "validMachineBaseConfig: vimm_len must be less than or equal to xlen"
    -- It turns out that this is too strict
    -- \| machineVectorImmLength < log2 machineScalarLength =
    --     mrgThrowError $
    --       "validMachineBaseConfig: vimm_len must be greater than or equal to log2(xlen), "
    --         <> "this will give weird semantics to the shifting by imm "
    --         <> "number instruction."
    | machineScalarImmLength > machineScalarLength =
        mrgThrowError $
          "validMachineBaseConfig: ximm_len must be greater than or equal to log2(xlen), "
            <> "this will give weird semantics to the shifting by imm "
            <> "number instruction."
    | machineScalarImmLength < log2 machineScalarLength =
        mrgThrowError
          "validMachineBaseConfig: vimm_len must be greater than or equal to log2(xlen)"
    | otherwise = mrgReturn ()

validMachineConfig :: (MonadContext ctx, HasCallStack) => MachineConfig -> ctx ()
validMachineConfig
  MachineConfig
    { scalableConfig = MachineScalableConfig {..},
      baseConfig = baseConfig@MachineBaseConfig {..}
    } = do
    validMachineBaseConfig baseConfig
    if
      | machineVectorLength <= 0 -> mrgThrowError "validMachineConfig: vlen must be positive"
      | not $ isPower2 machineVectorLength ->
          mrgThrowError "validMachineConfig: vlen must be power of two"
      | machineVectorLength `mod` machinePointerUnit /= 0 ->
          mrgThrowError "validMachineConfig: vlen must be a multiple of ptr_unit"
      | M.keysSet machineMemoryBlockLengths /= machineMemoryBlockIds ->
          mrgThrowError "validMachineConfig: invalid memory block ids"
      | any (\(_, v) -> v <= 0) $ M.toList machineMemoryBlockLengths ->
          mrgThrowError "validMachineConfig: memory block length must be positive"
      | any (\(_, v) -> v `mod` machinePointerUnit /= 0) $
          M.toList machineMemoryBlockLengths ->
          mrgThrowError
            "validMachineConfig: memory block length must be a multiple of ptr_unit"
      | otherwise -> mrgReturn ()

validPower2Multiplier :: (MonadContext ctx, HasCallStack) => Ratio Int -> ctx ()
validPower2Multiplier r
  | denominator r == 1 =
      assert
        ( "validPower2Multiplier: invalid ratio, denominator is 1, but "
            <> "numerator is not a power of 2: "
            <> showAsText r
        )
        (isPower2 (numerator r))
validPower2Multiplier r
  | numerator r == 1 =
      assert
        ( "validPower2Multiplier: invalid ratio, numerator is 1, but "
            <> "denominator is not a power of 2: "
            <> showAsText r
        )
        (isPower2 (denominator r))
validPower2Multiplier _ =
  mrgThrowError
    "validPower2Multiplier: invalid ratio, neither numerator nor denominator is 1"

validWidthMul :: (MonadContext ctx, HasCallStack) => MachineConfig -> WidthMul -> ctx ()
validWidthMul
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}, baseConfig = MachineBaseConfig {..}}
  widthMultiplier@(WidthMul xmul) = do
    validMachineConfig vconst
    validPower2Multiplier xmul
    res
    where
      res
        | denominator (fromIntegral machineScalarLength * xmul) /= 1 =
            assert "validWidthMul: xlen * multiplier must be an integer" False
        | xmul > 1 =
            assert
              ( "validWidthMul: multiplier must be fractional, but got"
                  <> showAsText xmul
              )
              False
        | xmul <= 0 = assert "validWidthMul: xmul must be positive" False
        | numerator xmul /= 1 =
            assert
              "validWidthMul: width multiplier must smaller or equal to 1"
              False
        | scalarBitWidth vconst widthMultiplier `mod` machinePointerUnit /= 0 =
            assert
              "validWidthMul: element bit width must be a multiple of ptr_unit"
              False
        | otherwise = mrgReturn ()

validLengthMul :: (MonadContext ctx, HasCallStack) => MachineConfig -> LengthMul -> ctx ()
validLengthMul
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}, baseConfig = MachineBaseConfig {..}}
  (LengthMul lmul) = do
    validMachineConfig vconst
    res
    where
      res
        | not (numerator lmul == 1 || denominator lmul == 1) =
            mrgThrowError "validLengthMul: emul must be a power of 2"
        | numerator lmul == 1 && not (isPower2 (denominator lmul)) =
            mrgThrowError "validLengthMul: emul must be a power of 2"
        | denominator lmul == 1 && not (isPower2 (numerator lmul)) =
            mrgThrowError "validLengthMul: emul must be a power of 2"
        | otherwise = mrgReturn ()

validMaskMul ::
  (MonadContext ctx, HasCallStack) => MachineConfig -> MaskMul -> ctx ()
validMaskMul
  MachineConfig
    { scalableConfig = MachineScalableConfig {..},
      baseConfig = MachineBaseConfig {..}
    }
  maskMultiplier = do
    validPower2Multiplier (getMaskMul maskMultiplier)
    assert
      ( "validMaskMul: maskMul too large. XLEN="
          <> showAsText machineScalarLength
          <> ", VLEN="
          <> showAsText machineVectorLength
          <> ", maskMul="
          <> showAsText maskMultiplier
      )
      $ machineScalarLength % machineVectorLength <= getMaskMul maskMultiplier
    assert
      ( "validMaskMul: maskMul too small. XLEN="
          <> showAsText machineScalarLength
          <> ", VLEN="
          <> showAsText machineVectorLength
          <> ", maskMul="
          <> showAsText maskMultiplier
      )
      $ maskMultiplier <= fromIntegral machineScalarLength

validVectorConfig :: (MonadContext ctx, HasCallStack) => MachineConfig -> VectorConfig -> ctx ()
validVectorConfig
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}, baseConfig = MachineBaseConfig {..}}
  vectorConfig@(VectorConfig {..}) = do
    validMachineConfig vconst
    validWidthMul vconst elementWidthMul
    validLengthMul vconst lengthMul
    res
    where
      ewmul = getWidthMul elementWidthMul
      lmul = getLengthMul lengthMul
      res
        | denominator (fromIntegral machineScalarLength * ewmul) /= 1 =
            mrgThrowError "validVectorConfig: xlen * multiplier must be an integer"
        | elementWidthMul > 1 =
            mrgThrowError "validVectorConfig: multiplier must be fractional"
        | vectorElementBitWidth vconst vectorConfig <= 0 =
            mrgThrowError "validVectorConfig: eew must be positive"
        | machineVectorLength `mod` vectorElementBitWidth vconst vectorConfig /= 0 =
            mrgThrowError "validVectorConfig: vlen must be a multiple of eew"
        | lmul >= 1 && vectorElementBitWidth vconst vectorConfig `mod` numerator lmul /= 0 =
            mrgThrowError $
              "validVectorConfig: EEW must be a multiple of EMUL or a mask cannot be"
                <> " stored in a single register, got EEW "
                <> showAsText (vectorElementBitWidth vconst vectorConfig)
                <> " and EMUL "
                <> showAsText lmul
        | denominator
            ( (machineVectorLength % machineScalarLength)
                * getMaskMul (vectorMaskMul vectorConfig)
            )
            /= 1 =
            mrgThrowError "validVectorConfig: fractional number of elements"
        | vectorElementBitWidth vconst vectorConfig `mod` machinePointerUnit /= 0 =
            mrgThrowError "validVectorConfig: eew must be a multiple of ptr_unit"
        | otherwise = mrgReturn ()

validVectorReg ::
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  VectorReg mode ->
  ctx ()
validVectorReg MachineConfig {scalableConfig = MachineScalableConfig {..}} (VectorReg reg uninitialized) = do
  assert "validVectorReg: inconsistent vlen" $
    finiteBitSize reg == machineVectorLength
  assert "validVectorReg: inconsistent vlen and invalid bits" $
    finiteBitSize uninitialized == machineVectorLength

validMask ::
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  ctx ()
validMask vconst maskMultiplier (Mask vmaskMultiplier reg) = do
  validMaskMul vconst vmaskMultiplier
  assert "validMask: inconsistent maskMul" $ maskMultiplier == vmaskMultiplier
  validVectorReg vconst reg

validVector ::
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  ctx ()
validVector vconst vectorConfig@(VectorConfig {..}) (Vector groupVectorConfig regs) = do
  validVectorConfig vconst vectorConfig
  assert "validVector: incorrect vtype" $ groupVectorConfig == vectorConfig
  mrgTraverse_ (validVectorReg vconst) regs
  assert "validVector: incorrect emul" $
    vectorNumRegisters vectorConfig == length regs

validScalar ::
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  WidthMul ->
  Scalar mode ->
  ctx ()
validScalar
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  xmul
  (Scalar reg _) = do
    validMachineConfig vconst
    validWidthMul vconst xmul
    assert "validScalar: invalid bit width" $
      finiteBitSize reg == scalarBitWidth vconst xmul

validVLMask ::
  forall mode ctx.
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  MaskMul ->
  GetSomeWordN mode ->
  ctx ()
validVLMask vconst maskMultiplier vlMask = do
  validMaskMul vconst maskMultiplier
  let num = maskNumValidElements vconst maskMultiplier
  assert "validVLMask: vlMask does not match the number of valid elements" $
    finiteBitSize vlMask == num
  let blasted = symBitBlast @mode vlMask
  let l1 = parallelAndl Sklansky blasted
  let l2 = parallelOrr Sklansky blasted
  let l3 = zipWith (.==) l1 l2
  let l4 = parallelAnd Sklansky l3 :: GetBool mode
  assert "validVLMask: not a prefix mask" l4

validVL ::
  forall mode ctx.
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  MaskMul ->
  VL mode ->
  ctx ()
validVL _ _ (VL _ (VLMask (SomeBVLit _)) _ _) =
  mrgThrowError $
    "validVL: VL is constructed from a VLMask literal. This might cause problems. "
      <> "Please use bv and provide the bit-widths"
validVL _ _ (VL _ (VLNum (SomeBVLit _)) _ _) =
  mrgThrowError $
    "validVL: VL is constructed from a VLNum literal. This might cause problems. "
      <> "Please use bv and provide the bit-widths"
validVL
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}, useVLMask}
  maskMultiplier
  (VL vlMaskMul (VLMask vlMask) _ _) = do
    assert "validVL: useVLMask is false, but VL is constructed from a VLMask" useVLMask
    validMaskMul vconst vlMaskMul
    assert "validVL: inconsistent maskMultiplier" $ maskMultiplier == vlMaskMul
    validVLMask vconst maskMultiplier vlMask
validVL
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}, useVLMask}
  maskMultiplier
  (VL vlMaskMul (VLNum (SomeBV vlNum)) _ _) = do
    validMaskMul vconst vlMaskMul
    assert "validVL: useVLMask is true, but VL is constructed from a VLNum" $ not useVLMask
    assert "validVL: inconsistent maskMultiplier" $
      maskMultiplier == vlMaskMul
    assert "validVL: inconsistent xlen" $ finiteBitSize vlNum == machineScalarLength
    assert
      "validVL: invalid vlNum"
      ( vlNum .<= fromIntegral (maskNumValidElements vconst maskMultiplier) ::
          GetBool mode
      )

narrowableVectorConfig :: (MonadContext ctx, HasCallStack) => MachineConfig -> VectorConfig -> ctx ()
narrowableVectorConfig vconst vectorConfig@VectorConfig {..}
  | odd (vectorElementBitWidth vconst vectorConfig) =
      mrgThrowError "narrowableVectorConfig: eew must be even"
  | otherwise = validVectorConfig vconst (narrowVectorConfig vectorConfig)

validPtr ::
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  WidthMul ->
  BlockId ->
  Ptr mode ->
  ctx ()
validPtr
  MachineConfig {scalableConfig = MachineScalableConfig {..}, baseConfig = MachineBaseConfig {..}}
  widthXlenMul
  blockId
  Ptr {..} = do
    assert "validPtr: width does not match eew" $
      widthXlenMul == ptrWidthXlenMultiplier
    assert "validPtr: invalid block id, not present in the vconst" $
      M.member ptrBlockId machineMemoryBlockLengths
    assert "validPtr: invalid block id" $ blockId == ptrBlockId
    assert "validPtr: ptr offset bit width should be xlen" $
      finiteBitSize ptrOffset == machineScalarLength

validMemoryBlock ::
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  MemoryBlock mode ->
  ctx ()
validMemoryBlock vconst@MachineConfig {..} MemoryBlock {..} = do
  validMachineConfig vconst
  res
  where
    res
      | finiteBitSize memoryBlockData
          /= finiteBitSize memoryBlockUninitialized =
          mrgThrowError $
            "validMemoryBlock: memory block data bit size must the same as the "
              <> "bit size"
      | otherwise = mrgReturn ()

validMemory ::
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  MachineConfig ->
  Memory mode ->
  ctx ()
validMemory vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}} (Memory map) = do
  validMachineConfig vconst
  assert "validMemory: invalid block ids" $
    M.keysSet map == M.keysSet machineMemoryBlockLengths
  mrgTraverse_
    ( \(blockId, block@MemoryBlock {..}) -> do
        validMemoryBlock vconst block
        assert ("validMemory: bad length for block id " <> showAsText blockId) $
          finiteBitSize memoryBlockData == machineMemoryBlockLengths M.! blockId
    )
    (M.toList map)
