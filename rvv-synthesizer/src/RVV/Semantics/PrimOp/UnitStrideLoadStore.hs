{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.UnitStrideLoadStore
  ( unitStrideValidAccess,
    unitStrideLoad,
    unitStrideStore,
  )
where

import Data.Bits
  ( Bits (complement, shiftL, (.&.), (.|.)),
    FiniteBits (finiteBitSize),
  )
import qualified Data.HashMap.Lazy as M
import Grisette
  ( BV (bv, bvConcat, bvSelect, bvZext),
    LogicalOp (symNot, (.&&), (.||)),
    SymShift (symShift, symShiftNegated),
    mrgReturn,
    mrgTraverse_,
  )
import Grisette.Unified (GetBool, GetSomeWordN, symIte, (.<), (.==), (.>=))
import RVV.Semantics.BitSizeUtil (log2)
import RVV.Semantics.Element
  ( MaskElement (MaskElement),
    VectorElement (vectorElementData, vectorElementUninitialized),
    decomposeMask,
    decomposeVector,
  )
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (MachineBaseConfig, machinePointerUnit, machineScalarLength),
    MachineConfig (MachineConfig, baseConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.Memory
  ( BlockId,
    Memory (Memory, memoryBlocks),
    MemoryBlock (MemoryBlock, memoryBlockData, memoryBlockUninitialized),
    Ptr (Ptr, ptrBlockId, ptrOffset),
    getMemoryBlock,
  )
import RVV.Semantics.PrimOp.Util (SemConstraint, handleVectorTailMasks)
import RVV.Semantics.SizeConstraint (validMemory, validPtr)
import RVV.Semantics.Value
  ( Mask,
    VL (VL, vlValue),
    VLValue,
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    elementWidthMul,
    vectorElementBitWidth,
    vectorElementIndices,
    vectorNumAllElements,
    vectorNumRegisters,
  )
import RVV.Util.Context (assert)

unitStrideValidAccess ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Mask mode ->
  Int ->
  BlockId ->
  Ptr mode ->
  VLValue mode ->
  ctx ()
unitStrideValidAccess
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  vectorConfig
  mask
  lenBlock
  blockId
  ptr@Ptr {..}
  vlNum = undefined {-do
                    validPtr vconst (elementWidthMul vectorConfig) blockId ptr
                    maskElements :: [MaskElement mode] <- decomposeMask vconst mask
                    assert
                      "Not aligned"
                      ((ptrOffset .&. alignMask) .== bv machineScalarLength 0 :: GetBool mode)
                    mrgTraverse_
                      (assert "Out of bounds access" . uncurry noOutOfBoundsAtIdxMask)
                      $ zip maskElements [0 .. vectorNumAllElements vconst vectorConfig - 1]
                    where
                      alignMask = bv machineScalarLength (machinePointerUnit - 1)
                      bvLenBlock = bv machineScalarLength lenBlock
                      noOutOfBoundsAtIdxMask :: MaskElement mode -> Int -> GetBool mode
                      noOutOfBoundsAtIdxMask (MaskElement maskData uninitialized) elemIdx =
                        let elemLastPtrIdx =
                              bv
                                machineScalarLength
                                (fromIntegral $ (elemIdx + 1) * vectorElementBitWidth vconst vectorConfig - 1)
                            elemIdxBV = bv machineScalarLength elemIdx
                         in (symNot maskData .&& symNot uninitialized)
                              .|| ((elemLastPtrIdx + ptrOffset) .< bvLenBlock)
                              .|| (elemIdxBV .>= vlNum)-}

unitStrideLoad ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Memory mode ->
  Int ->
  Ptr mode ->
  VL mode ->
  ctx (Vector mode)
unitStrideLoad
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}, baseConfig = MachineBaseConfig {..}}
  vectorConfig
  dest
  mask
  memory
  blockId
  ptr@Ptr {..}
  vlreg@VL {..} = undefined {-do
                            validMemory vconst memory
                            MemoryBlock {..} <- getMemoryBlock ptrBlockId memory
                            unitStrideValidAccess
                              vconst
                              vectorConfig
                              mask
                              (finiteBitSize memoryBlockData)
                              blockId
                              ptr
                              vlValue
                            let numDataWidth = machineVectorLength * vectorNumRegisters vectorConfig
                            let numToPad = numDataWidth - finiteBitSize memoryBlockData
                            let paddedData =
                                  if numDataWidth > finiteBitSize memoryBlockData
                                    then bvConcat (bv numToPad 0) memoryBlockData
                                    else memoryBlockData
                            let paddedUninitialized =
                                  if numDataWidth > finiteBitSize memoryBlockData
                                    then
                                      bvConcat
                                        (bv numToPad $ -1)
                                        memoryBlockUninitialized
                                    else memoryBlockUninitialized
                            let extendedPtrOffset target = bvZext (finiteBitSize target) ptrOffset
                            let shiftedAndExtendedPtrOffset target =
                                  extendedPtrOffset target `shiftL` log2 machinePointerUnit
                            let shiftedData =
                                  symShiftNegated paddedData (shiftedAndExtendedPtrOffset paddedData)
                            let shiftedUninitialized =
                                  symShiftNegated
                                    paddedUninitialized
                                    (shiftedAndExtendedPtrOffset paddedUninitialized)
                            let allDataVectors =
                                  (\i -> bvSelect (i * machineVectorLength) machineVectorLength shiftedData)
                                    <$> [0 .. vectorNumRegisters vectorConfig - 1]
                            let allUninitializedVectors =
                                  (\i -> bvSelect (i * machineVectorLength) machineVectorLength shiftedUninitialized)
                                    <$> [0 .. vectorNumRegisters vectorConfig - 1]
                            let preloadedReg =
                                  Vector vectorConfig $
                                    zipWith VectorReg allDataVectors allUninitializedVectors
                            preloadedElements <- decomposeVector vconst preloadedReg
                            handleVectorTailMasks
                              vconst
                              vlreg
                              mask
                              dest
                              (mrgReturn <$> preloadedElements)-}

unitStrideStore ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Memory mode ->
  Int ->
  Ptr mode ->
  VL mode ->
  ctx (Memory mode)
unitStrideStore
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  vectorConfig
  reg
  mask
  memory@Memory {..}
  blockId
  ptr@Ptr {..}
  VL {..} = undefined {-do
                      validMemory vconst memory
                      MemoryBlock {..} <- getMemoryBlock ptrBlockId memory
                      unitStrideValidAccess
                        vconst
                        vectorConfig
                        mask
                        (finiteBitSize memoryBlockData)
                        blockId
                        ptr
                        vlValue
                      regElements <- decomposeVector vconst reg
                      let regElementsData = vectorElementData <$> regElements
                      let regElementsUninitialized = vectorElementUninitialized <$> regElements
                      maskElements :: [MaskElement mode] <- decomposeMask vconst mask
                      let indices :: [GetSomeWordN mode] = vectorElementIndices vconst vectorConfig
                      let toShouldWrite :: MaskElement mode -> GetSomeWordN mode -> GetSomeWordN mode
                          toShouldWrite (MaskElement d u) idx =
                            symIte
                              ((d .|| u) .&& (idx .< vlNum))
                              (bv (vectorElementBitWidth vconst vectorConfig) $ -1)
                              (bv (vectorElementBitWidth vconst vectorConfig) 0)
                      let shouldWrite =
                            foldl1 (flip bvConcat) $ zipWith toShouldWrite maskElements indices
                      let elemsDataToWrite =
                            foldl1 (flip bvConcat) $
                              zipWith
                                ( \ed (MaskElement _ u) ->
                                    symIte u (bv (finiteBitSize ed) 0) ed
                                )
                                regElementsData
                                maskElements
                      let elemsUninitializedToWrite =
                            foldl1 (flip bvConcat) $
                              zipWith
                                ( \eu (MaskElement _ u) ->
                                    symIte u (bv (finiteBitSize eu) $ -1) eu
                                )
                                regElementsUninitialized
                                maskElements
                      let extend v =
                            if finiteBitSize v < finiteBitSize memoryBlockData
                              then bvZext (finiteBitSize memoryBlockData) v
                              else bvSelect 0 (finiteBitSize memoryBlockData) v
                      let extendedPtr =
                            extend ptrOffset * bv (finiteBitSize memoryBlockData) machinePointerUnit
                      let extendAndShift v = extend v `symShift` extendedPtr

                      let shouldWriteExtended = extendAndShift shouldWrite
                      let elemsDataToWriteExtended = extendAndShift elemsDataToWrite
                      let elemsUninitializedToWriteExtended =
                            extendAndShift elemsUninitializedToWrite

                      let newMemoryData =
                            (shouldWriteExtended .&. elemsDataToWriteExtended)
                              .|. (complement shouldWriteExtended .&. memoryBlockData)
                      let newUninitializedData =
                            (shouldWriteExtended .&. elemsUninitializedToWriteExtended)
                              .|. (complement shouldWriteExtended .&. memoryBlockUninitialized)
                      let newMemoryBlock = MemoryBlock newMemoryData newUninitializedData
                      return $ Memory $ M.insert ptrBlockId newMemoryBlock memoryBlocks-}
