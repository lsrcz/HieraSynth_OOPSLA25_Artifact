{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.Util
  ( handleVectorTailMasks,
    handleMaskTailMasks,
    scalarToVectorElement,
    SemConstraint,
  )
where

import Data.Bits (Bits (complement, (.&.), (.|.)), FiniteBits (finiteBitSize))
import Data.List (zipWith5)
import GHC.Stack (HasCallStack)
import Grisette
  ( BV (bv, bvConcat, bvSelect, bvSext, bvZext),
    mrgReturn,
    mrgSequence,
  )
import HieraSynth.Context (MonadContext)
import Grisette.Unified
  ( GetBool,
    GetSomeWordN,
    mrgIf,
    symIte,
    (.==),
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Semantics.Element
  ( MaskElement,
    VectorElement (VectorElement),
    composeMask,
    composeVector,
    maskToVectorMask,
    vlToMaskMask,
    vlToVectorMask,
  )
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (machineScalarLength),
    MachineConfig (baseConfig, scalableConfig),
    MachineScalableConfig (machineVectorLength),
  )
import RVV.Semantics.Multiplier (WidthMul)
import RVV.Semantics.Policy
  ( Policy (maskPolicy, tailPolicy),
    mu,
    tu,
  )
import RVV.Semantics.PrimOp.Undefined (undefinedMask, undefinedVector)
import RVV.Semantics.SizeConstraint (validScalar)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL, vlMaskMul, vlPolicy, vlUninitialized),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    maskNumValidElements,
    vectorElementBitWidth,
    vectorNumAllElements,
    vectorNumValidElements,
  )

type SemConstraint mode ctx =
  ( MonadContext ctx,
    MonadEvalMode mode ctx
  )

handleVectorTailMasks ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  MachineConfig ->
  VL mode ->
  Mask mode ->
  Vector mode ->
  [ctx (VectorElement mode)] ->
  ctx (Vector mode)
handleVectorTailMasks
  vconst
  vlreg@VL {..}
  mask
  dest@(Vector vectorConfig _)
  results = do
    let d = undefinedVector @mode vconst vectorConfig
    tailDest <- mrgIf (tailPolicy vlPolicy .== tu :: GetBool mode) (return dest) (return d)
    maskDest <- mrgIf (maskPolicy vlPolicy .== mu :: GetBool mode) (return dest) (return d)
    vlVectorMask <- vlToVectorMask vconst vlreg vectorConfig
    maskVectorMask <- maskToVectorMask vconst mask vectorConfig
    let numValidElements = vectorNumValidElements vconst vectorConfig
    let numAllElements = vectorNumAllElements vconst vectorConfig
    allResults <- mrgSequence $ take numValidElements results
    let allElementsPadding =
          replicate (numAllElements - length allResults) $
            VectorElement
              (bv (vectorElementBitWidth vconst vectorConfig) 0)
              (bv (vectorElementBitWidth vconst vectorConfig) (-1))
    originalVector <- composeVector vconst vectorConfig (allResults ++ allElementsPadding)

    let combineData vlMask maskMask tailDest maskDest original =
          let select = bvSelect 0 (finiteBitSize vlMask)
              padExtraTail =
                if finiteBitSize vlMask == finiteBitSize tailDest
                  then id
                  else
                    bvConcat
                      ( bvSelect
                          (finiteBitSize vlMask)
                          (finiteBitSize tailDest - finiteBitSize vlMask)
                          tailDest
                      )
           in padExtraTail $
                (vlMask .&. select maskMask .&. select original :: GetSomeWordN mode)
                  .|. (vlMask .&. complement (select maskMask) .&. select maskDest)
                  .|. (complement vlMask .&. select tailDest)
    let combineUninitialized vlMaskD vlMaskU maskMaskD maskMaskU tailDestU maskDestU originalU =
          let select = bvSelect 0 (finiteBitSize vlMaskD)
              padExtraTail =
                if finiteBitSize vlMaskD == finiteBitSize tailDestU
                  then id
                  else
                    bvConcat
                      ( bvSelect
                          (finiteBitSize vlMaskD)
                          (finiteBitSize tailDestU - finiteBitSize vlMaskD)
                          tailDestU
                      )
           in padExtraTail $
                vlMaskU
                  .|. (vlMaskD .&. select maskMaskU)
                  .|. (vlMaskD .&. select maskMaskD .&. select originalU)
                  .|. (vlMaskD .&. complement (select maskMaskD) .&. select maskDestU)
                  .|. (complement vlMaskD .&. select tailDestU)
    let combineVReg
          (VectorReg vlMaskD vlMaskU)
          (VectorReg maskMaskD maskMaskU)
          (VectorReg tailDestD tailDestU)
          (VectorReg maskDestD maskDestU)
          (VectorReg originalD originalU) =
            VectorReg
              (combineData vlMaskD maskMaskD tailDestD maskDestD originalD)
              (combineUninitialized vlMaskD vlMaskU maskMaskD maskMaskU tailDestU maskDestU originalU)
    let combineVector
          (Vector _ vlRegs)
          (Vector _ maskRegs)
          (Vector _ tailDestRegs)
          (Vector _ maskDestRegs)
          (Vector _ originalRegs) =
            Vector vectorConfig $
              zipWith5 combineVReg vlRegs maskRegs tailDestRegs maskDestRegs originalRegs
    mrgReturn $ combineVector vlVectorMask maskVectorMask tailDest maskDest originalVector

handleMaskTailMasks ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VL mode ->
  Mask mode ->
  Mask mode ->
  [ctx (MaskElement mode)] ->
  ctx (Mask mode)
handleMaskTailMasks vconst vlreg@VL {..} mask dest results = do
  let d = undefinedMask @mode vconst vlMaskMul
  maskDest <- mrgIf (maskPolicy vlPolicy .== mu :: GetBool mode) (return dest) (return d)
  vlMaskMask <- vlToMaskMask vconst vlreg
  let numValidElements = maskNumValidElements vconst vlMaskMul
  let vlen = machineVectorLength $ scalableConfig vconst
  allElements <- mrgSequence $ take numValidElements results
  originalMask <-
    composeMask vconst vlMaskMul allElements

  let combineData vlMask maskMask maskDest original =
        let select = bvSelect 0 (finiteBitSize vlMask)
            padExtraTail =
              if finiteBitSize vlMask == vlen
                then id
                else
                  bvConcat
                    (bv (vlen - finiteBitSize vlMask) 0)
         in padExtraTail $
              (vlMask .&. select maskMask .&. select original :: GetSomeWordN mode)
                .|. (vlMask .&. complement (select maskMask) .&. select maskDest)
  let combineUninitialized vlMaskD vlMaskU maskMaskD maskMaskU maskDestU originalU =
        let select = bvSelect 0 (finiteBitSize vlMaskD)
            padExtraTail =
              if finiteBitSize vlMaskD == vlen
                then id
                else
                  bvConcat
                    ( bv
                        (vlen - finiteBitSize vlMaskD)
                        (-1)
                    )
         in padExtraTail $
              vlMaskU
                .|. (vlMaskD .&. select maskMaskU)
                .|. (vlMaskD .&. select maskMaskD .&. select originalU)
                .|. (vlMaskD .&. complement (select maskMaskD) .&. select maskDestU)
                .|. complement vlMaskD
  let combineVReg
        (VectorReg vlMaskD vlMaskU)
        (VectorReg maskMaskD maskMaskU)
        (VectorReg maskDestD maskDestU)
        (VectorReg originalD originalU) =
          VectorReg
            (combineData vlMaskD maskMaskD maskDestD originalD)
            (combineUninitialized vlMaskD vlMaskU maskMaskD maskMaskU maskDestU originalU)
  let combineMask
        (Mask _ vlReg)
        (Mask _ maskReg)
        (Mask _ maskDestReg)
        (Mask _ originalReg) =
          Mask vlMaskMul $
            combineVReg vlReg maskReg maskDestReg originalReg
  mrgReturn $ combineMask vlMaskMask mask maskDest originalMask

scalarToVectorElement ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Bool ->
  WidthMul ->
  Scalar mode ->
  ctx (VectorElement mode)
scalarToVectorElement vconst vectorConfig signed xmul scalar@(Scalar v u) = do
  validScalar vconst xmul scalar
  let eew = vectorElementBitWidth vconst vectorConfig
  let value = case compare (machineScalarLength $ baseConfig vconst) eew of
        LT -> (if signed then bvSext else bvZext) eew v
        EQ -> v
        GT -> bvSelect 0 eew v
  let uninit = symIte u (bv eew $ -1) (bv eew 0)
  mrgReturn $ VectorElement value uninit
