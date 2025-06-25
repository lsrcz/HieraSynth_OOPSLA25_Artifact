{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Semantics.PrimOp.MiscMask
  ( countPopulation,
    findFirstSet,
    setMask,
    setBeforeFirst,
    setIncludingFirst,
    setOnlyFirst,
    setBeforeFirstCurSetFunc,
    setIncludingFirstCurSetFunc,
    setOnlyFirstCurSetFunc,
  )
where

import Control.Monad.State (MonadState (get, put), State, evalState)
import Data.Bits (Bits ((.&.)), FiniteBits (finiteBitSize))
import Grisette
  ( BV (bv, bvSelect),
    LogicalOp (false, symNot, true, (.&&), (.||)),
    mrgReturn,
  )
import Grisette.Unified (GetBool, GetSomeWordN, mrgIte, symBitBlast, symIte, (.<))
import Grisette.Unified.Lib.Data.Foldable (symOr)
import RVV.EvalMode (EvalMode)
import RVV.Semantics.Element
  ( MaskElement (MaskElement, maskElementData, maskElementUninitialized),
    decomposeMask,
  )
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (machineScalarLength),
    MachineConfig (baseConfig),
  )
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.PrimOp.GetVL (getVLMax)
import RVV.Semantics.PrimOp.MaskLogical (maskLogical)
import RVV.Semantics.PrimOp.Util (SemConstraint, handleMaskTailMasks)
import RVV.Semantics.SizeConstraint (validMask, validVL)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL, vlUninitialized, vlValue),
    VLValue (VLMask, VLNum),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig (maskNumValidElements, maskValidElementIndices)
import RVV.Synthesizer.Parameter.MaskLogicalOpCode
  ( MaskLogicalOpCode (MAnd),
    interpretMaskLogicalOpCode,
  )

isInvalidScalarResult ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  VL mode ->
  ctx (GetBool mode)
isInvalidScalarResult
  vconst
  maskMul
  mask@(Mask _ (VectorReg maskData maskUninitialized))
  vl@VL {vlValue = VLMask vlMask, ..} = do
    validVL vconst maskMul vl
    validMask vconst maskMul mask
    let numBits = maskNumValidElements vconst maskMul
    let maskData' = bvSelect 0 numBits maskData .&. vlMask
    maskElements <-
      decomposeMask vconst $ Mask maskMul (VectorReg maskData' maskUninitialized)
    let maskElementsIsInvalid =
          symOr $
            fmap
              (\me -> maskElementData me .&& maskElementUninitialized me)
              maskElements
    mrgReturn $ vlUninitialized .|| maskElementsIsInvalid
isInvalidScalarResult vconst maskMul mask VL {vlValue = VLNum vlNum, ..} = do
  let indices = maskValidElementIndices vconst maskMul
  maskElements <- decomposeMask vconst mask
  let maskElementsIsInvalid =
        symOr $
          zipWith
            (\me i -> i .< vlNum .&& maskElementUninitialized me)
            maskElements
            indices
  mrgReturn $ vlUninitialized .|| maskElementsIsInvalid

countPopulation ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  Mask mode ->
  VL mode ->
  ctx (Scalar mode)
countPopulation vconst maskMul mask maskToCount vl@VL {..} = do
  validMask vconst maskMul mask
  validMask vconst maskMul maskToCount
  validVL vconst maskMul vl
  vlMax <- getVLMax vconst maskMul nonePolicy
  andMask <-
    maskLogical (interpretMaskLogicalOpCode MAnd) vconst maskMul mask maskToCount vlMax
  resInvalid <- isInvalidScalarResult vconst maskMul andMask vl
  let indices = maskValidElementIndices vconst maskMul
  let xlen = machineScalarLength $ baseConfig vconst
  count <-
    case vlValue of
      VLMask vlMask -> do
        let Mask _ (VectorReg d _) = andMask
        mrgReturn $
          sum $
            fmap (\d -> symIte d (bv xlen 1 :: GetSomeWordN mode) (bv xlen 0)) $
              symBitBlast @mode $
                vlMask .&. bvSelect 0 (finiteBitSize vlMask) d
      VLNum vlNum -> do
        andMaskElements <- decomposeMask vconst andMask
        mrgReturn $
          sum $
            zipWith
              ( \(MaskElement d _) i ->
                  symIte
                    (d .&& i .< vlNum)
                    (bv xlen 1 :: GetSomeWordN mode)
                    (bv xlen 0)
              )
              andMaskElements
              indices
  mrgReturn $ Scalar count resInvalid

findFirstSet ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  Mask mode ->
  VL mode ->
  ctx (Scalar mode)
findFirstSet vconst maskMul mask maskToFind vl@VL {..} = do
  validMask vconst maskMul mask
  validMask vconst maskMul maskToFind
  validVL vconst maskMul vl
  vlMax <- getVLMax vconst maskMul nonePolicy
  andMask <-
    maskLogical (interpretMaskLogicalOpCode MAnd) vconst maskMul mask maskToFind vlMax
  resInvalid <- isInvalidScalarResult vconst maskMul andMask vl

  let indices = maskValidElementIndices vconst maskMul
  let xlen = machineScalarLength $ baseConfig vconst
  first <-
    case vlValue of
      VLMask vlMask -> do
        let Mask _ (VectorReg d _) = andMask
        mrgReturn $
          foldr (\(d, i) acc -> symIte d i acc) (bv xlen $ -1) $
            zip (symBitBlast @mode $ bvSelect 0 (finiteBitSize vlMask) d .&. vlMask) indices
      VLNum vlNum -> do
        andMaskElements <- decomposeMask vconst andMask
        mrgReturn
          $ foldr
            ( \(MaskElement d _, i) acc ->
                symIte (d .&& i .< vlNum) i acc
            )
            (bv xlen $ -1)
          $ zip
            andMaskElements
            indices
  mrgReturn $ Scalar first resInvalid

setMaskAcc ::
  (EvalMode mode) =>
  (GetBool mode -> GetBool mode) ->
  MaskElement mode ->
  State (GetBool mode, GetBool mode) (MaskElement mode)
setMaskAcc curSetFunc (MaskElement d u) = do
  (alreadySet, invalid) <- get
  let curInvalid = invalid .|| u
  put (alreadySet .|| d, curInvalid)
  return $ MaskElement (mrgIte alreadySet false (curSetFunc d)) curInvalid

setMask ::
  forall mode m.
  (SemConstraint mode m) =>
  (GetBool mode -> GetBool mode) ->
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  Mask mode ->
  Mask mode ->
  VL mode ->
  m (Mask mode)
setMask curSetFunc vconst maskMul dest mask srcMask vl = do
  validMask vconst maskMul dest
  validMask vconst maskMul mask
  validMask vconst maskMul srcMask
  validVL vconst maskMul vl
  vlMax <- getVLMax vconst maskMul nonePolicy
  andMask <-
    maskLogical (interpretMaskLogicalOpCode MAnd) vconst maskMul mask srcMask vlMax
  andMaskElements <- decomposeMask vconst andMask
  let newElements =
        flip evalState (false, false) $
          traverse (setMaskAcc curSetFunc) andMaskElements ::
          [MaskElement mode]
  handleMaskTailMasks
    vconst
    vl
    mask
    dest
    (return <$> newElements)

setBeforeFirstCurSetFunc :: (EvalMode mode) => GetBool mode -> GetBool mode
setBeforeFirstCurSetFunc = symNot

setIncludingFirstCurSetFunc :: (EvalMode mode) => GetBool mode -> GetBool mode
setIncludingFirstCurSetFunc _ = true

setOnlyFirstCurSetFunc :: (EvalMode mode) => GetBool mode -> GetBool mode
setOnlyFirstCurSetFunc = id

setBeforeFirst ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  Mask mode ->
  Mask mode ->
  VL mode ->
  ctx (Mask mode)
setBeforeFirst = setMask setBeforeFirstCurSetFunc

setIncludingFirst ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  Mask mode ->
  Mask mode ->
  VL mode ->
  ctx (Mask mode)
setIncludingFirst = setMask setIncludingFirstCurSetFunc

setOnlyFirst ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  Mask mode ->
  Mask mode ->
  VL mode ->
  ctx (Mask mode)
setOnlyFirst = setMask setOnlyFirstCurSetFunc
