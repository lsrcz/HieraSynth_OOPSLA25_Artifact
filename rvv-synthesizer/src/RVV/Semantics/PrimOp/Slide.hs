{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.Slide
  ( unsafeVectorRegShiftLeft,
    slideup,
    unsafeVectorRegShiftRight,
    slidedown,
    slide1up,
    slide1down,
  )
where

import Data.Bits (Bits (complement, shiftL, (.&.), (.|.)), FiniteBits (finiteBitSize))
import Grisette
  ( BV (bv, bvConcat, bvSelect, bvZext),
    LogicalOp (false),
    SymShift (symShift, symShiftNegated),
    mrgReturn,
    mrgTraverse,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified (GetBool, GetSomeWordN, mrgIf, symIte, (.<))
import RVV.Semantics.BitSizeUtil (log2)
import RVV.Semantics.Element
  ( decomposeVector,
    vlToVectorMask,
  )
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (MachineBaseConfig, machineScalarLength),
    MachineConfig (MachineConfig, baseConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.PrimOp.Undefined (undefinedVector)
import RVV.Semantics.PrimOp.Util
  ( SemConstraint,
    handleVectorTailMasks,
    scalarToVectorElement,
  )
import RVV.Semantics.SizeConstraint (validScalar, validVL, validVector)
import RVV.Semantics.Value
  ( Mask,
    Scalar (Scalar),
    VL (VL, vlPolicy, vlUninitialized, vlValue),
    Vector (Vector),
    VectorReg (VectorReg, vectorRegData, vectorRegUninitialized),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    vectorElementBitWidth,
    vectorElementIndices,
    vectorMaskMul,
  )
import RVV.Util.Context (assert)

splitByVlen ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  Int ->
  GetSomeWordN mode ->
  ctx [GetSomeWordN mode]
splitByVlen vlen bv
  | finiteBitSize bv == vlen = mrgReturn [bv]
  | finiteBitSize bv < vlen =
      mrgThrowError "The bitwidth of the vector isn't a multiple of vlen."
  | otherwise = do
      tl <- splitByVlen vlen (bvSelect vlen (finiteBitSize bv - vlen) bv)
      mrgReturn $ bvSelect 0 vlen bv : tl

-- This will not handle the case where the vl is uninitialized
unsafeVectorRegShift ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  (GetSomeWordN mode -> GetSomeWordN mode -> GetSomeWordN mode) ->
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Scalar mode ->
  ctx (Vector mode)
unsafeVectorRegShift
  shiftFunc
  vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}, baseConfig = MachineBaseConfig {..}}
  vectorConfig
  group@(Vector _ regs)
  xreg@(Scalar offset _) = do
    validVector vconst vectorConfig group
    validScalar vconst 1 xreg
    assert "unsafeVectorRegShift: pointer should have xlen bits" $
      finiteBitSize offset == machineScalarLength
    let concatedRegData = foldl1 (flip bvConcat) $ vectorRegData <$> regs
    let concatedRegUninitialized =
          foldl1 (flip bvConcat) $ vectorRegUninitialized <$> regs
    let extendedOffset =
          bvZext (finiteBitSize concatedRegData) $
            shiftL offset $
              log2 $
                vectorElementBitWidth vconst vectorConfig
    let shiftedData = concatedRegData `shiftFunc` extendedOffset
    let shiftedUninitialized =
          concatedRegUninitialized `shiftFunc` extendedOffset
    splittedShiftedData <- splitByVlen machineVectorLength shiftedData
    splittedShiftedUninitialized <-
      splitByVlen machineVectorLength shiftedUninitialized
    return $
      Vector vectorConfig $
        zipWith VectorReg splittedShiftedData splittedShiftedUninitialized

-- This will not handle the case where the vl is uninitialized
unsafeVectorRegShiftLeft ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Scalar mode ->
  ctx (Vector mode)
unsafeVectorRegShiftLeft = unsafeVectorRegShift symShift

-- This will not handle the case where the vl is uninitialized
unsafeVectorRegShiftRight ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Scalar mode ->
  ctx (Vector mode)
unsafeVectorRegShiftRight = unsafeVectorRegShift symShiftNegated

slideup ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Vector mode ->
  Scalar mode ->
  VL mode ->
  ctx (Vector mode)
slideup
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  vectorConfig
  dest
  mask
  src
  xreg@(Scalar offset xuninit)
  vlreg = do
    assert "unsafeVectorRegShift: pointer should have xlen bits" $
      finiteBitSize offset == machineScalarLength
    destElements <- decomposeVector vconst dest
    shiftedSrc <- unsafeVectorRegShiftLeft vconst vectorConfig src xreg
    srcElements <- decomposeVector vconst shiftedSrc
    let indices :: [GetSomeWordN mode] = vectorElementIndices vconst vectorConfig
    allElements <-
      mrgTraverse
        ( \(idx, destElement, srcElement) ->
            mrgIf (idx .< offset :: GetBool mode) (mrgReturn destElement) $
              mrgReturn srcElement
        )
        $ zip3
          indices
          destElements
          srcElements
    mrgIf xuninit (return $ undefinedVector vconst vectorConfig) $
      handleVectorTailMasks vconst vlreg mask dest (mrgReturn <$> allElements)

{-validVL vconst (vectorMaskMul vectorConfig) vlreg
let numBits = vectorNumValidBits vconst vectorConfig
let destd = bvSelect 0 numBits $ concatData dest
let destu = bvSelect 0 numBits $ concatUninitialized dest
shiftedSrc <- unsafeVectorRegShiftLeft vconst vectorConfig src xreg
let srcd = bvSelect 0 numBits $ concatData shiftedSrc
let srcu = bvSelect 0 numBits $ concatUninitialized shiftedSrc
let eew = vectorElementBitWidth vconst vectorConfig

srcd' <-
  buildTreeMux
    vconst
    (\l r -> Just $ bvSelect l (r - l) srcd)
    (\l r -> Just $ bvSelect l (r - l) destd)
    (\l -> bvSelect l eew srcd)
    destd
    xreg
    vectorConfig
srcu' <-
  buildTreeMux
    vconst
    (\l r -> Just $ bvSelect l (r - l) srcu)
    (\l r -> Just $ bvSelect l (r - l) destu)
    (\l -> bvSelect l eew srcu)
    destu
    xreg
    vectorConfig
let newSrc =
      fromConcatenated
        vconst
        vectorConfig
        True
        srcd'
        (symIte xuninit (bv (finiteBitSize srcu') $ -1) srcu')
srcElements <- decomposeVector vconst newSrc
handleVectorTailMasks vconst vlreg mask dest (mrgReturn <$> srcElements)-}

slidedown ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Vector mode ->
  Scalar mode ->
  VL mode ->
  ctx (Vector mode)
slidedown vconst vectorConfig dest mask src xreg vlreg = do
  shiftedSrc <- unsafeVectorRegShiftRight vconst vectorConfig src xreg
  srcElements <- decomposeVector vconst shiftedSrc
  handleVectorTailMasks vconst vlreg mask dest (mrgReturn <$> srcElements)

slide1up ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Vector mode ->
  Scalar mode ->
  VL mode ->
  ctx (Vector mode)
slide1up vconst vectorConfig dest mask src xreg@(Scalar value _) vlreg = do
  validScalar vconst (elementWidthMul vectorConfig) xreg
  assert "slide1up: value should have eew bits" $
    finiteBitSize value == vectorElementBitWidth vconst vectorConfig
  srcElements <- decomposeVector vconst src
  scalarElement <-
    scalarToVectorElement
      vconst
      vectorConfig
      True
      (elementWidthMul vectorConfig)
      xreg
  handleVectorTailMasks
    vconst
    vlreg
    mask
    dest
    (mrgReturn <$> scalarElement : srcElements)

slide1down ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Vector mode ->
  Scalar mode ->
  VL mode ->
  ctx (Vector mode)
slide1down
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  vectorConfig
  dest
  mask
  src
  (Scalar value scalarUninitialized)
  vlreg@VL {..} = do
    validVL vconst (vectorMaskMul vectorConfig) vlreg
    assert
      ( "slide1down: value should have eew bits. eew: "
          <> showAsText (vectorElementBitWidth vconst vectorConfig)
          <> ", value bit size: "
          <> showAsText (finiteBitSize value)
      )
      $ finiteBitSize value == vectorElementBitWidth vconst vectorConfig
    vlVectorMask <- vlToVectorMask vconst vlreg vectorConfig
    let xlen = machineScalarLength
    let eew = vectorElementBitWidth vconst vectorConfig
    shiftedSrc <-
      unsafeVectorRegShiftRight
        vconst
        vectorConfig
        src
        (Scalar (bv xlen 1) false)

    shiftedVlVectorMask <-
      unsafeVectorRegShiftRight
        vconst
        vectorConfig
        vlVectorMask
        (Scalar (bv xlen 1) false)
    let combineData vlMask origSrcData =
          let vlMaskPadded =
                bvZext (finiteBitSize origSrcData) vlMask
              duplicatedScalar =
                foldl1 bvConcat $
                  replicate (finiteBitSize origSrcData `div` eew) value
           in (vlMaskPadded .&. origSrcData)
                .|. (complement vlMaskPadded .&. duplicatedScalar)
    let combineUninitialized vlMask origSrcUninitialized =
          let vlMaskPadded =
                bvZext (finiteBitSize origSrcUninitialized) vlMask
              duplicatedScalarUninitialized =
                symIte @mode
                  scalarUninitialized
                  (bv (finiteBitSize origSrcUninitialized) $ -1)
                  (bv (finiteBitSize origSrcUninitialized) 0)
           in (vlMaskPadded .&. origSrcUninitialized)
                .|. (complement vlMaskPadded .&. duplicatedScalarUninitialized)
    let combineVReg (VectorReg vlMaskD vlMaskU) (VectorReg origSrcData origSrcUninitialized) =
          VectorReg
            (combineData vlMaskD origSrcData)
            (combineUninitialized vlMaskU origSrcUninitialized)
    let combineVector (Vector _ vlRegs) (Vector _ origRegs) =
          Vector vectorConfig $
            zipWith combineVReg vlRegs origRegs
    let newVector = combineVector shiftedVlVectorMask shiftedSrc
    srcElements <- decomposeVector vconst newVector

    {-
        let numBits = vectorNumValidBits vconst vectorConfig
        let srcd = bvSelect 0 numBits $ concatData src
        let srcu = bvSelect 0 numBits $ concatUninitialized src
        (VectorElement scalard scalaru) <-
          scalarToVectorElement
            vconst
            vectorConfig
            True
            (elementWidthMul vectorConfig)
            xreg
        let replaceHigher orig scalarOrig =
              if finiteBitSize orig == finiteBitSize scalarOrig
                then scalarOrig
                else
                  bvConcat scalarOrig $
                    bvSelect
                      (finiteBitSize scalarOrig)
                      (finiteBitSize orig - finiteBitSize scalarOrig)
                      orig
        let eew = vectorElementBitWidth vconst vectorConfig
        let numElements = vectorNumValidElements vconst vectorConfig
        let insert orig scalarOrig =
              case vlValue of
                VLMask vlMask -> do
                  let vlMaskHigh =
                        bvSelect
                          1
                          (finiteBitSize vlMask)
                          (bvZext (finiteBitSize vlMask + 1) vlMask + 1)
                  let pad v =
                        if finiteBitSize v == finiteBitSize orig
                          then v
                          else
                            bvConcat (bv (finiteBitSize orig - finiteBitSize v) 0) v
                  let vecMask = pad $ duplicateMask vlMaskHigh eew
                  let duplicatedScalar = foldl1 bvConcat $ replicate numElements scalarOrig
                  let shiftedOrig = bvConcat (bv eew 0) (bvSelect eew (finiteBitSize orig - eew) orig)
                  mrgReturn $ (vecMask .&. duplicatedScalar) .|. (complement vecMask .&. shiftedOrig)
                VLNum vlNum ->
                  mrgReturn $
                    buildTreeMux
                      (\l r -> Just $ bv (r - l) 0)
                      (\l r -> Just $ bvSelect (l + eew) (r - l) orig)
                      (const scalarOrig)
                      (replaceHigher orig scalarOrig)
                      (vlNum - bv (finiteBitSize vlNum) 1)
                      eew
                      numElements
        srcd' <- insert srcd scalard
        srcu' <- insert srcu scalaru
        let newSrc = fromConcatenated vconst vectorConfig True srcd' srcu'

        srcElements <- decomposeVector vconst newSrc
        -}

    handleVectorTailMasks vconst vlreg mask dest (mrgReturn <$> srcElements)
    -- traceShow (src, r) $ mrgReturn r
