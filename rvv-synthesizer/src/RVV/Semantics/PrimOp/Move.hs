{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Semantics.PrimOp.Move (moveVV, moveSX, moveXS) where

import Data.Bits (Bits ((.&.)), FiniteBits (finiteBitSize))
import Grisette (BV (bv), mrgReturn)
import Grisette.Unified (symMin, (./=))
import RVV.Semantics.Element
  ( VectorElement (vectorElementData, vectorElementUninitialized),
    decomposeVector,
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.FullMask (fullMask)
import RVV.Semantics.PrimOp.Util
  ( SemConstraint,
    handleVectorTailMasks,
    scalarToVectorElement,
  )
import RVV.Semantics.SizeConstraint (validScalar, validVL, validVector)
import RVV.Semantics.Value
  ( Scalar (Scalar),
    VL (VL, vlMaskMul),
    VLValue (VLMask, VLNum),
    Vector (Vector),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    vectorElementBitWidth,
    vectorMaskMul,
    vectorNumValidElements,
  )
import RVV.Util.Context (assert)

moveVV ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Vector mode ->
  VL mode ->
  ctx (Vector mode)
moveVV vconst vectorConfig dest src vl = do
  validVector vconst vectorConfig dest
  validVector vconst vectorConfig src
  srcElements <- decomposeVector vconst src
  handleVectorTailMasks
    vconst
    vl
    (fullMask vconst (vlMaskMul vl))
    dest
    (return <$> srcElements)

moveSX ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  Scalar mode ->
  Vector mode ->
  VL mode ->
  ctx (Vector mode)
moveSX vconst scalar@(Scalar i _) dest@(Vector vectorConfig _) vl@(VL m n p u) = do
  let xmul = elementWidthMul vectorConfig
  validScalar vconst xmul scalar
  validVL vconst (vectorMaskMul vectorConfig) vl
  validVector vconst vectorConfig dest
  let eew = vectorElementBitWidth vconst vectorConfig
  assert "moveSX: source does not have SEW bits" $ finiteBitSize i == eew
  elems <- decomposeVector vconst dest
  let numValidElements = vectorNumValidElements vconst vectorConfig
  let evl =
        VL
          m
          ( case n of
              VLMask orig -> VLMask $ bv numValidElements 1 .&. orig
              VLNum orig -> VLNum $ symMin @mode orig 1
          )
          p
          u
  firstElement <- scalarToVectorElement vconst vectorConfig True xmul scalar
  handleVectorTailMasks
    vconst
    evl
    (fullMask vconst m)
    dest
    (fmap return $ firstElement : tail elems)

moveXS ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  Vector mode ->
  ctx (Scalar mode)
moveXS vconst dest = do
  elems <- decomposeVector vconst dest
  let d = vectorElementData $ head elems
  let u = vectorElementUninitialized $ head elems
  mrgReturn $ Scalar d $ u ./= 0
