{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.Broadcast (broadcast, fullBroadcast) where

import Data.Bits (FiniteBits (finiteBitSize))
import Grisette (BV (bv), mrgReturn)
import Grisette.Unified (symIte)
import RVV.Semantics.Element (VectorElement (VectorElement))
import RVV.Semantics.MachineConfig (MachineConfig (MachineConfig))
import RVV.Semantics.Policy (Policy, nonePolicy)
import RVV.Semantics.PrimOp.FullMask (fullMask)
import RVV.Semantics.PrimOp.GetVL (getVLMax)
import RVV.Semantics.PrimOp.Undefined (undefinedVector)
import RVV.Semantics.PrimOp.Util (SemConstraint, handleVectorTailMasks)
import RVV.Semantics.SizeConstraint
  ( validScalar,
  )
import RVV.Semantics.Value
  ( Mask,
    Scalar (Scalar),
    VL (VL),
    Vector,
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    vectorElementBitWidth,
    vectorMaskMul,
  )
import RVV.Util.Context (assert)

broadcast ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Scalar mode ->
  VL mode ->
  ctx (Vector mode)
broadcast
  vconst@MachineConfig {..}
  vectorConfig
  dest
  mask
  xreg@(Scalar v u)
  vlreg@VL {..} = do
    validScalar vconst (elementWidthMul vectorConfig) xreg
    let eew = vectorElementBitWidth vconst vectorConfig
    assert "broadcast: incorrect scalar size" (eew == finiteBitSize v)
    let uninit = symIte u (bv eew $ -1) (bv eew 0)
    handleVectorTailMasks vconst vlreg mask dest $
      repeat (mrgReturn $ VectorElement v uninit)

fullBroadcast ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Scalar mode ->
  ctx (Vector mode)
fullBroadcast vconst vectorConfig xreg = do
  let dest = undefinedVector vconst vectorConfig
  let mask = fullMask vconst (vectorMaskMul vectorConfig)
  vl <-
    getVLMax @mode
      vconst
      (vectorMaskMul vectorConfig)
      (nonePolicy :: Policy mode)
  broadcast vconst vectorConfig dest mask xreg vl
