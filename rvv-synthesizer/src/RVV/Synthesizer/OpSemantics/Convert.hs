{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Convert
  ( applyVLToScalar,
    applyPtrToScalar,
    typeVLToScalar,
    typePtrToScalar,
    applyScalarToPtr,
    typeScalarToPtr,
  )
where

import Grisette (BV (bv), mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified (symBitBlast, symIte)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (MachineBaseConfig, machineScalarLength),
    MachineConfig (MachineConfig, baseConfig),
  )
import RVV.Semantics.Memory (BlockId, Ptr (Ptr, ptrOffset, ptrUninitialized))
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validPtr, validScalar, validVL)
import RVV.Semantics.Value (Scalar (Scalar), VL (VL, vlUninitialized, vlValue), VLValue (VLMask, VLNum))
import RVV.Synthesizer.Type (ValueType (PtrType, ScalarType, VLType))
import RVV.Synthesizer.Value
  ( Value (PtrValue, ScalarValue),
    extractPtrValue,
    extractScalarValue,
    extractVLValue,
  )
import RVV.Util.Context (assert)

applyVLToScalar ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  [Value mode] ->
  ctx [Value mode]
applyVLToScalar vconst maskMul values = do
  assert
    ( "applyVLToScalar: expected 1 arguments, but got "
        <> showAsText (length values)
    )
    $ length values == 1
  vlreg@VL {..} <- extractVLValue (head values)
  validVL vconst maskMul vlreg
  case vlValue of
    VLMask vlMask -> do
      let bits = symBitBlast @mode vlMask
      let xlen = machineScalarLength $ baseConfig vconst
      let scalar = sum $ fmap (\d -> symIte d (bv xlen 1) (bv xlen 0)) bits
      mrgReturn [ScalarValue $ Scalar scalar vlUninitialized]
    VLNum vlNum -> mrgReturn [ScalarValue $ Scalar vlNum vlUninitialized]

applyPtrToScalar ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  BlockId ->
  [Value mode] ->
  ctx [Value mode]
applyPtrToScalar vconst ptrXMul blockId values = do
  assert
    ( "applyPtrToScalar: expected 1 arguments, but got "
        <> showAsText (length values)
    )
    $ length values == 1
  ptr@Ptr {..} <- extractPtrValue (head values)
  validPtr vconst ptrXMul blockId ptr
  mrgReturn [ScalarValue $ Scalar ptrOffset ptrUninitialized]

applyScalarToPtr ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  BlockId ->
  [Value mode] ->
  ctx [Value mode]
applyScalarToPtr
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  ptrXMul
  blockId
  values = do
    assert
      ( "applyPtrToScalar: expected 1 arguments, but got "
          <> showAsText (length values)
      )
      $ length values == 1
    xreg@(Scalar bv uninitialized) <- extractScalarValue (head values)
    validScalar vconst 1 xreg
    let ptr = Ptr ptrXMul blockId bv uninitialized
    validPtr vconst ptrXMul blockId ptr
    mrgReturn [PtrValue ptr]

typeVLToScalar ::
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeVLToScalar vlMaskMul =
  mrgReturn $ TypeSignature [VLType vlMaskMul] [ScalarType 1]

typePtrToScalar ::
  (MonadContext ctx) =>
  WidthMul ->
  BlockId ->
  ctx (TypeSignature ValueType)
typePtrToScalar ptrXMul blockId =
  mrgReturn $ TypeSignature [PtrType ptrXMul blockId] [ScalarType 1]

typeScalarToPtr ::
  (MonadContext ctx) =>
  WidthMul ->
  BlockId ->
  ctx (TypeSignature ValueType)
typeScalarToPtr ptrXMul blockId =
  mrgReturn $ TypeSignature [ScalarType 1] [PtrType ptrXMul blockId]
