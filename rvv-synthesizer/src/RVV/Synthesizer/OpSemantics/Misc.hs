{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Misc
  ( applyVSetVL,
    applyVSetVLMax,
    applyVSetVLRelay,
    applyUndefinedMask,
    applyUndefinedVector,
    typeVSetVL,
    typeVSetVLMax,
    typeVSetVLRelay,
    typeUndefinedMask,
    typeUndefinedVector,
  )
where

import Data.Bits (FiniteBits (finiteBitSize))
import Grisette (BV (bv, bvConcat), ToSym (toSym), mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified (GetBool)
import RVV.Semantics.Element (vlValueToVLNum)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (MachineBaseConfig, machineScalarLength),
    MachineConfig (MachineConfig, baseConfig),
  )
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.Policy (Policy)
import RVV.Semantics.PrimOp.GetVL (getVL, getVLMax)
import RVV.Semantics.PrimOp.Undefined (undefinedMask, undefinedVector)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validVL)
import RVV.Semantics.Value (Scalar (Scalar), VL (VL), VLValue (VLMask, VLNum))
import RVV.Semantics.VectorConfig (VectorConfig, maskNumValidElements)
import RVV.Synthesizer.Type
  ( ValueType (MaskType, ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, VLValue, VectorValue),
    extractScalarValue,
    extractVLValue,
  )
import RVV.Util.Context (assert)

applyVSetVL ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  Policy mode ->
  [Value mode] ->
  ctx [Value mode]
applyVSetVL vconst@MachineConfig {baseConfig = MachineBaseConfig {..}} maskMul policy l = do
  assert
    ( "applyVSetVL: invalid argument number, expected 1, but got "
        <> showAsText (length l)
    )
    (toSym (length l == 1) :: GetBool mode)
  avlReg <- extractScalarValue (head l)
  vl <- getVL vconst maskMul avlReg policy
  mrgReturn [VLValue vl]

typeVSetVL ::
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeVSetVL maskMul =
  mrgReturn $
    TypeSignature
      { argTypes = [ScalarType 1],
        resTypes = [VLType maskMul]
      }

applyVSetVLMax ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  Policy mode ->
  [Value mode] ->
  ctx [Value mode]
applyVSetVLMax vconst maskMul policy l = do
  assert
    ( "applyVSetVLMax: invalid argument number, expected 0, but got "
        <> showAsText (length l)
    )
    (toSym (null l) :: GetBool mode)
  vl <- getVLMax vconst maskMul policy
  mrgReturn [VLValue vl]

typeVSetVLMax ::
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeVSetVLMax maskMul =
  mrgReturn $
    TypeSignature {argTypes = [], resTypes = [VLType maskMul]}

applyVSetVLRelay ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  MaskMul ->
  Policy mode ->
  [Value mode] ->
  ctx [Value mode]
applyVSetVLRelay vconst maskMul srcMaskMul policy l = do
  assert
    ( "applyVSetVLRelay: invalid argument number, expected 1, but got "
        <> showAsText (length l)
    )
    (toSym (length l == 1) :: GetBool mode)
  vl@(VL _ relayedValue _ u) <- extractVLValue (head l)
  validVL vconst srcMaskMul vl
  case relayedValue of
    VLNum relayedNum -> do
      vl <- getVL vconst maskMul (Scalar relayedNum u) policy
      mrgReturn [VLValue vl]
    VLMask relayedMask -> do
      let numBits = maskNumValidElements vconst maskMul
      if numBits == finiteBitSize relayedMask
        then mrgReturn [VLValue $ VL maskMul relayedValue policy u]
        else
          if numBits > finiteBitSize relayedMask
            then
              mrgReturn
                [ VLValue $
                    VL
                      maskMul
                      ( VLMask $
                          bvConcat (bv (numBits - finiteBitSize relayedMask) 0) relayedMask
                      )
                      policy
                      u
                ]
            else do
              case vlValueToVLNum vconst relayedValue of
                VLNum num -> do
                  vl <- getVL vconst maskMul (Scalar num u) policy
                  mrgReturn [VLValue vl]
                _ -> error "Should not happen"

typeVSetVLRelay ::
  (MonadContext ctx) =>
  MaskMul ->
  MaskMul ->
  ctx (TypeSignature ValueType)
typeVSetVLRelay maskMul srcMaskMul =
  mrgReturn $
    TypeSignature
      { argTypes = [VLType srcMaskMul],
        resTypes = [VLType maskMul]
      }

applyUndefinedMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  [Value mode] ->
  ctx [Value mode]
applyUndefinedMask vconst maskMul l = do
  assert
    ( "applyUndefinedMask: invalid argument number, expected 0, but got "
        <> showAsText (length l)
    )
    (toSym (null l) :: GetBool mode)
  mrgReturn [MaskValue $ undefinedMask vconst maskMul]

typeUndefinedMask ::
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeUndefinedMask maskMul =
  mrgReturn $
    TypeSignature {argTypes = [], resTypes = [MaskType maskMul]}

applyUndefinedVector ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  [Value mode] ->
  ctx [Value mode]
applyUndefinedVector vconst config l = do
  assert
    ( "applyUndefinedVector: invalid argument number, expected 0, but got "
        <> showAsText (length l)
    )
    (toSym (null l) :: GetBool mode)
  mrgReturn
    [ VectorValue $ undefinedVector vconst config
    ]

typeUndefinedVector ::
  (MonadContext ctx) =>
  VectorConfig ->
  ctx (TypeSignature ValueType)
typeUndefinedVector config =
  mrgReturn $
    TypeSignature {argTypes = [], resTypes = [VectorType config]}
