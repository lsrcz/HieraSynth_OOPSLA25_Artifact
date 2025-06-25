{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module RVV.Synthesizer.OpSemantics.MiscMask
  ( applyVCPop,
    applyVFirst,
    applyVSetMask,
    typeVCPop,
    typeVFirst,
    typeVSetMask,
  )
where

import Grisette (mrgReturn)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (GetBool, GetData, extractData)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.MiscMask (countPopulation, findFirstSet, setMask)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.OpSemantics.Util
  ( getMaskDestMaskAndValidateTotalNum,
    typeMaskOpWithDestMask,
  )
import RVV.Synthesizer.Parameter.Destination (Destination, ud)
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask, UseProvidedMask))
import RVV.Synthesizer.Type
  ( ValueType (MaskType, ScalarType, VLType),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, ScalarValue),
    extractMaskValue,
    extractVLValue,
  )

applyVCPop ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyVCPop vconst maskMul masking values = do
  (remainingValues, _, vm) <-
    getMaskDestMaskAndValidateTotalNum
      "cpop"
      ud
      masking
      vconst
      maskMul
      2
      values
  vl <- extractVLValue (head remainingValues)
  srcMask <- extractMaskValue (remainingValues !! 1)
  res <- countPopulation vconst maskMul vm srcMask vl
  mrgReturn [ScalarValue res]

applyVFirst ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyVFirst vconst maskMul masking values = do
  (remainingValues, _, vm) <-
    getMaskDestMaskAndValidateTotalNum
      "vfirst"
      ud
      masking
      vconst
      maskMul
      2
      values
  vl <- extractVLValue (head remainingValues)
  srcMask <- extractMaskValue (remainingValues !! 1)
  res <- findFirstSet vconst maskMul vm srcMask vl
  mrgReturn [ScalarValue res]

applyVSetMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  (GetBool mode -> GetBool mode) ->
  MachineConfig ->
  MaskMul ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyVSetMask curSetFunc vconst maskMul dest masking values = do
  (remainingValues, vd, vm) <-
    getMaskDestMaskAndValidateTotalNum
      "set_mask"
      dest
      masking
      vconst
      maskMul
      2
      values
  vl <- extractVLValue (head remainingValues)
  srcMask <- extractMaskValue (remainingValues !! 1)
  res <- setMask curSetFunc vconst maskMul vd vm srcMask vl
  mrgReturn [MaskValue res]

typeVCPop ::
  (SemConstraint mode ctx) =>
  MaskMul ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeVCPop maskMul masking = do
  m <- extractData masking
  case m of
    UseFullMask ->
      mrgReturn $
        TypeSignature
          [VLType maskMul, MaskType maskMul]
          [ScalarType 1]
    UseProvidedMask ->
      mrgReturn $
        TypeSignature
          [VLType maskMul, MaskType maskMul, MaskType maskMul]
          [ScalarType 1]

typeVFirst ::
  (SemConstraint mode ctx) =>
  MaskMul ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeVFirst = typeVCPop

typeVSetMask ::
  (SemConstraint mode ctx) =>
  MaskMul ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeVSetMask maskMul =
  typeMaskOpWithDestMask
    maskMul
    ( TypeSignature
        { argTypes = [VLType maskMul, MaskType maskMul],
          resTypes = [MaskType maskMul]
        }
    )
