{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Iota
  ( applyVIota,
    applyVId,
    typeVIota,
    typeVId,
  )
where

import Grisette
  ( mrgReturn,
  )
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (GetData)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Iota (vid, viota)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig, vectorMaskMul)
import RVV.Synthesizer.OpSemantics.Util
  ( getVectorDestMaskAndValidateTotalNum,
    typeVectorOpWithDestMask,
  )
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Type (ValueType (MaskType, VLType, VectorType))
import RVV.Synthesizer.Value
  ( Value (VectorValue),
    extractMaskValue,
    extractVLValue,
  )

applyVIota ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyVIota vconst config wrappedDestination wrappedMasking values = do
  (remainingValues, vd, vm) <-
    getVectorDestMaskAndValidateTotalNum
      "applyVIota"
      wrappedDestination
      wrappedMasking
      vconst
      config
      2
      values
  vl <- extractVLValue (head remainingValues)
  srcMask <- extractMaskValue (remainingValues !! 1)
  res <- viota vconst config vd vm srcMask vl
  mrgReturn [VectorValue res]

applyVId ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyVId vconst config wrappedDestination wrappedMasking values = do
  (remainingValues, vd, vm) <-
    getVectorDestMaskAndValidateTotalNum
      "applyVId"
      wrappedDestination
      wrappedMasking
      vconst
      config
      1
      values
  vl <- extractVLValue (head remainingValues)
  res <- vid vconst config vd vm vl
  mrgReturn [VectorValue res]

typeVIota ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeVIota config =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              MaskType $ vectorMaskMul config
            ],
          resTypes = [VectorType config]
        }
    )

typeVId ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeVId config =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes = [VLType $ vectorMaskMul config],
          resTypes = [VectorType config]
        }
    )
