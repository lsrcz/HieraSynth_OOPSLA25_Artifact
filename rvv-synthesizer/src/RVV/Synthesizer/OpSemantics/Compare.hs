{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Compare
  ( applyCompare,
    typeCompareVV,
    typeCompareVX,
    typeCompareVI,
  )
where

import Grisette (mrgReturn)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (GetBool, GetData, GetSomeWordN)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Compare (compareToMask)
import RVV.Semantics.PrimOp.Util
  ( SemConstraint,
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    vectorMaskMul,
  )
import RVV.Synthesizer.OpSemantics.Util
  ( getMaskDestMaskAndValidateTotalNum,
    typeMaskOpWithDestMask,
  )
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Type
  ( ValueType (MaskType, ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue),
    extractVLValue,
    extractVectorValue,
  )

applyCompare ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  (GetSomeWordN mode -> GetSomeWordN mode -> GetBool mode) ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyCompare
  vconst
  vectorConfig
  elemFunc
  wrappedDestination
  wrappedMasking
  values = do
    (remainingValues, vd, vm) <-
      getMaskDestMaskAndValidateTotalNum
        "compare"
        wrappedDestination
        wrappedMasking
        vconst
        (vectorMaskMul vectorConfig)
        3
        values
    vl <- extractVLValue (head remainingValues)
    lhs <- extractVectorValue (remainingValues !! 1)
    rhs <- extractVectorValue (remainingValues !! 2)
    res <- compareToMask elemFunc vconst vectorConfig vd vm lhs rhs vl
    mrgReturn [MaskValue res]

typeCompareVV ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeCompareVV vectorConfig wrappedDestination wrappedMasking = do
  let maskMul = vectorMaskMul vectorConfig
  typeMaskOpWithDestMask
    maskMul
    ( TypeSignature
        { argTypes =
            [ VLType maskMul,
              VectorType vectorConfig,
              VectorType vectorConfig
            ],
          resTypes = [MaskType maskMul]
        }
    )
    wrappedDestination
    wrappedMasking

typeCompareVX ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeCompareVX vectorConfig wrappedDestination wrappedMasking = do
  let maskMul = vectorMaskMul vectorConfig
  typeMaskOpWithDestMask
    maskMul
    ( TypeSignature
        { argTypes =
            [ VLType maskMul,
              VectorType vectorConfig,
              ScalarType $ elementWidthMul vectorConfig
            ],
          resTypes = [MaskType maskMul]
        }
    )
    wrappedDestination
    wrappedMasking

typeCompareVI ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeCompareVI vectorConfig wrappedDestination wrappedMasking = do
  let maskMul = vectorMaskMul vectorConfig
  typeMaskOpWithDestMask
    maskMul
    ( TypeSignature
        { argTypes =
            [VLType maskMul, VectorType vectorConfig],
          resTypes = [MaskType maskMul]
        }
    )
    wrappedDestination
    wrappedMasking
