{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Merge
  ( applyMerge,
    typeMergeVVM,
    typeMergeVXM,
    typeMergeVIM,
  )
where

import Grisette (mrgReturn)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (GetData)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Merge (mergeVector)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    vectorMaskMul,
  )
import RVV.Synthesizer.OpSemantics.Util
  ( getVectorDestMaskAndValidateTotalNum,
    typeVectorOpWithDestMask,
  )
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (pm)
import RVV.Synthesizer.Type
  ( ValueType (ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (VectorValue),
    extractVLValue,
    extractVectorValue,
  )

applyMerge ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode Destination ->
  [Value mode] ->
  ctx [Value mode]
applyMerge vconst config wrappedDestination values = do
  (remainingValues, vd, vm) <-
    getVectorDestMaskAndValidateTotalNum
      "applyMerge"
      wrappedDestination
      (pm @mode)
      vconst
      config
      3
      values
  vl <- extractVLValue (head remainingValues)
  false <- extractVectorValue (remainingValues !! 1)
  true <- extractVectorValue (remainingValues !! 2)
  res <- mergeVector vconst config vd vm false true vl
  mrgReturn [VectorValue res]

typeMergeVVM ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  ctx (TypeSignature ValueType)
typeMergeVVM config wrappedDestination = do
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              VectorType config,
              VectorType config
            ],
          resTypes = [VectorType config]
        }
    )
    wrappedDestination
    (pm @mode)

typeMergeVXM ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  ctx (TypeSignature ValueType)
typeMergeVXM config wrappedDestination = do
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              VectorType config,
              ScalarType $ elementWidthMul config
            ],
          resTypes = [VectorType config]
        }
    )
    wrappedDestination
    (pm @mode)

typeMergeVIM ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  ctx (TypeSignature ValueType)
typeMergeVIM config wrappedDestination = do
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              VectorType config
            ],
          resTypes = [VectorType config]
        }
    )
    wrappedDestination
    (pm @mode)
