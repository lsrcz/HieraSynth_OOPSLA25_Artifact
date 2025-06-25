{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.MulAdd
  ( applySingleWidthVVMulAdd,
    applyWideningVVMulAdd,
    typeSingleWidthVVMulAdd,
    typeWideningVVMulAdd,
    typeSingleWidthVXMulAdd,
    typeWideningVXMulAdd,
    typeSingleWidthVIMulAdd,
    typeWideningVIMulAdd,
  )
where

import Grisette (mrgReturn)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (GetData, GetSomeWordN)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.MulAdd (mulAdd)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (narrowableVectorConfig)
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    narrowVectorConfig,
    vectorMaskMul,
  )
import RVV.Synthesizer.OpSemantics.Util
  ( getVectorDestMaskAndValidateTotalNum,
    typeVectorOpWithDestMask,
  )
import RVV.Synthesizer.Parameter.Destination (pd)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Type
  ( ValueType (ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (VectorValue),
    extractVLValue,
    extractVectorValue,
  )
import RVV.Util.Context (assert)

applyMulAdd ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  (GetSomeWordN mode -> GetSomeWordN mode) ->
  (GetSomeWordN mode -> GetSomeWordN mode) ->
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  (GetSomeWordN mode -> GetSomeWordN mode -> GetSomeWordN mode -> ctx (GetSomeWordN mode)) ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyMulAdd
  transformLhs
  transformRhs
  vconst
  argConfig
  resConfig
  elemFunc
  wrappedMasking
  values = do
    assert "arg and result should have the same maskMul" $
      vectorMaskMul argConfig == vectorMaskMul resConfig
    (remainingValues, vd, vm) <-
      getVectorDestMaskAndValidateTotalNum
        "applyMulAdd"
        (pd @mode)
        wrappedMasking
        vconst
        resConfig
        3
        values
    vl <- extractVLValue (head remainingValues)
    lhs <- extractVectorValue (remainingValues !! 1)
    rhs <- extractVectorValue (remainingValues !! 2)
    res <-
      mulAdd
        transformLhs
        transformRhs
        elemFunc
        vconst
        argConfig
        resConfig
        vd
        vm
        lhs
        rhs
        vl
    mrgReturn [VectorValue res]

applySingleWidthVVMulAdd ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  (GetSomeWordN mode -> GetSomeWordN mode -> GetSomeWordN mode -> ctx (GetSomeWordN mode)) ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applySingleWidthVVMulAdd vconst config =
  applyMulAdd id id vconst config config

applyWideningVVMulAdd ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  (GetSomeWordN mode -> GetSomeWordN mode) ->
  (GetSomeWordN mode -> GetSomeWordN mode) ->
  MachineConfig ->
  VectorConfig ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyWideningVVMulAdd
  transformLhs
  transformRhs
  vconst
  config
  wrappedMasking
  values = do
    narrowableVectorConfig vconst config
    applyMulAdd
      transformLhs
      transformRhs
      vconst
      (narrowVectorConfig config)
      config
      (\d l r -> mrgReturn $ d + l * r)
      wrappedMasking
      values

typeMulAdd ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  VectorConfig ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeMulAdd argConfig resConfig wrappedMasking = do
  assert "arg and result should have the same mulMask" $
    vectorMaskMul argConfig == vectorMaskMul resConfig
  typeVectorOpWithDestMask
    resConfig
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul argConfig,
              VectorType argConfig,
              VectorType argConfig
            ],
          resTypes = [VectorType resConfig]
        }
    )
    (pd @mode)
    wrappedMasking

typeSingleWidthVVMulAdd ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSingleWidthVVMulAdd config = typeMulAdd config config

typeWideningVVMulAdd ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeWideningVVMulAdd config = typeMulAdd (narrowVectorConfig config) config

typeMulAddScalar ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  VectorConfig ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeMulAddScalar argConfig resConfig wrappedMasking = do
  assert "arg and result should have the same mulMask" $
    vectorMaskMul argConfig == vectorMaskMul resConfig
  typeVectorOpWithDestMask
    resConfig
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul argConfig,
              ScalarType $ elementWidthMul argConfig,
              VectorType argConfig
            ],
          resTypes = [VectorType resConfig]
        }
    )
    (pd @mode)
    wrappedMasking

typeSingleWidthVXMulAdd ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSingleWidthVXMulAdd config = typeMulAddScalar config config

typeWideningVXMulAdd ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeWideningVXMulAdd config =
  typeMulAddScalar (narrowVectorConfig config) config

typeMulAddImm ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  VectorConfig ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeMulAddImm argConfig resConfig wrappedMasking = do
  assert "arg and result should have the same mulMask" $
    vectorMaskMul argConfig == vectorMaskMul resConfig
  typeVectorOpWithDestMask
    resConfig
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul argConfig,
              VectorType argConfig
            ],
          resTypes = [VectorType resConfig]
        }
    )
    (pd @mode)
    wrappedMasking

typeSingleWidthVIMulAdd ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSingleWidthVIMulAdd config = typeMulAddImm config config

typeWideningVIMulAdd ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeWideningVIMulAdd config =
  typeMulAddImm (narrowVectorConfig config) config
