{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Move
  ( applyVMVVX,
    applyVMVMX,
    applyVMVVI,
    applyVMVMI,
    typeVMVVXSX,
    typeVMVMX,
    typeVMVVI,
    typeVMVMI,
    applyVMVSX,
    applyVMVXS,
    typeVMVXS,
    applyVMVVV,
    typeVMVVV,
  )
where

-- validDelegatedVType,

import Grisette (mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (GetData)
import RVV.Semantics.Imm (Imm)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.PrimOp.Broadcast (broadcast)
import RVV.Semantics.PrimOp.GetVL (getVLMax)
import RVV.Semantics.PrimOp.ImmToReg (immToVector)
import RVV.Semantics.PrimOp.Move (moveSX, moveVV, moveXS)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.Value (Mask (Mask), Vector (Vector, vectorConfig))
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    getDelegatedVectorConfig,
    vectorMaskMul,
  )
import RVV.Synthesizer.OpSemantics.Util
  ( getVectorDestMaskAndValidateTotalNum,
    typeMaskOpWithDestMask',
    typeVectorOpWithDestMask,
  )
import RVV.Synthesizer.Parameter.Destination (Destination (UseUndefinedDest), ud)
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask), fm)
import RVV.Synthesizer.Type
  ( ValueType (MaskType, ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, ScalarValue, VLValue, VectorValue),
    extractScalarValue,
    extractVLValue,
    extractVectorValue,
  )
import RVV.Util.Context (assert)

applyVMVVV ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode Destination ->
  [Value mode] ->
  ctx [Value mode]
applyVMVVV vconst config wrappedDestination values = do
  (remainingValues, vd, _) <-
    getVectorDestMaskAndValidateTotalNum
      "applyVMVVV"
      wrappedDestination
      (fm @mode)
      vconst
      config
      2
      values
  vl <- extractVLValue (head remainingValues)
  src <- extractVectorValue (remainingValues !! 1)
  res <- moveVV vconst config vd src vl
  mrgReturn [VectorValue res]

applyVMVSX ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode Destination ->
  [Value mode] ->
  ctx [Value mode]
applyVMVSX vconst config wrappedDestination values = do
  (remainingValues, vd, _) <-
    getVectorDestMaskAndValidateTotalNum
      "applyVMVSX"
      wrappedDestination
      (fm @mode)
      vconst
      config
      2
      values
  vl <- extractVLValue (head remainingValues)
  scalar <- extractScalarValue (remainingValues !! 1)
  vec <- moveSX vconst scalar vd vl
  mrgReturn [VectorValue vec]

applyVMVXS ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  [Value mode] ->
  ctx [Value mode]
applyVMVXS vconst config values = do
  assert "applyVMVXS: invalid number of arguments" $ length values == 1
  vec <- extractVectorValue (head values)
  assert "applyVMVXS: invalid vector type" $
    config == vectorConfig vec
  res <- moveXS vconst vec
  mrgReturn [ScalarValue res]

applyVMVVX ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode Destination ->
  [Value mode] ->
  ctx [Value mode]
applyVMVVX vconst config wrappedDestination values = do
  (remainingValues, vd, vm) <-
    getVectorDestMaskAndValidateTotalNum
      "applyVMVVX"
      wrappedDestination
      (fm @mode)
      vconst
      config
      2
      values
  vl <- extractVLValue (head remainingValues)
  src <- extractScalarValue (remainingValues !! 1)
  res <- broadcast vconst config vd vm src vl
  mrgReturn [VectorValue res]

applyVMVMX ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  MaskMul ->
  [Value mode] ->
  ctx [Value mode]
applyVMVMX vconst xmul maskMul values = do
  config <- getDelegatedVectorConfig vconst xmul maskMul
  vl <- getVLMax @mode vconst (vectorMaskMul config) nonePolicy
  r <- applyVMVVX vconst config (ud @mode) (VLValue vl : values)
  case r of
    [VectorValue (Vector _ [res])] -> mrgReturn [MaskValue $ Mask maskMul res]
    _ -> error "Should not happen"

applyVMVVI ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode Destination ->
  Imm mode ->
  [Value mode] ->
  ctx [Value mode]
applyVMVVI vconst config wrappedDestination imm values = do
  (remainingValues, vd, vm) <-
    getVectorDestMaskAndValidateTotalNum
      "applyVMVVX"
      wrappedDestination
      (fm @mode)
      vconst
      config
      1
      values
  vl <- extractVLValue (head remainingValues)
  res <- immToVector vconst config True vd vm vl imm
  mrgReturn [VectorValue res]

applyVMVMI ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  MaskMul ->
  Imm mode ->
  [Value mode] ->
  ctx [Value mode]
applyVMVMI vconst xmul maskMul imm values = do
  config <- getDelegatedVectorConfig vconst xmul maskMul
  vl <- getVLMax @mode vconst (vectorMaskMul config) nonePolicy
  r <- applyVMVVI vconst config (ud @mode) imm (VLValue vl : values)
  case r of
    [VectorValue (Vector _ [res])] -> mrgReturn [MaskValue $ Mask maskMul res]
    _ -> error "Should not happen"

typeVMVVV ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  ctx (TypeSignature ValueType)
typeVMVVV config wrappedDestination =
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
    (fm @mode)

typeVMVVXSX ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  ctx (TypeSignature ValueType)
typeVMVVXSX config wrappedDestination =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              ScalarType $ elementWidthMul config
            ],
          resTypes = [VectorType config]
        }
    )
    wrappedDestination
    (fm @mode)

typeVMVXS ::
  (MonadContext ctx) =>
  VectorConfig ->
  ctx (TypeSignature ValueType)
typeVMVXS config =
  mrgReturn $
    TypeSignature
      { argTypes = [VectorType config],
        resTypes = [ScalarType $ elementWidthMul config]
      }

typeVMVMX ::
  forall ctx.
  (MonadContext ctx) =>
  WidthMul ->
  MaskMul ->
  ctx (TypeSignature ValueType)
typeVMVMX xmul maskMul = do
  typeMaskOpWithDestMask'
    maskMul
    ( TypeSignature
        { argTypes = [ScalarType xmul],
          resTypes = [MaskType maskMul]
        }
    )
    UseUndefinedDest
    UseFullMask

typeVMVVI ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  ctx (TypeSignature ValueType)
typeVMVVI config wrappedDestination =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes = [VLType $ vectorMaskMul config],
          resTypes = [VectorType config]
        }
    )
    wrappedDestination
    (fm @mode)

typeVMVMI ::
  forall ctx.
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeVMVMI maskMul = do
  typeMaskOpWithDestMask'
    maskMul
    (TypeSignature {argTypes = [], resTypes = [MaskType maskMul]})
    UseUndefinedDest
    UseFullMask
