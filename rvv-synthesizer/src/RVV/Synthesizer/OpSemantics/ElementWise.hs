{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.ElementWise
  ( applyElementWise,
    applySingleWidthVVElementWise,
    applyWideningVVElementWise,
    applyWideningWVElementWise,
    typeElementWiseVV,
    typeSingleWidthVVElementWise,
    typeWideningVVElementWise,
    typeWideningWVElementWise,
    typeElementWiseVX,
    typeSingleWidthVXElementWise,
    typeWideningVXElementWise,
    typeWideningWXElementWise,
    typeSingleWidthVI,
    applyElementWiseMMElementWise,
    typeElementWiseMM,
    typeElementWiseMX,
    typeElementWiseMI,
    typeFullScalarVX,
    applyNarrowingRightShiftWVElementWise,
    typeNarrowingRightShiftWVElementWise,
    typeNarrowingRightShiftWXElementWise,
    typeNarrowingRightShiftWIElementWise,
    applyFixedPointClipWVElementWise,
    typeWideningVIElementWise,
    typeWideningWIElementWise,
  )
where

import GHC.Stack (HasCallStack)
import Grisette (mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (GetData)
import RVV.Semantics.Element (VectorElement)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.PrimOp.ElementWise (elementWise)
import RVV.Semantics.PrimOp.GetVL (getVLMax)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (narrowableVectorConfig)
import RVV.Semantics.Value (Mask (Mask), Vector (Vector))
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    getDelegatedVectorConfig,
    narrowVectorConfig,
    vectorMaskMul,
    widenVectorConfig,
  )
import RVV.Synthesizer.OpSemantics.Util
  ( getVectorDestMaskAndValidateTotalNum,
    typeVectorOpWithDestMask,
  )
import RVV.Synthesizer.Parameter.Destination (Destination, ud)
import RVV.Synthesizer.Parameter.Masking (Masking, fm)
import RVV.Synthesizer.Type
  ( ValueType
      ( MaskType,
        ScalarType,
        VLType,
        VectorType
      ),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, VLValue, VectorValue),
    extractMaskValue,
    extractVLValue,
    extractVectorValue,
  )
import RVV.Util.Context (assert)

applyElementWise ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode) ->
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  VectorConfig ->
  (VectorElement mode -> VectorElement mode -> ctx (VectorElement mode)) ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyElementWise
  transformLhs
  transformRhs
  transformDest
  vconst
  lhsConfig
  rhsConfig
  resConfig
  elemFunc
  wrappedDestination
  wrappedMasking
  values = do
    assert "LHS and RHS types must have the same maskMul" $
      vectorMaskMul lhsConfig == vectorMaskMul rhsConfig
    assert "LHS and result types must have the same maskMul" $
      vectorMaskMul lhsConfig == vectorMaskMul resConfig
    (remainingValues, vd, vm) <-
      getVectorDestMaskAndValidateTotalNum
        "applyElementWise"
        wrappedDestination
        wrappedMasking
        vconst
        resConfig
        3
        values
    vl <- extractVLValue (head remainingValues)
    lhs <- extractVectorValue (remainingValues !! 1)
    rhs <- extractVectorValue (remainingValues !! 2)
    res <-
      elementWise
        transformLhs
        transformRhs
        transformDest
        elemFunc
        vconst
        lhsConfig
        rhsConfig
        resConfig
        vd
        vm
        lhs
        rhs
        vl
    mrgReturn [VectorValue res]

applySingleWidthVVElementWise ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  MachineConfig ->
  VectorConfig ->
  (VectorElement mode -> VectorElement mode -> ctx (VectorElement mode)) ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applySingleWidthVVElementWise vconst config =
  applyElementWise id id id vconst config config config

applyElementWiseMMElementWise ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  MachineConfig ->
  WidthMul ->
  MaskMul ->
  (VectorElement mode -> VectorElement mode -> ctx (VectorElement mode)) ->
  [Value mode] ->
  ctx [Value mode]
applyElementWiseMMElementWise vconst xmul maskMul elemFunc values = do
  config <- getDelegatedVectorConfig vconst xmul maskMul
  assert "applyElementWiseMMElementWise: should have 2 arguments" $
    length values == 2
  Mask _ lhsReg <- extractMaskValue (head values)
  Mask _ rhsReg <- extractMaskValue (values !! 1)
  vl <- getVLMax @mode vconst (vectorMaskMul config) nonePolicy
  res <-
    applySingleWidthVVElementWise
      vconst
      config
      elemFunc
      (ud @mode)
      (fm @mode)
      [ VLValue vl,
        VectorValue $ Vector config [lhsReg],
        VectorValue $ Vector config [rhsReg]
      ]
  resVec <- extractVectorValue (head res)
  case resVec of
    Vector _ [resReg] ->
      mrgReturn [MaskValue $ Mask maskMul resReg]
    _ -> error "BUG: applyElementWiseMMElementWise: unexpected result"

applyWideningVVElementWise ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode -> ctx (VectorElement mode)) ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyWideningVVElementWise
  vconst
  config
  widenLhs
  widenRhs
  elemFunc
  useUninitializedDest
  wrappedMasking
  values = do
    narrowableVectorConfig vconst config
    applyElementWise
      widenLhs
      widenRhs
      id
      vconst
      (narrowVectorConfig config)
      (narrowVectorConfig config)
      config
      elemFunc
      useUninitializedDest
      wrappedMasking
      values

applyNarrowingRightShiftWVElementWise ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode -> ctx (VectorElement mode)) ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyNarrowingRightShiftWVElementWise vconst config widenRhs narrowRes =
  applyElementWise
    id
    widenRhs
    narrowRes
    vconst
    (widenVectorConfig config)
    config
    config

applyFixedPointClipWVElementWise ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  MachineConfig ->
  VectorConfig ->
  (VectorElement mode -> VectorElement mode -> ctx (VectorElement mode)) ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyFixedPointClipWVElementWise vconst config =
  applyElementWise id id id vconst (widenVectorConfig config) config config

applyWideningWVElementWise ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode -> ctx (VectorElement mode)) ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyWideningWVElementWise
  vconst
  config
  widenRhs
  elemFunc
  useUninitializedDest
  wrappedMasking
  values = do
    narrowableVectorConfig vconst config
    applyElementWise
      id
      widenRhs
      id
      vconst
      config
      (narrowVectorConfig config)
      config
      elemFunc
      useUninitializedDest
      wrappedMasking
      values

typeElementWiseVV ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  VectorConfig ->
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeElementWiseVV
  lhsConfig
  rhsConfig
  resConfig
  wrappedDestination
  wrappedMasking = do
    assert "LHS and RHS types must have the same maskMul" $
      vectorMaskMul lhsConfig == vectorMaskMul rhsConfig
    assert "LHS and result types must have the same maskMul" $
      vectorMaskMul lhsConfig == vectorMaskMul resConfig
    typeVectorOpWithDestMask
      resConfig
      ( TypeSignature
          { argTypes =
              [ VLType $ vectorMaskMul lhsConfig,
                VectorType lhsConfig,
                VectorType rhsConfig
              ],
            resTypes = [VectorType resConfig]
          }
      )
      wrappedDestination
      wrappedMasking

typeSingleWidthVVElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSingleWidthVVElementWise vtype = typeElementWiseVV vtype vtype vtype

typeWideningVVElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeWideningVVElementWise config =
  typeElementWiseVV
    (narrowVectorConfig config)
    (narrowVectorConfig config)
    config

typeWideningWVElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeWideningWVElementWise config =
  typeElementWiseVV
    config
    (narrowVectorConfig config)
    config

typeNarrowingRightShiftWVElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeNarrowingRightShiftWVElementWise config =
  typeElementWiseVV (widenVectorConfig config) config config

typeNarrowingRightShiftWXElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeNarrowingRightShiftWXElementWise
  config
  wrappedDestination
  wrappedMasking = do
    typeVectorOpWithDestMask
      config
      ( TypeSignature
          { argTypes =
              [ VLType $ vectorMaskMul config,
                VectorType (widenVectorConfig config),
                ScalarType 1
              ],
            resTypes = [VectorType config]
          }
      )
      wrappedDestination
      wrappedMasking

typeNarrowingRightShiftWIElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeNarrowingRightShiftWIElementWise
  config
  wrappedDestination
  wrappedMasking = do
    typeVectorOpWithDestMask
      config
      ( TypeSignature
          { argTypes =
              [ VLType $ vectorMaskMul config,
                VectorType (widenVectorConfig config)
              ],
            resTypes = [VectorType config]
          }
      )
      wrappedDestination
      wrappedMasking

typeElementWiseVX ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  VectorConfig ->
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeElementWiseVX
  lhsConfig
  rhsConfig
  resConfig
  wrappedDestination
  wrappedMasking = do
    assert "LHS and result types must have the same ratio" $
      vectorMaskMul lhsConfig == vectorMaskMul resConfig
    typeVectorOpWithDestMask
      resConfig
      ( TypeSignature
          { argTypes =
              [ VLType $ vectorMaskMul lhsConfig,
                VectorType lhsConfig,
                ScalarType $ elementWidthMul rhsConfig
              ],
            resTypes = [VectorType resConfig]
          }
      )
      wrappedDestination
      wrappedMasking

typeElementWiseVI ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeElementWiseVI
  lhsConfig
  resConfig
  wrappedDestination
  wrappedMasking = do
    assert "LHS and result types must have the same ratio" $
      vectorMaskMul lhsConfig == vectorMaskMul resConfig
    typeVectorOpWithDestMask
      resConfig
      ( TypeSignature
          { argTypes =
              [ VLType $ vectorMaskMul lhsConfig,
                VectorType lhsConfig
              ],
            resTypes = [VectorType resConfig]
          }
      )
      wrappedDestination
      wrappedMasking

typeSingleWidthVXElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSingleWidthVXElementWise config = typeElementWiseVX config config config

typeFullScalarVX ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeFullScalarVX
  config
  wrappedDestination
  wrappedMasking = do
    typeVectorOpWithDestMask
      config
      ( TypeSignature
          { argTypes =
              [ VLType $ vectorMaskMul config,
                VectorType config,
                ScalarType 1
              ],
            resTypes = [VectorType config]
          }
      )
      wrappedDestination
      wrappedMasking

typeWideningVXElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeWideningVXElementWise config =
  typeElementWiseVX (narrowVectorConfig config) (narrowVectorConfig config) config

typeWideningWXElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeWideningWXElementWise config =
  typeElementWiseVX config (narrowVectorConfig config) config

typeWideningVIElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeWideningVIElementWise config =
  typeElementWiseVI (narrowVectorConfig config) config

typeWideningWIElementWise ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeWideningWIElementWise config =
  typeElementWiseVI config config

typeSingleWidthVI ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSingleWidthVI config = typeElementWiseVI config config

typeElementWiseMM ::
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeElementWiseMM maskMul = do
  mrgReturn $
    TypeSignature
      [MaskType maskMul, MaskType maskMul]
      [MaskType maskMul]

typeElementWiseMX ::
  (MonadContext ctx) =>
  WidthMul ->
  MaskMul ->
  ctx (TypeSignature ValueType)
typeElementWiseMX xmul maskMul = do
  mrgReturn $
    TypeSignature
      [MaskType maskMul, ScalarType xmul]
      [MaskType maskMul]

typeElementWiseMI ::
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeElementWiseMI maskMul = do
  mrgReturn $
    TypeSignature
      [MaskType maskMul]
      [MaskType maskMul]
