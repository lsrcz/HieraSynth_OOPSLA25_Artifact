{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Util
  ( getVectorDestMaskAndValidateTotalNum,
    typeVectorOpWithDestMask,
    getMaskDestMaskAndValidateTotalNum,
    typeMaskOpWithDestMask,
    typeMaskOpWithDestMask',
  )
where

import Control.Monad (join)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Grisette (mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified (GetData, extractData)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.FullMask (fullMask)
import RVV.Semantics.PrimOp.Undefined (undefinedMask, undefinedVector)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validMask, validVector)
import RVV.Semantics.Value (Mask, Vector)
import RVV.Semantics.VectorConfig (VectorConfig, vectorMaskMul)
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest, UseUndefinedDest),
  )
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask, UseProvidedMask))
import RVV.Synthesizer.Type
  ( ValueType (MaskType, VectorType),
  )
import RVV.Synthesizer.Value (Value, extractMaskValue, extractVectorValue)
import RVV.Util.Context (assert)

getVectorDestMaskAndValidateTotalNum ::
  forall mode ctx.
  (HasCallStack, SemConstraint mode ctx) =>
  T.Text ->
  GetData mode Destination ->
  GetData mode Masking ->
  MachineConfig ->
  VectorConfig ->
  Int ->
  [Value mode] ->
  ctx ([Value mode], Vector mode, Mask mode)
getVectorDestMaskAndValidateTotalNum
  name
  wrappedDestination
  wrappedMasking
  vconst
  vectorConfig
  numOfBaseInputs
  values = do
    destination <- extractData wrappedDestination
    masking <- extractData wrappedMasking
    let useUndefinedDest = destination == UseUndefinedDest
    let useFullMask = masking == UseFullMask
    let undefinedDestNum = if useUndefinedDest then 0 else 1
    let fullMaskNum = if useFullMask then 0 else 1
    let consumedNum = undefinedDestNum + fullMaskNum
    let remaining = take (length values - consumedNum) values
    assert
      ( name
          <> ": invalid argument number, expected "
          <> showAsText (consumedNum + numOfBaseInputs)
          <> ", but got "
          <> showAsText (length values)
      )
      (length values == consumedNum + numOfBaseInputs)

    let destPos = numOfBaseInputs
    let maskPos = numOfBaseInputs + undefinedDestNum
    vd <-
      if useUndefinedDest
        then return $ undefinedVector vconst vectorConfig
        else extractVectorValue (values !! destPos)
    validVector vconst vectorConfig vd
    vm <-
      if useFullMask
        then return $ fullMask vconst (vectorMaskMul vectorConfig)
        else extractMaskValue (values !! maskPos)
    validMask vconst (vectorMaskMul vectorConfig) vm
    mrgReturn (remaining, vd, vm)

getMaskDestMaskAndValidateTotalNum ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  T.Text ->
  GetData mode Destination ->
  GetData mode Masking ->
  MachineConfig ->
  MaskMul ->
  Int ->
  [Value mode] ->
  ctx ([Value mode], Mask mode, Mask mode)
getMaskDestMaskAndValidateTotalNum
  name
  wrappedDestination
  wrappedMasking
  vconst
  maskMul
  numOfBaseInputs
  values = do
    destination <- extractData wrappedDestination
    masking <- extractData wrappedMasking
    let useUndefinedDest = destination == UseUndefinedDest
    let useFullMask = masking == UseFullMask
    let undefinedDestNum = if useUndefinedDest then 0 else 1
    let fullMaskNum = if useFullMask then 0 else 1
    let consumedNum = undefinedDestNum + fullMaskNum
    let remaining = take (length values - consumedNum) values
    assert
      ( name
          <> ": invalid argument number, expected "
          <> showAsText (consumedNum + numOfBaseInputs)
          <> ", but got "
          <> showAsText (length values)
      )
      (length values == consumedNum + numOfBaseInputs)

    let destPos = numOfBaseInputs
    let maskPos = numOfBaseInputs + undefinedDestNum
    vd <-
      if useUndefinedDest
        then return $ undefinedMask vconst maskMul
        else extractMaskValue (values !! destPos)
    vm <-
      if useFullMask
        then return $ fullMask vconst maskMul
        else extractMaskValue (values !! maskPos)
    mrgReturn (remaining, vd, vm)

typeVectorOpWithDestMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  TypeSignature ValueType ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeVectorOpWithDestMask
  vectorConfig
  baseSignature
  wrappedDestination
  wrappedMasking = do
    destination <- extractData wrappedDestination
    masking <- extractData wrappedMasking
    let inputTypes =
          join
            [ argTypes baseSignature,
              [VectorType vectorConfig | destination == UseProvidedDest],
              [ MaskType $ vectorMaskMul vectorConfig
                | masking == UseProvidedMask
              ]
            ]
    mrgReturn $
      TypeSignature {argTypes = inputTypes, resTypes = resTypes baseSignature}

typeMaskOpWithDestMask' ::
  forall ctx.
  (MonadContext ctx) =>
  MaskMul ->
  TypeSignature ValueType ->
  Destination ->
  Masking ->
  ctx (TypeSignature ValueType)
typeMaskOpWithDestMask' maskMul baseSignature destination masking = do
  let inputTypes =
        join
          [ argTypes baseSignature,
            [MaskType maskMul | destination == UseProvidedDest],
            [MaskType maskMul | masking == UseProvidedMask]
          ]
  mrgReturn $
    TypeSignature {argTypes = inputTypes, resTypes = resTypes baseSignature}

typeMaskOpWithDestMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MaskMul ->
  TypeSignature ValueType ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeMaskOpWithDestMask
  maskMul
  baseSignature
  wrappedDestination
  wrappedMasking = do
    destination <- extractData wrappedDestination
    masking <- extractData wrappedMasking
    typeMaskOpWithDestMask' maskMul baseSignature destination masking

{-
validDelegatedVType :: (MonadContext ctx) => MachineConfig -> VType -> Ratio Int -> ctx ()
validDelegatedVType vconst vtype maskMul = do
  vmaskMul <- vtypeAsMaskMul vconst vtype
  assert "validDelegatedVType: the vtype isn't large enough to hold the mask" $
    vmaskMul >= maskMul
    -}
