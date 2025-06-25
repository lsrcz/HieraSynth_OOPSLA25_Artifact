{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.MaskLogical
  ( applyMaskLogical,
    typeMaskLogical,
  )
where

import Grisette
  ( mrgReturn,
  )
import Grisette.Internal.Unified.UnifiedBV (GetSomeWordN)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import HieraSynth.Util.Show (showAsText)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.MaskLogical (maskLogical')
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validMaskMul)
import RVV.Synthesizer.Type (ValueType (MaskType, VLType))
import RVV.Synthesizer.Value
  ( Value (MaskValue),
    extractMaskValue,
    extractVLValue,
  )
import RVV.Util.Context (assert)

applyMaskLogical ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  (GetSomeWordN mode -> GetSomeWordN mode -> GetSomeWordN mode) ->
  ( GetSomeWordN mode ->
    GetSomeWordN mode ->
    GetSomeWordN mode ->
    GetSomeWordN mode ->
    GetSomeWordN mode
  ) ->
  [Value mode] ->
  ctx [Value mode]
applyMaskLogical vconst maskMul dataFunc uninitFunc values = do
  assert
    ( "applyMaskLogical: expected 3 arguments, but got "
        <> showAsText (length values)
    )
    $ length values == 3
  validMaskMul vconst maskMul
  vl <- extractVLValue (head values)
  lhs <- extractMaskValue (values !! 1)
  rhs <- extractMaskValue (values !! 2)
  res <- maskLogical' dataFunc uninitFunc vconst maskMul lhs rhs vl
  mrgReturn [MaskValue res]

typeMaskLogical ::
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeMaskLogical maskMul =
  mrgReturn $
    TypeSignature
      [VLType maskMul, MaskType maskMul, MaskType maskMul]
      [MaskType maskMul]
