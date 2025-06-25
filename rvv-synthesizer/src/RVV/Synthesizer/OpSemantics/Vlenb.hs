{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module RVV.Synthesizer.OpSemantics.Vlenb (applyVlenb, typeVlenb) where

import Grisette (mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import HieraSynth.Util.Show (showAsText)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.GetVL (getVLenB)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.Type (ValueType (ScalarType))
import RVV.Synthesizer.Value (Value (ScalarValue))
import RVV.Util.Context (assert)

applyVlenb ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  [Value mode] ->
  ctx [Value mode]
applyVlenb vconst values = do
  assert
    ( "applyVlenb: expected 0 arguments, but got "
        <> showAsText (length values)
    )
    $ null values
  vlenb <- getVLenB vconst
  return [ScalarValue vlenb]

typeVlenb :: (MonadContext ctx) => ctx (TypeSignature ValueType)
typeVlenb = mrgReturn $ TypeSignature [] [ScalarType 1]
