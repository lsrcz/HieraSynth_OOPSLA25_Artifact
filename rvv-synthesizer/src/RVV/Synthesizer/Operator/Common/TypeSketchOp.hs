{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp) where

import Grisette (GenSym (fresh), Mergeable, Union)
import HieraSynth.Context (MonadAngelicContext)
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.TypeSignature (TypeSignature)

typeSketchOp ::
  forall op skOp ctx.
  ( MonadAngelicContext ctx,
    GenSym skOp op,
    OpTyping op ctx,
    Mergeable (OpTypeType op)
  ) =>
  skOp ->
  ctx (TypeSignature (OpTypeType op))
typeSketchOp sketchOp = do
  op :: (Union op) <- fresh sketchOp
  typeOp op
