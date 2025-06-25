{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Operator.Common.OpToSym (opToSym) where

import Grisette (ToSym (toSym))
import Grisette.Unified
  ( EvalModeConvertible (withModeConvertible'),
    EvalModeTag (C, S),
  )

opToSym ::
  forall c s a b.
  ( EvalModeConvertible c s,
    (ToSym (a 'C) (b 'C)),
    (ToSym (a 'S) (b 'S)),
    (ToSym (a 'C) (b 'S))
  ) =>
  a c ->
  b s
opToSym = withModeConvertible' @c @s toSym toSym toSym
