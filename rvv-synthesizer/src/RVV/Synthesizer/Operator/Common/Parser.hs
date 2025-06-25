{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.Common.Parser
  ( immParser,
    vtypeDestMaskParser,
    vtypeDestParser,
    vtypeMaskParser,
    vtypeImmDestParser,
    vtypeImmDestMaskParser,
  )
where

import Control.Monad.Identity (Identity)
import HieraSynth.Util.Parser
  ( CharParser,
    bracketCommaSep2,
    bracketCommaSep3,
    bracketCommaSep4,
    maybeNamed,
    named,
  )
import Grisette.Unified
  ( EvalModeTag (C),
  )
import RVV.Parser.ArgParser
  ( destinationParser,
    hexadecimalParser,
    maskingParser,
    vectorConfigParser,
  )
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)

vtypeDestMaskParser ::
  (CharParser e s m) =>
  Maybe s ->
  m (VectorConfig, Identity Destination, Identity Masking)
vtypeDestMaskParser vectorConfigName =
  bracketCommaSep3
    (maybeNamed vectorConfigName vectorConfigParser)
    destinationParser
    maskingParser

vtypeDestParser ::
  (CharParser e s m) =>
  Maybe s ->
  m (VectorConfig, Identity Destination)
vtypeDestParser vectorConfigName =
  bracketCommaSep2
    (maybeNamed vectorConfigName vectorConfigParser)
    destinationParser

vtypeMaskParser ::
  (CharParser e s m) =>
  Maybe s ->
  m (VectorConfig, Identity Masking)
vtypeMaskParser vectorConfigName =
  bracketCommaSep2
    (maybeNamed vectorConfigName vectorConfigParser)
    maskingParser

immParser :: (CharParser e s m) => m (Imm 'C)
immParser = ConstImm <$> hexadecimalParser

vtypeImmDestParser ::
  (CharParser e s m) =>
  Maybe s ->
  s ->
  m (VectorConfig, Imm 'C, Identity Destination)
vtypeImmDestParser vectorConfigName immName =
  bracketCommaSep3
    (maybeNamed vectorConfigName vectorConfigParser)
    (named immName immParser)
    destinationParser

vtypeImmDestMaskParser ::
  (CharParser e s m) =>
  Maybe s ->
  s ->
  m (VectorConfig, Imm 'C, Identity Destination, Identity Masking)
vtypeImmDestMaskParser vectorConfigName immName =
  bracketCommaSep4
    (maybeNamed vectorConfigName vectorConfigParser)
    (named immName immParser)
    destinationParser
    maskingParser
