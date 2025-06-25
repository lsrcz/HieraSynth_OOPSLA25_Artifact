{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Parameter.NarrowingRightShiftOpCode
  ( NarrowingRightShiftOpCode (..),
    mkNSrl,
    mkNSra,
    narrowingRightShiftWidenRhs,
    interpretNarrowingRightShiftOpCode,
    narrowingRightShiftNarrowingResult,
    narrowingRightShiftOpCodeParser,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString)
import Grisette (PPrint (pformat), makePrefixedUnifiedCtor)
import HieraSynth.Util.Parser (CharParser)
import RVV.EvalMode (EvalMode)
import RVV.Semantics.Element (VectorElement)
import RVV.Synthesizer.Parameter.Common
  ( elemSextDouble,
    elemTruncHalf,
    elemZextDouble,
    opCodeParser,
  )
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (Sra, Srl),
    interpretSingleWidthBinaryOpCode,
  )
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data NarrowingRightShiftOpCode = NSrl | NSra

deriveFull noModeDeriveConfig [''NarrowingRightShiftOpCode]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''NarrowingRightShiftOpCode

instance PPrint NarrowingRightShiftOpCode where
  pformat NSrl = "nsrl"
  pformat NSra = "nsra"

narrowingRightShiftWidenRhs ::
  forall mode.
  (EvalMode mode) =>
  NarrowingRightShiftOpCode ->
  VectorElement mode ->
  VectorElement mode
narrowingRightShiftWidenRhs NSrl = elemZextDouble
narrowingRightShiftWidenRhs NSra = elemSextDouble

interpretNarrowingRightShiftOpCode ::
  forall mode.
  (EvalMode mode) =>
  NarrowingRightShiftOpCode ->
  VectorElement mode ->
  VectorElement mode ->
  VectorElement mode
interpretNarrowingRightShiftOpCode NSrl = interpretSingleWidthBinaryOpCode Srl
interpretNarrowingRightShiftOpCode NSra = interpretSingleWidthBinaryOpCode Sra

narrowingRightShiftNarrowingResult ::
  forall mode.
  (EvalMode mode) =>
  NarrowingRightShiftOpCode ->
  VectorElement mode ->
  VectorElement mode
narrowingRightShiftNarrowingResult _ = elemTruncHalf

{-# INLINE narrowingRightShiftOpCodeNames #-}
narrowingRightShiftOpCodeNames :: (IsString s) => [(s, NarrowingRightShiftOpCode)]
narrowingRightShiftOpCodeNames =
  [ ("nsrl", NSrl),
    ("nsra", NSra)
  ]

narrowingRightShiftOpCodeParser ::
  (CharParser e s m) => s -> m (Identity NarrowingRightShiftOpCode)
narrowingRightShiftOpCodeParser =
  fmap Identity . opCodeParser narrowingRightShiftOpCodeNames "v"
