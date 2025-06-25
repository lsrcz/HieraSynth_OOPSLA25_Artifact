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

module RVV.Synthesizer.Parameter.SingleWidthMulAddOpCode
  ( SingleWidthMulAddOpCode (..),
    interpretSingleWidthMulAddOpCode,
    mkMAcc,
    mkNMSac,
    mkMAdd,
    mkNMSub,
    singleWidthMulAddOpCodeParser,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString)
import Grisette (PPrint (pformat), makePrefixedUnifiedCtor)
import HieraSynth.Util.Parser (CharParser)
import Grisette.Unified (GetSomeWordN)
import RVV.EvalMode (EvalMode)
import RVV.Synthesizer.Parameter.Common (opCodeParser)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data SingleWidthMulAddOpCode
  = MAcc
  | NMSac
  | MAdd
  | NMSub

deriveFull noModeDeriveConfig [''SingleWidthMulAddOpCode]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''SingleWidthMulAddOpCode

instance PPrint SingleWidthMulAddOpCode where
  pformat MAcc = "macc"
  pformat NMSac = "nmsac"
  pformat MAdd = "madd"
  pformat NMSub = "nmsub"

interpretSingleWidthMulAddOpCode ::
  forall mode.
  (EvalMode mode) =>
  SingleWidthMulAddOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
interpretSingleWidthMulAddOpCode MAcc d l r = l * r + d
interpretSingleWidthMulAddOpCode NMSac d l r = d - l * r
interpretSingleWidthMulAddOpCode MAdd d l r = l * d + r
interpretSingleWidthMulAddOpCode NMSub d l r = r - l * d

{-# INLINE singleWidthMulAddOpCodeNames #-}
singleWidthMulAddOpCodeNames ::
  (IsString s) => [(s, SingleWidthMulAddOpCode)]
singleWidthMulAddOpCodeNames =
  [ ("macc", MAcc),
    ("nmsac", NMSac),
    ("madd", MAdd),
    ("nmsub", NMSub)
  ]

singleWidthMulAddOpCodeParser ::
  (CharParser e s m) =>
  s ->
  m (Identity SingleWidthMulAddOpCode)
singleWidthMulAddOpCodeParser =
  fmap Identity . opCodeParser singleWidthMulAddOpCodeNames "v"
