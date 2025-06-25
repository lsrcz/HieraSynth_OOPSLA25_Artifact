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

module RVV.Synthesizer.Parameter.WideningMulAddOpCode
  ( mkWMAccu,
    mkWMAcc,
    mkWMAccsu,
    mkWMAccus,
    WideningMulAddOpCode (..),
    wideningMulAddOpCodeWidenLhs,
    wideningMulAddOpCodeWidenRhs,
    wideningMulAddOpCodeParser,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString)
import Grisette (PPrint (pformat), makePrefixedUnifiedCtor)
import HieraSynth.Util.Parser (CharParser)
import Grisette.Unified (GetSomeWordN)
import RVV.EvalMode (EvalMode)
import RVV.Synthesizer.Parameter.Common (bvSextDouble, bvZextDouble, opCodeParser)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data WideningMulAddOpCode
  = WMAccu
  | WMAcc
  | WMAccsu
  | WMAccus

deriveFull noModeDeriveConfig [''WideningMulAddOpCode]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''WideningMulAddOpCode

wideningMulAddOpCodeWidenLhs ::
  forall mode.
  (EvalMode mode) =>
  WideningMulAddOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
wideningMulAddOpCodeWidenLhs WMAcc = bvSextDouble
wideningMulAddOpCodeWidenLhs WMAccsu = bvSextDouble
wideningMulAddOpCodeWidenLhs _ = bvZextDouble

wideningMulAddOpCodeWidenRhs ::
  forall mode.
  (EvalMode mode) =>
  WideningMulAddOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
wideningMulAddOpCodeWidenRhs WMAcc = bvSextDouble
wideningMulAddOpCodeWidenRhs WMAccus = bvSextDouble
wideningMulAddOpCodeWidenRhs _ = bvZextDouble

instance PPrint WideningMulAddOpCode where
  pformat WMAccu = "wmaccu"
  pformat WMAcc = "wmacc"
  pformat WMAccsu = "wmaccsu"
  pformat WMAccus = "wmaccus"

{-# INLINE wideningMulAddOpCodeNames #-}
wideningMulAddOpCodeNames :: (IsString s) => [(s, WideningMulAddOpCode)]
wideningMulAddOpCodeNames =
  [ ("wmaccu", WMAccu),
    ("wmacc", WMAcc),
    ("wmaccus", WMAccus),
    ("wmaccsu", WMAccsu)
  ]

wideningMulAddOpCodeParser ::
  (CharParser e s m) => s -> m (Identity WideningMulAddOpCode)
wideningMulAddOpCodeParser =
  fmap Identity . opCodeParser wideningMulAddOpCodeNames "v"
