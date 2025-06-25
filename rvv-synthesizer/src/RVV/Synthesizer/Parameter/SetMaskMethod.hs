{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Parameter.SetMaskMethod
  ( SetMaskMethod (..),
    mkBeforeFirst,
    mkIncludingFirst,
    mkOnlyFirst,
    setCurBitWithMethod,
    setMaskMethodParser,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString)
import Grisette
  ( LogicalOp (symNot, true),
    PPrint (pformat),
    makePrefixedUnifiedCtor,
  )
import HieraSynth.Util.Parser (CharParser)
import Grisette.Unified (GetBool)
import RVV.EvalMode (EvalMode)
import RVV.Synthesizer.Parameter.Common (opCodeParser)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data SetMaskMethod = BeforeFirst | IncludingFirst | OnlyFirst

deriveFull noModeDeriveConfig [''SetMaskMethod]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''SetMaskMethod

instance PPrint SetMaskMethod where
  pformat BeforeFirst = "sbf"
  pformat IncludingFirst = "sif"
  pformat OnlyFirst = "sof"

setCurBitWithMethod ::
  (EvalMode mode) => SetMaskMethod -> GetBool mode -> GetBool mode
setCurBitWithMethod BeforeFirst = symNot
setCurBitWithMethod IncludingFirst = const true
setCurBitWithMethod OnlyFirst = id

{-# INLINE setMaskMethodNames #-}
setMaskMethodNames :: (IsString s) => [(s, SetMaskMethod)]
setMaskMethodNames =
  [ ("msbf", BeforeFirst),
    ("msif", IncludingFirst),
    ("msof", OnlyFirst)
  ]

setMaskMethodParser ::
  (CharParser e s m) => s -> m (Identity SetMaskMethod)
setMaskMethodParser = fmap Identity . opCodeParser setMaskMethodNames "v"
