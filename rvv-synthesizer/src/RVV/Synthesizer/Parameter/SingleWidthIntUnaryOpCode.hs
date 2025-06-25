{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Parameter.SingleWidthIntUnaryOpCode
  ( SingleWidthIntUnaryOpCode (..),
    mkNeg,
    mkNot,
    mkSeqz,
    mkSnez,
    mkSltz,
    mkSgtz,
    mkCPop,
    mkClz,
    mkCtz,
    interpretSingleWidthIntUnaryOpCode,
    singleWidthIntUnaryOpParser,
    singleWidthIntUnaryOpCodeFeature,
  )
where

import Data.Bits (Bits (complement), FiniteBits (finiteBitSize))
import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString)
import Grisette (BV (bv), PPrint (pformat), makePrefixedUnifiedCtor)
import HieraSynth.Util.Parser (CharParser)
import Grisette.Unified
  ( symCountLeadingZeros,
    symCountTrailingZeros,
    symIte,
    symPopCount,
    (.<),
    (.==),
    (.>),
  )
import RVV.EvalMode (EvalMode)
import RVV.Semantics.Element (VectorElement (VectorElement))
import RVV.Synthesizer.Feature.FeatureSet (FeatureSet)
import RVV.Synthesizer.Parameter.Common (anyInvalid, opCodeParser)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data SingleWidthIntUnaryOpCode
  = Neg
  | Not
  | Seqz
  | Snez
  | Sltz
  | Sgtz
  | CPop
  | Clz
  | Ctz

deriveFull noModeDeriveConfig [''SingleWidthIntUnaryOpCode]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''SingleWidthIntUnaryOpCode

instance PPrint SingleWidthIntUnaryOpCode where
  pformat Neg = "neg"
  pformat Not = "not"
  pformat Seqz = "seqz"
  pformat Snez = "snez"
  pformat Sltz = "sltz"
  pformat Sgtz = "sgtz"
  pformat CPop = "cpop"
  pformat Clz = "clz"
  pformat Ctz = "ctz"

interpretSingleWidthIntUnaryOpCode ::
  forall mode.
  (EvalMode mode) =>
  SingleWidthIntUnaryOpCode ->
  VectorElement mode ->
  VectorElement mode
interpretSingleWidthIntUnaryOpCode Neg (VectorElement v vi) =
  VectorElement (-v) (anyInvalid vi)
interpretSingleWidthIntUnaryOpCode Not (VectorElement v vi) =
  VectorElement (complement v) vi
interpretSingleWidthIntUnaryOpCode Seqz (VectorElement v vi) =
  VectorElement
    (symIte @mode (v .== 0) (bv (finiteBitSize v) 1) (bv (finiteBitSize v) 0))
    (anyInvalid vi)
interpretSingleWidthIntUnaryOpCode Snez (VectorElement v vi) =
  VectorElement
    (symIte @mode (v .== 0) (bv (finiteBitSize v) 0) (bv (finiteBitSize v) 1))
    (anyInvalid vi)
interpretSingleWidthIntUnaryOpCode Sltz (VectorElement v vi) =
  VectorElement
    (symIte @mode (v .< 0) (bv (finiteBitSize v) 1) (bv (finiteBitSize v) 0))
    (anyInvalid vi)
interpretSingleWidthIntUnaryOpCode Sgtz (VectorElement v vi) =
  VectorElement
    (symIte @mode (v .> 0) (bv (finiteBitSize v) 1) (bv (finiteBitSize v) 0))
    (anyInvalid vi)
interpretSingleWidthIntUnaryOpCode CPop (VectorElement v vi) =
  VectorElement
    (symPopCount @mode v)
    (anyInvalid vi)
interpretSingleWidthIntUnaryOpCode Clz (VectorElement v vi) =
  VectorElement
    (symCountLeadingZeros @mode v)
    (anyInvalid vi)
interpretSingleWidthIntUnaryOpCode Ctz (VectorElement v vi) =
  VectorElement
    (symCountTrailingZeros @mode v)
    (anyInvalid vi)

{-# INLINE singleWidthIntUnaryOpNames #-}
singleWidthIntUnaryOpNames :: (IsString s) => [(s, SingleWidthIntUnaryOpCode)]
singleWidthIntUnaryOpNames =
  [ ("not", Not),
    ("neg", Neg),
    ("seqz", Seqz),
    ("snez", Snez),
    ("sltz", Sltz),
    ("sgtz", Sgtz),
    ("cpop", CPop),
    ("clz", Clz),
    ("ctz", Ctz)
  ]

singleWidthIntUnaryOpParser ::
  (CharParser e s m) => m (Identity SingleWidthIntUnaryOpCode)
singleWidthIntUnaryOpParser =
  Identity <$> opCodeParser singleWidthIntUnaryOpNames "" ""

singleWidthIntUnaryOpCodeFeature :: SingleWidthIntUnaryOpCode -> FeatureSet
singleWidthIntUnaryOpCodeFeature _ = error "Not supported yet"
