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

module RVV.Synthesizer.Parameter.FixedPointRoundingMode
  ( FixedPointRoundingMode (..),
    mkFixedRNU,
    mkFixedRNE,
    mkFixedRDN,
    mkFixedROD,
    interpretFixedPointClip,
    fixedPointRoundingModeFeature,
    fixedPointClipSignedRoundingModeParser,
  )
where

import Data.Bifunctor (second)
import Data.Bits
  ( FiniteBits (finiteBitSize),
  )
import Data.Functor.Identity (Identity (Identity))
import qualified Data.HashSet as HS
import Data.String (IsString)
import Grisette
  ( BV (bv, bvSelect, bvZext),
    LogicalOp (symNot, (.&&), (.||)),
    PPrint (pformat),
    SignConversion (toSigned, toUnsigned),
    SymShift (symShift, symShiftNegated),
    makePrefixedUnifiedCtor,
  )
import HieraSynth.Util.Parser (CharParser)
import Grisette.Unified
  ( GetBool,
    GetSomeWordN,
    symIte,
    symTestBit,
    (./=),
  )
import RVV.EvalMode (EvalMode)
import RVV.Semantics.Element (VectorElement (VectorElement))
import RVV.Synthesizer.Feature.FeatureSet (FeatureSet (roundingModeFeatures), FixedPointRoundingModeFeature (FixedRDNFeature, FixedRNEFeature, FixedRNUFeature, FixedRODFeature))
import RVV.Synthesizer.Parameter.Common
  ( combineAnyInvalid,
    opCodeParser,
    shiftBits,
  )
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data FixedPointRoundingMode = FixedRNU | FixedRNE | FixedRDN | FixedROD

deriveFull noModeDeriveConfig [''FixedPointRoundingMode]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''FixedPointRoundingMode

instance PPrint FixedPointRoundingMode where
  pformat FixedRNU = "rnu"
  pformat FixedRNE = "rne"
  pformat FixedRDN = "rdn"
  pformat FixedROD = "rod"

interpretFixedPointClip ::
  forall mode.
  (EvalMode mode) =>
  FixedPointRoundingMode ->
  GetBool mode ->
  VectorElement mode ->
  VectorElement mode ->
  VectorElement mode
interpretFixedPointClip mode signed (VectorElement v vi) (VectorElement s si) =
  let bs = finiteBitSize v
      halfBs = finiteBitSize s
      shiftAmount = shiftBits $ bvZext bs s
      lshiftAmount = fromIntegral (bs - 1) - shiftAmount
      rshifted =
        bvSelect 0 halfBs $
          symIte
            signed
            (toUnsigned $ symShiftNegated (toSigned v) (toSigned shiftAmount))
            (symShiftNegated v shiftAmount)
      lshifted = symShift v lshiftAmount
      vd = symTestBit @mode lshifted (bs - 1) :: GetBool mode
      vdm1 = symTestBit @mode lshifted (bs - 2) :: GetBool mode
      vdm10 = bvSelect 0 (bs - 1) lshifted
      vdm20 = bvSelect 0 (bs - 2) lshifted
      bv1 = bv halfBs 1 :: GetSomeWordN mode
      bv0 = bv halfBs 0 :: GetSomeWordN mode
      and :: forall mode. (EvalMode mode) => GetBool mode -> GetBool mode -> GetBool mode
      and a b = a .&& b
      or :: forall mode. (EvalMode mode) => GetBool mode -> GetBool mode -> GetBool mode
      or a b = a .|| b
      incr = case mode of
        FixedRNU -> symIte @mode vdm1 bv1 bv0
        FixedRNE -> symIte @mode (and vdm1 (or (vdm20 ./= 0) vd)) bv1 bv0
        FixedRDN -> bv0
        FixedROD -> symIte @mode (and (symNot vd) (vdm10 ./= 0)) bv1 bv0
   in VectorElement
        (rshifted + incr)
        (bvSelect 0 halfBs (combineAnyInvalid vi (shiftBits (bvZext bs si))))

fixedPointRoundingModeFeature :: FixedPointRoundingMode -> FeatureSet
fixedPointRoundingModeFeature FixedRNU =
  mempty {roundingModeFeatures = HS.singleton FixedRNUFeature}
fixedPointRoundingModeFeature FixedRNE =
  mempty {roundingModeFeatures = HS.singleton FixedRNEFeature}
fixedPointRoundingModeFeature FixedRDN =
  mempty {roundingModeFeatures = HS.singleton FixedRDNFeature}
fixedPointRoundingModeFeature FixedROD =
  mempty {roundingModeFeatures = HS.singleton FixedRODFeature}

{-# INLINE fixedPointClipSignedRoundingModeNames #-}
fixedPointClipSignedRoundingModeNames ::
  (IsString s) => [(s, (Bool, FixedPointRoundingMode))]
fixedPointClipSignedRoundingModeNames =
  [ ("nclip.rnu", (True, FixedRNU)),
    ("nclipu.rnu", (False, FixedRNU)),
    ("nclip.rne", (True, FixedRNE)),
    ("nclipu.rne", (False, FixedRNE)),
    ("nclip.rdn", (True, FixedRDN)),
    ("nclipu.rdn", (False, FixedRDN)),
    ("nclip.rod", (True, FixedROD)),
    ("nclipu.rod", (False, FixedROD))
  ]

fixedPointClipSignedRoundingModeParser ::
  (CharParser e s m) => s -> m (Bool, Identity FixedPointRoundingMode)
fixedPointClipSignedRoundingModeParser =
  fmap (second Identity)
    . opCodeParser fixedPointClipSignedRoundingModeNames "v"
