{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Parameter.IntCompareOpCode
  ( IntCompareOpCode (..),
    mkMSEq,
    mkMSNe,
    mkMSLt,
    mkMSLtu,
    mkMSLe,
    mkMSLeu,
    mkMSGt,
    mkMSGtu,
    mkMSGe,
    mkMSGeu,
    commutativeIntCompareOpCode,
    interpretIntCompareOpCode,
    intCompareOpCodeParser,
    intCompareOpCodeFeature,
  )
where

import Data.Functor.Identity (Identity (Identity))
import qualified Data.HashSet as HS
import Data.String (IsString)
import Grisette
  ( LogicalOp (symNot, (.&&), (.||)),
    PPrint (pformat),
    SignConversion (toSigned),
    makePrefixedUnifiedCtor,
  )
import Grisette.Internal.Unified.UnifiedBV (GetSomeWordN)
import HieraSynth.Util.Parser (CharParser)
import Grisette.Unified (GetBool, symMsb, (./=), (.<), (.<=), (.==), (.>), (.>=))
import RVV.Circuit.ParallelFoldable (ParallelFoldMethod (BrentKung, Sklansky), parallelPrefixEq, parallelPrefixNeq, parallelPrefixSge, parallelPrefixSgt, parallelPrefixSle, parallelPrefixSlt, parallelPrefixUge, parallelPrefixUgt, parallelPrefixUle, parallelPrefixUlt)
import RVV.EvalMode (EvalMode)
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (Compare, Masking),
    FeatureSet (opFeatures),
    Signedness (Signed, Unsigned),
    signedFeature,
  )
import RVV.Synthesizer.Parameter.Common (opCodeParser)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data IntCompareOpCode
  = MSEq
  | MSNe
  | MSLt
  | MSLtu
  | MSLe
  | MSLeu
  | MSGt
  | MSGtu
  | MSGe
  | MSGeu

commutativeIntCompareOpCode :: [IntCompareOpCode]
commutativeIntCompareOpCode = [MSEq, MSNe]

deriveFull noModeDeriveConfig [''IntCompareOpCode]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''IntCompareOpCode

instance PPrint IntCompareOpCode where
  pformat MSEq = "mseq"
  pformat MSNe = "msne"
  pformat MSLt = "mslt"
  pformat MSLtu = "msltu"
  pformat MSLe = "msle"
  pformat MSLeu = "msleu"
  pformat MSGt = "msgt"
  pformat MSGtu = "msgtu"
  pformat MSGe = "msge"
  pformat MSGeu = "msgeu"

interpretIntCompareOpCodeSklansky ::
  (EvalMode mode) =>
  IntCompareOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
interpretIntCompareOpCodeSklansky MSEq l r = parallelPrefixEq Sklansky l r -- l .== r
interpretIntCompareOpCodeSklansky MSNe l r = parallelPrefixNeq Sklansky l r -- l ./= r
interpretIntCompareOpCodeSklansky MSLt l r = parallelPrefixSlt Sklansky l r -- toSigned l .< toSigned r
interpretIntCompareOpCodeSklansky MSLtu l r = parallelPrefixUlt Sklansky l r -- l .< r
interpretIntCompareOpCodeSklansky MSLe l r = parallelPrefixSle Sklansky l r -- toSigned l .<= toSigned r
interpretIntCompareOpCodeSklansky MSLeu l r = parallelPrefixUle Sklansky l r -- l .<= r
interpretIntCompareOpCodeSklansky MSGt l r = parallelPrefixSgt Sklansky l r -- toSigned l .> toSigned r
interpretIntCompareOpCodeSklansky MSGtu l r = parallelPrefixUgt Sklansky l r -- l .> r
interpretIntCompareOpCodeSklansky MSGe l r = parallelPrefixSge Sklansky l r -- toSigned l .>= toSigned r
interpretIntCompareOpCodeSklansky MSGeu l r = parallelPrefixUge Sklansky l r -- l .>= r

interpretIntCompareOpCodeBrentKung ::
  (EvalMode mode) =>
  IntCompareOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
interpretIntCompareOpCodeBrentKung MSEq l r = parallelPrefixEq (BrentKung []) l r -- l .== r
interpretIntCompareOpCodeBrentKung MSNe l r = parallelPrefixNeq (BrentKung []) l r -- l ./= r
interpretIntCompareOpCodeBrentKung MSLt l r = parallelPrefixSlt (BrentKung []) l r -- toSigned l .< toSigned r
interpretIntCompareOpCodeBrentKung MSLtu l r = parallelPrefixUlt (BrentKung []) l r -- l .< r
interpretIntCompareOpCodeBrentKung MSLe l r = parallelPrefixSle (BrentKung []) l r -- toSigned l .<= toSigned r
interpretIntCompareOpCodeBrentKung MSLeu l r = parallelPrefixUle (BrentKung []) l r -- l .<= r
interpretIntCompareOpCodeBrentKung MSGt l r = parallelPrefixSgt (BrentKung []) l r -- toSigned l .> toSigned r
interpretIntCompareOpCodeBrentKung MSGtu l r = parallelPrefixUgt (BrentKung []) l r -- l .> r
interpretIntCompareOpCodeBrentKung MSGe l r = parallelPrefixSge (BrentKung []) l r -- toSigned l .>= toSigned r
interpretIntCompareOpCodeBrentKung MSGeu l r = parallelPrefixUge (BrentKung []) l r -- l .>= r

interpretIntCompareOpCodeDefault ::
  (EvalMode mode) =>
  IntCompareOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
interpretIntCompareOpCodeDefault MSEq l r = l .== r
interpretIntCompareOpCodeDefault MSNe l r = l ./= r
interpretIntCompareOpCodeDefault MSLt l r = toSigned l .< toSigned r
interpretIntCompareOpCodeDefault MSLtu l r = l .< r
interpretIntCompareOpCodeDefault MSLe l r = toSigned l .<= toSigned r
interpretIntCompareOpCodeDefault MSLeu l r = l .<= r
interpretIntCompareOpCodeDefault MSGt l r = toSigned l .> toSigned r
interpretIntCompareOpCodeDefault MSGtu l r = l .> r
interpretIntCompareOpCodeDefault MSGe l r = toSigned l .>= toSigned r
interpretIntCompareOpCodeDefault MSGeu l r = l .>= r

lt :: forall mode. (EvalMode mode) => GetSomeWordN mode -> GetSomeWordN mode -> GetBool mode
lt l r = (lmsb .&& symNot rmsb) .|| (lmsb .== rmsb .&& l .< r)
  where
    lmsb = symMsb @mode l
    rmsb = symMsb @mode r

interpretIntCompareOpCodeShared ::
  (EvalMode mode) =>
  IntCompareOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
interpretIntCompareOpCodeShared MSEq l r = l .== r
interpretIntCompareOpCodeShared MSNe l r = l ./= r
interpretIntCompareOpCodeShared MSLt l r = lt l r
interpretIntCompareOpCodeShared MSLtu l r = l .< r
interpretIntCompareOpCodeShared MSLe l r = lt l r .|| l .== r
interpretIntCompareOpCodeShared MSLeu l r = l .< r .|| l .== r
interpretIntCompareOpCodeShared MSGt l r = symNot $ interpretIntCompareOpCodeShared MSLe l r
interpretIntCompareOpCodeShared MSGtu l r = symNot $ interpretIntCompareOpCodeShared MSLeu l r
interpretIntCompareOpCodeShared MSGe l r = symNot $ interpretIntCompareOpCodeShared MSLt l r
interpretIntCompareOpCodeShared MSGeu l r = symNot $ interpretIntCompareOpCodeShared MSLtu l r

interpretIntCompareOpCode ::
  (EvalMode mode) =>
  IntCompareOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
interpretIntCompareOpCode = interpretIntCompareOpCodeShared

{-# INLINE intCompareOpCodeNames #-}
intCompareOpCodeNames :: (IsString s) => [(s, IntCompareOpCode)]
intCompareOpCodeNames =
  [ ("mseq", MSEq),
    ("msne", MSNe),
    ("mslt", MSLt),
    ("msltu", MSLtu),
    ("msle", MSLe),
    ("msleu", MSLeu),
    ("msge", MSGe),
    ("msgeu", MSGeu),
    ("msgt", MSGt),
    ("msgtu", MSGtu)
  ]

intCompareOpCodeParser ::
  (CharParser e s m) => s -> m (Identity IntCompareOpCode)
intCompareOpCodeParser = fmap Identity . opCodeParser intCompareOpCodeNames "v"

intCompareOpCodeFeature :: IntCompareOpCode -> FeatureSet
intCompareOpCodeFeature op =
  mempty {opFeatures = HS.fromList [Compare, Masking]}
    <> ( case op of
           MSLe -> signedFeature [Signed]
           MSLt -> signedFeature [Signed]
           MSGe -> signedFeature [Signed]
           MSGt -> signedFeature [Signed]
           MSLeu -> signedFeature [Unsigned]
           MSLtu -> signedFeature [Unsigned]
           MSGeu -> signedFeature [Unsigned]
           MSGtu -> signedFeature [Unsigned]
           MSEq -> signedFeature []
           MSNe -> signedFeature []
       )
