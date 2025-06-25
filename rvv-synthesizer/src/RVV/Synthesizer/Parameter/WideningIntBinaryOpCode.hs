{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Parameter.WideningIntBinaryOpCode
  ( WideningIntBinaryOpCode (..),
    mkWAddu,
    mkWSubu,
    mkWAdd,
    mkWSub,
    mkWMul,
    mkWMulu,
    mkWMulsu,
    commutativeWideningIntBinaryOpCode,
    interpretWideningIntBinaryOpCode,
    wideningIntBinaryOpCodeWidenLhs,
    wideningIntBinaryOpCodeWidenRhs,
    wideningIntBinaryOpCodeParser,
    wideningIntBinaryOpCodeFeature,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString)
import Grisette (PPrint (pformat), makePrefixedUnifiedCtor)
import HieraSynth.Util.Parser (CharParser)
import RVV.EvalMode (EvalMode)
import RVV.Semantics.Element (VectorElement (VectorElement))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    Signedness (Signed, Unsigned),
    linearArithFeature,
    multiplicationFeature,
    multiplicationHighFeature,
    signedFeature,
  )
import RVV.Synthesizer.Parameter.Common (combineArithInvalid, elemSextDouble, elemZextDouble, opCodeParser)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data WideningIntBinaryOpCode
  = WAddu
  | WSubu
  | WAdd
  | WSub
  | WMul
  | WMulu
  | WMulsu

commutativeWideningIntBinaryOpCode :: [WideningIntBinaryOpCode]
commutativeWideningIntBinaryOpCode = [WAddu, WAdd, WMul, WMulu]

deriveFull noModeDeriveConfig [''WideningIntBinaryOpCode]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''WideningIntBinaryOpCode

interpretWideningIntBinaryOpCode ::
  forall mode.
  (EvalMode mode) =>
  WideningIntBinaryOpCode ->
  VectorElement mode ->
  VectorElement mode ->
  VectorElement mode
interpretWideningIntBinaryOpCode WAddu (VectorElement l li) (VectorElement r ri) =
  VectorElement (l + r) (combineArithInvalid li ri)
interpretWideningIntBinaryOpCode WSubu (VectorElement l li) (VectorElement r ri) =
  VectorElement (l - r) (combineArithInvalid li ri)
interpretWideningIntBinaryOpCode WAdd (VectorElement l li) (VectorElement r ri) =
  VectorElement (l + r) (combineArithInvalid li ri)
interpretWideningIntBinaryOpCode WSub (VectorElement l li) (VectorElement r ri) =
  VectorElement (l - r) (combineArithInvalid li ri)
interpretWideningIntBinaryOpCode WMul (VectorElement l li) (VectorElement r ri) =
  VectorElement (l * r) (combineArithInvalid li ri)
interpretWideningIntBinaryOpCode WMulu (VectorElement l li) (VectorElement r ri) =
  VectorElement (l * r) (combineArithInvalid li ri)
interpretWideningIntBinaryOpCode WMulsu (VectorElement l li) (VectorElement r ri) =
  VectorElement (l * r) (combineArithInvalid li ri)

wideningIntBinaryOpCodeWidenBoth ::
  forall mode.
  (EvalMode mode) =>
  WideningIntBinaryOpCode ->
  VectorElement mode ->
  VectorElement mode
wideningIntBinaryOpCodeWidenBoth WAddu = elemZextDouble
wideningIntBinaryOpCodeWidenBoth WSubu = elemZextDouble
wideningIntBinaryOpCodeWidenBoth WAdd = elemSextDouble
wideningIntBinaryOpCodeWidenBoth WSub = elemSextDouble
wideningIntBinaryOpCodeWidenBoth WMul = elemSextDouble
wideningIntBinaryOpCodeWidenBoth WMulu = elemZextDouble
wideningIntBinaryOpCodeWidenBoth WMulsu =
  error "VWMulsu need different widening for lhs and rhs"

wideningIntBinaryOpCodeWidenLhs ::
  forall mode.
  (EvalMode mode) =>
  WideningIntBinaryOpCode ->
  VectorElement mode ->
  VectorElement mode
wideningIntBinaryOpCodeWidenLhs WMulsu = elemSextDouble
wideningIntBinaryOpCodeWidenLhs op = wideningIntBinaryOpCodeWidenBoth op

wideningIntBinaryOpCodeWidenRhs ::
  forall mode.
  (EvalMode mode) =>
  WideningIntBinaryOpCode ->
  VectorElement mode ->
  VectorElement mode
wideningIntBinaryOpCodeWidenRhs WMulsu = elemZextDouble
wideningIntBinaryOpCodeWidenRhs op = wideningIntBinaryOpCodeWidenBoth op

instance PPrint WideningIntBinaryOpCode where
  pformat WAddu = "waddu"
  pformat WSubu = "wsubu"
  pformat WAdd = "wadd"
  pformat WSub = "wsub"
  pformat WMul = "wmul"
  pformat WMulu = "wmulu"
  pformat WMulsu = "wmulsu"

{-# INLINE wideningIntBinaryOpCodeNames #-}
wideningIntBinaryOpCodeNames :: (IsString s) => [(s, WideningIntBinaryOpCode)]
wideningIntBinaryOpCodeNames =
  [ ("waddu", WAddu),
    ("wadd", WAdd),
    ("wsubu", WSubu),
    ("wsub", WSub),
    ("wmulu", WMulu),
    ("wmulsu", WMulsu),
    ("wmul", WMul)
  ]

wideningIntBinaryOpCodeParser ::
  (CharParser e s m) => s -> m (Identity WideningIntBinaryOpCode)
wideningIntBinaryOpCodeParser =
  fmap Identity . opCodeParser wideningIntBinaryOpCodeNames "v"

wideningIntBinaryOpCodeFeature :: WideningIntBinaryOpCode -> FeatureSet
wideningIntBinaryOpCodeFeature op
  | op `elem` [WAddu, WSubu] = linearArithFeature <> signedFeature [Unsigned]
  | op `elem` [WAdd, WSub] = linearArithFeature <> signedFeature [Signed]
  | op `elem` [WMul, WMulu, WMulsu] =
      multiplicationFeature
        <> multiplicationHighFeature
        <> ( case op of
               WMul -> signedFeature [Signed]
               WMulu -> signedFeature [Unsigned]
               WMulsu -> signedFeature [Signed, Unsigned]
               _ -> error "Should not happen"
           )
  | otherwise = error "Not supported yet"
