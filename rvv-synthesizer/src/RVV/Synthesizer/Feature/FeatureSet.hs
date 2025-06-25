{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet (..),
    Feature (..),
    Signedness (..),
    wideVectorConfigFeatures,
    narrowVectorConfigFeatures,
    featureSetDifference,
    maskMulFeature,
    widthMulFeature,
    vectorConfigFeature,
    wideningVectorConfigFeature,
    narrowingVectorConfigFeature,
    maskingFeature,
    linearArithFeature,
    saturateLinearArithFeature,
    multiplicationFeature,
    multiplicationHighFeature,
    signedFeature,
    FixedPointRoundingModeFeature (..),
  )
where

import qualified Data.HashSet as HS
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.Policy (Policy)
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    narrowVectorConfig,
    vectorMaskMul,
    widenVectorConfig,
  )
import RVV.Synthesizer.Parameter.Masking (Masking (UseProvidedMask))
import RVV.Util.Derive (deriveNoSymEval)

data Feature
  = Widening
  | Narrowing
  | FixedPointClip
  | LinearArith
  | Multiplication
  | MultiplicationHigh
  | Logical
  | Masking
  | Slide
  | Slide1
  | PartialVL
  | FullVL
  | VLToScalarFeature
  | Compare
  | SaturateLinearArith
  | GetSet
  | SetMask
  | GetVlenb
  | Broadcast

deriveNoSymEval [''Feature]

data Signedness = Signed | Unsigned

deriveNoSymEval [''Signedness]

data FixedPointRoundingModeFeature
  = FixedRNUFeature
  | FixedRNEFeature
  | FixedRDNFeature
  | FixedRODFeature

deriveNoSymEval [''FixedPointRoundingModeFeature]

data FeatureSet
  = FeatureSet
  { vectorConfigFeatures :: HS.HashSet VectorConfig,
    maskMulFeatures :: HS.HashSet MaskMul,
    widthMulFeatures :: HS.HashSet WidthMul,
    policyFeatures :: HS.HashSet (Policy 'C),
    opFeatures :: HS.HashSet Feature,
    signednessFeatures :: HS.HashSet Signedness,
    roundingModeFeatures :: HS.HashSet FixedPointRoundingModeFeature
  }

deriveNoSymEval [''FeatureSet]

featureSetDifference :: FeatureSet -> FeatureSet -> FeatureSet
featureSetDifference
  (FeatureSet vectorConfigs1 maskMuls1 scalarXMuls1 policies1 features1 signedness1 roundingModes1)
  (FeatureSet vectorConfigs2 maskMuls2 scalarXMuls2 policies2 features2 signedness2 roundingModes2) =
    FeatureSet
      (HS.difference vectorConfigs1 vectorConfigs2)
      (HS.difference maskMuls1 maskMuls2)
      (HS.difference scalarXMuls1 scalarXMuls2)
      (HS.difference policies1 policies2)
      (HS.difference features1 features2)
      (HS.difference signedness1 signedness2)
      (HS.difference roundingModes1 roundingModes2)

wideVectorConfigFeatures :: FeatureSet -> HS.HashSet VectorConfig
wideVectorConfigFeatures set =
  HS.filter
    (\v -> HS.member (narrowVectorConfig v) (vectorConfigFeatures set))
    (vectorConfigFeatures set)

narrowVectorConfigFeatures :: FeatureSet -> HS.HashSet VectorConfig
narrowVectorConfigFeatures set =
  HS.filter
    (\v -> HS.member (widenVectorConfig v) (vectorConfigFeatures set))
    (vectorConfigFeatures set)

instance Semigroup FeatureSet where
  ( FeatureSet
      vectorConfigs1
      maskMuls1
      scalarXMuls1
      policies1
      features1
      signedness1
      roundingModes1
    )
    <> ( FeatureSet
           vectorConfigs2
           maskMuls2
           scalarXMuls2
           policies2
           features2
           signedness2
           roundingModes2
         ) =
      FeatureSet
        (HS.union vectorConfigs1 vectorConfigs2)
        (HS.union maskMuls1 maskMuls2)
        (HS.union scalarXMuls1 scalarXMuls2)
        (HS.union policies1 policies2)
        (HS.union features1 features2)
        (HS.union signedness1 signedness2)
        (HS.union roundingModes1 roundingModes2)

instance Monoid FeatureSet where
  mempty =
    FeatureSet HS.empty HS.empty HS.empty HS.empty HS.empty HS.empty HS.empty

maskMulFeature :: MaskMul -> FeatureSet
maskMulFeature maskMul = mempty {maskMulFeatures = HS.singleton maskMul}

widthMulFeature :: WidthMul -> FeatureSet
widthMulFeature widthMul = mempty {widthMulFeatures = HS.singleton widthMul}

vectorConfigFeature :: VectorConfig -> FeatureSet
vectorConfigFeature vectorConfig =
  mempty
    { vectorConfigFeatures = HS.singleton vectorConfig,
      maskMulFeatures = HS.singleton $ vectorMaskMul vectorConfig,
      widthMulFeatures = HS.singleton $ elementWidthMul vectorConfig
    }

wideningVectorConfigFeature :: VectorConfig -> FeatureSet
wideningVectorConfigFeature vectorConfig =
  vectorConfigFeature (narrowVectorConfig vectorConfig)
    <> vectorConfigFeature vectorConfig
    <> mempty {opFeatures = HS.singleton Widening}

narrowingVectorConfigFeature :: VectorConfig -> FeatureSet
narrowingVectorConfigFeature vectorConfig =
  vectorConfigFeature (widenVectorConfig vectorConfig)
    <> vectorConfigFeature vectorConfig
    <> mempty {opFeatures = HS.singleton Narrowing}

maskingFeature :: Masking -> FeatureSet
maskingFeature masking =
  mempty
    { opFeatures =
        if masking == UseProvidedMask
          then HS.singleton Masking
          else HS.empty
    }

linearArithFeature :: FeatureSet
linearArithFeature = mempty {opFeatures = HS.singleton LinearArith}

saturateLinearArithFeature :: FeatureSet
saturateLinearArithFeature =
  linearArithFeature {opFeatures = HS.singleton SaturateLinearArith}

multiplicationFeature :: FeatureSet
multiplicationFeature = mempty {opFeatures = HS.singleton Multiplication}

multiplicationHighFeature :: FeatureSet
multiplicationHighFeature = mempty {opFeatures = HS.singleton MultiplicationHigh}

signedFeature :: [Signedness] -> FeatureSet
signedFeature s = mempty {signednessFeatures = HS.fromList s}
