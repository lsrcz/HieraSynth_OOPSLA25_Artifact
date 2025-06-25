{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.Synthesizer.Feature.ToSketchOp
  ( opListFromFeatures,
    inferSketch,
    sketchFromFeatures,
    inferRestrictedSketch,
    simplifyChoiceTree,
    allowPartialVLFromFeatures,
    toFlatSketchSpec,
    toFlatSketchSpecTable,
  )
where

import Control.Monad (foldM, join)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (zipWith4)
import Data.Maybe (maybeToList)
import Data.Ratio (numerator)
import qualified Data.Text as T
import Grisette (ToSym (toSym), WordN)
import HieraSynth.Combinator.Embed (inj)
import HieraSynth.Combinator.Invoke (Invoke (Invoke))
import HieraSynth.Program.Choice.ChoiceTree (ChoiceMeta (NoSplit, Split), ChoiceTree (Branch, Leaf))
import HieraSynth.Program.Choice.ComponentBag (ComponentBag (ComponentBag))
import HieraSynth.Program.Choice.Counting (SplitChoice (splitChoice))
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.ProgTyping (symbolType)
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.MachineConfig (AllowPartialVL (AllowPartialVL, DisallowPartialVL))
import RVV.Semantics.Multiplier (LengthMul (getLengthMul), MaskMul, WidthMul)
import RVV.Semantics.Policy (Policy, nonePolicy)
import RVV.Semantics.VectorConfig (VectorConfig (lengthMul), halfVectorConfig)
import RVV.Synthesizer.DefaultSynthType (DefaultConProg)
import RVV.Synthesizer.Feature.ExtractFeature (extractProgTableFeature)
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature
      ( Broadcast,
        Compare,
        FixedPointClip,
        FullVL,
        GetSet,
        GetVlenb,
        LinearArith,
        Logical,
        Masking,
        Multiplication,
        MultiplicationHigh,
        Narrowing,
        PartialVL,
        SaturateLinearArith,
        SetMask,
        Slide,
        Slide1,
        VLToScalarFeature
      ),
    FeatureSet
      ( maskMulFeatures,
        opFeatures,
        policyFeatures,
        roundingModeFeatures,
        signednessFeatures,
        vectorConfigFeatures,
        widthMulFeatures
      ),
    FixedPointRoundingModeFeature
      ( FixedRDNFeature,
        FixedRNEFeature,
        FixedRNUFeature,
        FixedRODFeature
      ),
    Signedness (Signed, Unsigned),
    narrowVectorConfigFeatures,
    wideVectorConfigFeatures,
  )
import RVV.Synthesizer.Op
  ( CompSketchSpec,
    SketchChoiceTree,
    SketchOp,
    SketchSpec (CompSpec, ConProg, ConSpec),
    SketchSpecTable,
  )
import RVV.Synthesizer.Operator.Common.ImmSpec
  ( ImmSpec (ArbitraryImmSpec, BoundedImmSpec, ConstImmSpec),
  )
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( SketchRHSSpec (SketchImmRHS, SketchScalarRHS, SketchVectorRHS),
  )
import RVV.Synthesizer.Operator.DelegatedVectorBinaryOnMask
  ( SketchDelegatedVectorBinaryOnMask (SketchDelegatedVectorBinaryOnMask),
  )
import RVV.Synthesizer.Operator.Extract (SketchExtract (SketchExtractVector))
import RVV.Synthesizer.Operator.FixedPointClip
  ( SketchFixedPointClip (SketchFixedPointClip),
  )
import RVV.Synthesizer.Operator.Insert (SketchInsert (SketchInsertVector))
import RVV.Synthesizer.Operator.MaskLogical
  ( SketchMaskLogical (SketchMaskLogical),
  )
import RVV.Synthesizer.Operator.Merge (SketchMerge (SketchMerge))
import RVV.Synthesizer.Operator.MiscMask (SketchMiscMask (SketchMaskSetBit))
import RVV.Synthesizer.Operator.Move (SketchMove (SketchMoveToMask, SketchMoveToVector))
import RVV.Synthesizer.Operator.NarrowingRightShift
  ( SketchNarrowingRightShift (SketchNarrowingRightShift),
  )
import RVV.Synthesizer.Operator.Reinterpret
  ( SketchReinterpret (SketchVLToScalar, SketchVectorToVector),
  )
import RVV.Synthesizer.Operator.Scalar (SketchScalar (SketchScalarLongImm))
import RVV.Synthesizer.Operator.ScalarOperator
  ( SketchScalarOperator (SketchScalarBin, SketchScalarBinVectorLength),
  )
import RVV.Synthesizer.Operator.ScalarTrunc
  ( SketchScalarTrunc (SketchScalarTrunc),
  )
import RVV.Synthesizer.Operator.SetVectorLength
  ( SketchSetVectorLength
      ( SketchSetMaxVectorLength,
        SketchSetRelayedVectorLength,
        SketchSetVectorLength
      ),
  )
import RVV.Synthesizer.Operator.SingleWidthIntBinary
  ( SketchSingleWidthIntBinary (SketchSingleWidthIntBinary),
  )
import RVV.Synthesizer.Operator.SingleWidthMulAdd
  ( SketchSingleWidthMulAdd (SketchSingleWidthMulAdd),
  )
import RVV.Synthesizer.Operator.Slide (SketchSlide (SketchSlide, SketchSlide1))
import RVV.Synthesizer.Operator.VectorCompare (SketchVectorCompare (SketchVectorCompare))
import RVV.Synthesizer.Operator.VectorIndex (SketchVectorIndex (SketchVectorId, SketchVectorIota))
import RVV.Synthesizer.Operator.Vlenb (SketchVlenb (SketchVlenb))
import RVV.Synthesizer.Operator.WideningIntBinary
  ( SketchWideningIntBinary (SketchWideningIntBinary),
  )
import RVV.Synthesizer.Operator.WideningMulAdd
  ( SketchWideningMulAdd (SketchWideningMulAdd),
  )
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest, UseUndefinedDest),
  )
import RVV.Synthesizer.Parameter.FixedPointRoundingMode
  ( FixedPointRoundingMode (FixedRDN, FixedRNE, FixedRNU, FixedROD),
  )
import RVV.Synthesizer.Parameter.IntCompareOpCode
  ( IntCompareOpCode
      ( MSEq,
        MSGe,
        MSGeu,
        MSGt,
        MSGtu,
        MSLe,
        MSLeu,
        MSLt,
        MSLtu,
        MSNe
      ),
  )
import RVV.Synthesizer.Parameter.MaskLogicalOpCode
  ( MaskLogicalOpCode (MAnd, MAndn, MNand, MNor, MOr, MOrn, MXnor, MXor),
  )
import RVV.Synthesizer.Parameter.Masking
  ( Masking (UseFullMask, UseProvidedMask),
  )
import RVV.Synthesizer.Parameter.NarrowingRightShiftOpCode
  ( NarrowingRightShiftOpCode (NSra, NSrl),
  )
import RVV.Synthesizer.Parameter.SetMaskMethod
  ( SetMaskMethod (BeforeFirst, IncludingFirst, OnlyFirst),
  )
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode
      ( Add,
        And,
        Max,
        Maxu,
        Min,
        Minu,
        Mul,
        Mulh,
        Mulhsu,
        Mulhu,
        Or,
        RSub,
        SAdd,
        SAddu,
        SSub,
        SSubu,
        Sll,
        Sra,
        Srl,
        Sub,
        Xor
      ),
  )
import RVV.Synthesizer.Parameter.SingleWidthMulAddOpCode
  ( SingleWidthMulAddOpCode (MAcc, MAdd, NMSac, NMSub),
  )
import RVV.Synthesizer.Parameter.SlideDirection
  ( SlideDirection (SlideDown, SlideUp),
  )
import RVV.Synthesizer.Parameter.WideningIntBinaryOpCode
  ( WideningIntBinaryOpCode (WAdd, WAddu, WMul, WMulsu, WMulu, WSub, WSubu),
  )
import RVV.Synthesizer.Parameter.WideningMulAddOpCode
  ( WideningMulAddOpCode (WMAcc, WMAccsu, WMAccu, WMAccus),
  )
import RVV.Synthesizer.Type (ValueType)

destChoices :: FeatureSet -> [Destination]
destChoices set =
  if (HS.member PartialVL (opFeatures set) || HS.member Masking (opFeatures set))
    && policyFeatures set /= HS.singleton nonePolicy
    then [UseProvidedDest, UseUndefinedDest]
    else [UseUndefinedDest]

maskChoices :: FeatureSet -> [Masking]
maskChoices set =
  if HS.member Masking (opFeatures set)
    then [UseProvidedMask, UseFullMask]
    else
      [UseFullMask]

maySigned :: FeatureSet -> Bool
maySigned set = signednessFeatures set /= HS.singleton Unsigned

mayUnsigned :: FeatureSet -> Bool
mayUnsigned set = signednessFeatures set /= HS.singleton Signed

maskElementWiseMMOps :: FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
maskElementWiseMMOps set =
  if Masking `HS.member` opFeatures set
    then HS.fromList [Add]
    else HS.empty

maskElementWiseMXOps :: FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
maskElementWiseMXOps set =
  if Masking `HS.member` opFeatures set
    then HS.fromList [Sll, Srl]
    else HS.empty

maskElementWiseMILogicalOps :: FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
maskElementWiseMILogicalOps set =
  if Masking `HS.member` opFeatures set
    then HS.fromList [And, Or, Xor]
    else HS.empty

maskElementWiseMIShiftOps :: FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
maskElementWiseMIShiftOps set =
  if Masking `HS.member` opFeatures set
    then HS.fromList [Sll, Srl]
    else HS.empty

maskElementWiseMIMulOps :: FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
maskElementWiseMIMulOps set =
  if Masking `HS.member` opFeatures set
    then HS.fromList [Mul]
    else HS.empty

compareVVOps :: FeatureSet -> HS.HashSet IntCompareOpCode
compareVVOps set =
  if Compare `HS.member` opFeatures set
    then
      HS.fromList $
        concat $
          concat
            [ [[MSEq, MSNe]],
              [[MSLt, MSLe] | maySigned set],
              [[MSLtu, MSLeu] | mayUnsigned set]
            ]
    else HS.empty

compareOps :: FeatureSet -> HS.HashSet IntCompareOpCode
compareOps set =
  if Compare `HS.member` opFeatures set
    then
      HS.fromList $
        concat $
          concat
            [ [[MSEq, MSNe]],
              [[MSLt, MSLe, MSGt, MSGe] | maySigned set],
              [[MSLtu, MSLeu, MSGtu, MSGeu] | mayUnsigned set]
            ]
    else HS.empty

compareOpVVGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchVectorCompare]
compareOpVVGroup set =
  let vvOps = compareVVOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                [ SketchVectorCompare
                    config
                    (HS.toList vvOps)
                    (destChoices set)
                    (maskChoices set)
                    SketchVectorRHS
                  | not (null vvOps)
                ]
              )
          )
        $ HS.toList
        $ vectorConfigFeatures set

compareOpVXVIGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchVectorCompare]
compareOpVXVIGroup set =
  let ops = compareOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                concat
                  [ [ SketchVectorCompare
                        config
                        (HS.toList ops)
                        (destChoices set)
                        (maskChoices set)
                        SketchScalarRHS,
                      SketchVectorCompare
                        config
                        (HS.toList ops)
                        (destChoices set)
                        (maskChoices set)
                        (SketchImmRHS ArbitraryImmSpec)
                    ]
                    | not (null ops)
                  ]
              )
          )
        $ HS.toList
        $ vectorConfigFeatures set

singleWidthMulValidOps :: FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
singleWidthMulValidOps set =
  mconcat
    [ if Multiplication `HS.member` opFeatures set
        then HS.fromList [Mul]
        else HS.empty,
      if MultiplicationHigh `HS.member` opFeatures set
        then
          HS.fromList $
            concat
              [ [Mulh | maySigned set],
                [Mulhsu | maySigned set && mayUnsigned set],
                [Mulhu | mayUnsigned set]
              ]
        else HS.empty
    ]

singleWidthCmpValidOps :: FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
singleWidthCmpValidOps set =
  mconcat
    [ if Compare `HS.member` opFeatures set
        then
          HS.fromList $
            concat $
              [[Min, Max] | maySigned set]
                ++ [[Minu, Maxu] | mayUnsigned set]
        else HS.empty
    ]

singleWidthArithValidOps :: Bool -> FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
singleWidthArithValidOps isVXVI set =
  mconcat
    [ if LinearArith `HS.member` opFeatures set
        then HS.fromList ([Add, Sub] ++ ([RSub | isVXVI]))
        else HS.empty,
      if SaturateLinearArith `HS.member` opFeatures set
        then
          HS.fromList $
            concat $
              [[SSub, SAdd] | maySigned set]
                ++ [[SSubu, SAddu] | mayUnsigned set]
        else HS.empty
    ]

singleWidthLogicalValidOps :: FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
singleWidthLogicalValidOps set =
  mconcat
    [ if LinearArith `HS.member` opFeatures set
        then HS.fromList [Sll, Srl]
        else HS.empty,
      if LinearArith `HS.member` opFeatures set && maySigned set
        then HS.fromList [Sra]
        else HS.empty,
      if Logical `HS.member` opFeatures set
        then HS.fromList [And, Or, Xor]
        else HS.empty
    ]

singleWidthNonMulValidOps :: FeatureSet -> HS.HashSet SingleWidthIntBinaryOpCode
singleWidthNonMulValidOps set =
  mconcat
    [ if LinearArith `HS.member` opFeatures set
        then HS.fromList [Add, Sub, Sll, Srl]
        else HS.empty,
      if LinearArith `HS.member` opFeatures set && maySigned set
        then HS.fromList [Sra]
        else HS.empty,
      if Logical `HS.member` opFeatures set
        then HS.fromList [And, Or, Xor]
        else HS.empty,
      if Compare `HS.member` opFeatures set
        then
          HS.fromList $
            concat $
              [[Min, Max] | maySigned set]
                ++ [[Minu, Maxu] | mayUnsigned set]
        else HS.empty,
      if SaturateLinearArith `HS.member` opFeatures set
        then
          HS.fromList $
            concat $
              [[SSub, SAdd] | maySigned set]
                ++ [[SSubu, SAddu] | mayUnsigned set]
        else HS.empty
    ]

wideningValidLinearOps :: FeatureSet -> HS.HashSet WideningIntBinaryOpCode
wideningValidLinearOps set =
  if LinearArith `HS.member` opFeatures set
    then
      (if maySigned set then HS.fromList [WAdd, WSub] else HS.empty)
        <> (if mayUnsigned set then HS.fromList [WAddu, WSubu] else HS.empty)
    else HS.empty

wideningValidMulOps :: FeatureSet -> HS.HashSet WideningIntBinaryOpCode
wideningValidMulOps set =
  if Multiplication `HS.member` opFeatures set
    || MultiplicationHigh `HS.member` opFeatures set
    then
      (if maySigned set then HS.fromList [WMul] else HS.empty)
        <> (if mayUnsigned set then HS.fromList [WMulu] else HS.empty)
        <> ( if maySigned set && mayUnsigned set
               then HS.fromList [WMulsu]
               else HS.empty
           )
    else HS.empty

singleWidthFMAValidOps :: FeatureSet -> HS.HashSet SingleWidthMulAddOpCode
singleWidthFMAValidOps set =
  mconcat
    [ if Multiplication `HS.member` opFeatures set
        then HS.fromList [MAcc, NMSac, MAdd, NMSub]
        else HS.empty
    ]

wideningFMAValidOps :: FeatureSet -> HS.HashSet WideningMulAddOpCode
wideningFMAValidOps set =
  mconcat
    [ if Multiplication `HS.member` opFeatures set
        || MultiplicationHigh `HS.member` opFeatures set
        then
          (if maySigned set then HS.fromList [WMAcc] else HS.empty)
            <> (if mayUnsigned set then HS.fromList [WMAccu] else HS.empty)
            <> ( if maySigned set && mayUnsigned set
                   then HS.fromList [WMAccsu, WMAccus]
                   else HS.empty
               )
        else HS.empty
    ]

narrowingRightShiftOps :: FeatureSet -> HS.HashSet NarrowingRightShiftOpCode
narrowingRightShiftOps set
  | Narrowing `HS.member` opFeatures set =
      (if maySigned set then HS.fromList [NSra] else HS.empty)
        <> (if mayUnsigned set then HS.fromList [NSrl] else HS.empty)
  | otherwise = HS.empty

fixedPointClipRightShiftSignedChoices :: FeatureSet -> Maybe Bool
fixedPointClipRightShiftSignedChoices set
  | maySigned set && mayUnsigned set = Nothing
  | maySigned set = Just True
  | mayUnsigned set = Just False
  | otherwise = Nothing

hasVMerge :: FeatureSet -> Bool
hasVMerge set = Masking `HS.member` opFeatures set

hasVSlide :: FeatureSet -> Bool
hasVSlide set = Slide `HS.member` opFeatures set

hasVSlide1 :: FeatureSet -> Bool
hasVSlide1 set =
  Slide1 `HS.member` opFeatures set
    || ( SetMask `HS.member` opFeatures set
           && Masking `HS.member` opFeatures set
           && Slide `HS.member` opFeatures set
       )

hasVId :: FeatureSet -> Bool
hasVId set = Masking `HS.member` opFeatures set

scalarTruncGroup :: FeatureSet -> HM.HashMap WidthMul [SketchScalarTrunc]
scalarTruncGroup set =
  let xmuls = widthMulFeatures set
      xmulPairs =
        filter (uncurry (/=)) $ [(xmul, xmul) | xmul <- HS.toList xmuls]
   in HM.fromList
        [ (dst, [SketchScalarTrunc (max src dst) (min src dst)])
          | (src, dst) <- xmulPairs
        ]

vlenbGroup :: FeatureSet -> HM.HashMap WidthMul [SketchVlenb]
vlenbGroup set =
  let xmuls = widthMulFeatures set
   in HM.fromList
        [ (xmul, [SketchVlenb | GetVlenb `HS.member` opFeatures set])
          | xmul <- HS.toList xmuls
        ]

simpleScalarOpGroup :: FeatureSet -> HM.HashMap WidthMul [SketchScalar]
simpleScalarOpGroup set =
  let xmuls = widthMulFeatures set
      scalarLongs =
        HM.fromList [(xmul, [SketchScalarLongImm xmul ArbitraryImmSpec]) | xmul <- HS.toList xmuls]
      unionsWith = foldl1 (HM.unionWith (<>))
   in unionsWith [scalarLongs]

constantMaskOpGroup :: WidthMul -> FeatureSet -> HM.HashMap MaskMul [SketchMove]
constantMaskOpGroup delegatedXMul set =
  let maskMuls = maskMulFeatures set
   in HM.fromList
        [ (mmul, [SketchMoveToMask delegatedXMul mmul (SketchImmRHS ArbitraryImmSpec)])
          | Masking `HS.member` opFeatures set,
            mmul <- HS.toList maskMuls
        ]

featurePolicy :: FeatureSet -> Policy 'C
featurePolicy set = mconcat $ HS.toList $ policyFeatures set

vsetvlGroup :: FeatureSet -> HM.HashMap MaskMul [SketchSetVectorLength]
vsetvlGroup set =
  HM.fromList
    $ fmap
      ( \mmul ->
          ( mmul,
            concat
              [ [ SketchSetVectorLength mmul [toSym $ featurePolicy set]
                  | PartialVL `HS.member` opFeatures set
                ],
                [ SketchSetMaxVectorLength mmul [toSym $ featurePolicy set]
                  | FullVL `HS.member` opFeatures set
                ],
                [ SketchSetRelayedVectorLength mmul mmul1 [toSym $ featurePolicy set]
                  | PartialVL `HS.member` opFeatures set,
                    mmul1 <- HS.toList $ maskMulFeatures set,
                    mmul /= mmul1
                ]
              ]
          )
      )
    $ HS.toList
    $ maskMulFeatures set

scalarSingleWidthNonMulGroup ::
  FeatureSet -> HM.HashMap WidthMul [SketchScalarOperator]
scalarSingleWidthNonMulGroup set =
  let nonMulOps =
        singleWidthNonMulValidOps set
          <> HS.fromList [Add, Sub, Sll, Srl]
      scalarSingleWidthOps =
        HM.fromList
          [ ( xmul,
              [ SketchScalarBin (HS.toList nonMulOps) xmul SketchScalarRHS,
                SketchScalarBin (HS.toList nonMulOps) xmul (SketchImmRHS ArbitraryImmSpec)
              ]
            )
            | xmul <- HS.toList $ widthMulFeatures set
          ]
   in scalarSingleWidthOps

scalarSingleWidthNonMulVLGroup :: FeatureSet -> HM.HashMap MaskMul [SketchScalarOperator]
scalarSingleWidthNonMulVLGroup set =
  let nonMulOps =
        singleWidthNonMulValidOps set
          <> HS.fromList [Add, Sub, Sll, Srl]
      scalarSingleWidthOps =
        HM.fromList
          [ ( mmul,
              [ SketchScalarBinVectorLength (HS.toList nonMulOps) mmul SketchScalarRHS,
                SketchScalarBinVectorLength (HS.toList nonMulOps) mmul (SketchImmRHS ArbitraryImmSpec)
              ]
            )
            | mmul <- HS.toList $ maskMulFeatures set
          ]
   in scalarSingleWidthOps

scalarSingleWidthMulGroup :: FeatureSet -> HM.HashMap WidthMul [SketchScalarOperator]
scalarSingleWidthMulGroup set =
  let mulOps = singleWidthMulValidOps set
      scalarSingleWidthOps =
        HM.fromList
          [ ( xmul,
              [ SketchScalarBin (HS.toList mulOps) xmul SketchScalarRHS,
                SketchScalarBin (HS.toList mulOps) xmul (SketchImmRHS ArbitraryImmSpec)
              ]
            )
            | mulOps /= HS.empty,
              xmul <- HS.toList $ widthMulFeatures set
          ]
   in scalarSingleWidthOps

scalarSingleWidthMulVLGroup :: FeatureSet -> HM.HashMap MaskMul [SketchScalarOperator]
scalarSingleWidthMulVLGroup set =
  let mulOps = singleWidthMulValidOps set
      scalarSingleWidthOps =
        HM.fromList
          [ ( mmul,
              [ SketchScalarBinVectorLength (HS.toList mulOps) mmul SketchScalarRHS,
                SketchScalarBinVectorLength (HS.toList mulOps) mmul (SketchImmRHS ArbitraryImmSpec)
              ]
            )
            | mulOps /= HS.empty,
              mmul <- HS.toList $ maskMulFeatures set
          ]
   in scalarSingleWidthOps

singleWidthOpCmpGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchSingleWidthIntBinary]
singleWidthOpCmpGroup set =
  let ops = singleWidthCmpValidOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                concat
                  [ [SketchSingleWidthIntBinary config (HS.toList ops) (destChoices set) (maskChoices set) SketchVectorRHS | not (null ops)],
                    [SketchSingleWidthIntBinary config (HS.toList ops) (destChoices set) (maskChoices set) SketchScalarRHS | not (null ops)],
                    [SketchSingleWidthIntBinary config (HS.toList ops) (destChoices set) (maskChoices set) (SketchImmRHS ArbitraryImmSpec) | not (null ops)]
                  ]
              )
          )
        $ HS.toList
        $ vectorConfigFeatures set

singleWidthOpArithGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchSingleWidthIntBinary]
singleWidthOpArithGroup set =
  let nonMulOps = singleWidthArithValidOps False set
      vxviOps = singleWidthArithValidOps True set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                concat
                  [ [SketchSingleWidthIntBinary config (HS.toList nonMulOps) (destChoices set) (maskChoices set) SketchVectorRHS | not (null nonMulOps)],
                    [SketchSingleWidthIntBinary config (HS.toList vxviOps) (destChoices set) (maskChoices set) SketchScalarRHS | not (null vxviOps)],
                    [SketchSingleWidthIntBinary config (HS.toList vxviOps) (destChoices set) (maskChoices set) (SketchImmRHS ArbitraryImmSpec) | not (null vxviOps)]
                  ]
              )
          )
        $ HS.toList
        $ vectorConfigFeatures set

singleWidthOpLogicalGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchSingleWidthIntBinary]
singleWidthOpLogicalGroup set =
  let nonMulOps = singleWidthLogicalValidOps set
      vxviOps = singleWidthLogicalValidOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                concat
                  [ [SketchSingleWidthIntBinary config (HS.toList nonMulOps) (destChoices set) (maskChoices set) SketchVectorRHS | not (null nonMulOps)],
                    [SketchSingleWidthIntBinary config (HS.toList vxviOps) (destChoices set) (maskChoices set) SketchScalarRHS | not (null vxviOps)],
                    [SketchSingleWidthIntBinary config (HS.toList vxviOps) (destChoices set) (maskChoices set) (SketchImmRHS ArbitraryImmSpec) | not (null vxviOps)]
                  ]
              )
          )
        $ HS.toList
        $ vectorConfigFeatures set

singleWidthOpMulGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchSingleWidthIntBinary]
singleWidthOpMulGroup set =
  let mulOps = singleWidthMulValidOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                concat
                  [ [SketchSingleWidthIntBinary config (HS.toList mulOps) (destChoices set) (maskChoices set) SketchVectorRHS | not (null mulOps)],
                    [SketchSingleWidthIntBinary config (HS.toList mulOps) (destChoices set) (maskChoices set) SketchScalarRHS | not (null mulOps)],
                    [SketchSingleWidthIntBinary config (HS.toList mulOps) (destChoices set) (maskChoices set) (SketchImmRHS ArbitraryImmSpec) | not (null mulOps)]
                  ]
              )
          )
        $ HS.toList
        $ vectorConfigFeatures set

singleWidthOpFMAGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchSingleWidthMulAdd]
singleWidthOpFMAGroup set =
  let mulOps = singleWidthFMAValidOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                concat
                  [ [SketchSingleWidthMulAdd config (HS.toList mulOps) (maskChoices set) SketchVectorRHS | not (null mulOps)],
                    [SketchSingleWidthMulAdd config (HS.toList mulOps) (maskChoices set) SketchScalarRHS | not (null mulOps)],
                    [SketchSingleWidthMulAdd config (HS.toList mulOps) (maskChoices set) (SketchImmRHS ArbitraryImmSpec) | not (null mulOps)]
                  ]
              )
          )
        $ HS.toList
        $ vectorConfigFeatures set

wideningLinearGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchWideningIntBinary]
wideningLinearGroup set =
  let ops = wideningValidLinearOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                concat
                  [ [ SketchWideningIntBinary
                        config
                        (HS.toList ops)
                        (destChoices set)
                        (maskChoices set)
                        [False, True]
                        SketchVectorRHS,
                      SketchWideningIntBinary
                        config
                        (HS.toList ops)
                        (destChoices set)
                        (maskChoices set)
                        [False, True]
                        SketchScalarRHS,
                      SketchWideningIntBinary
                        config
                        (HS.toList ops)
                        (destChoices set)
                        (maskChoices set)
                        [False, True]
                        (SketchImmRHS ArbitraryImmSpec)
                    ]
                    | not (null ops)
                  ]
              )
          )
        $ HS.toList
        $ wideVectorConfigFeatures set

wideningMulGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchWideningIntBinary]
wideningMulGroup set =
  let ops = wideningValidMulOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                concat
                  [ [ SketchWideningIntBinary
                        config
                        (HS.toList ops)
                        (destChoices set)
                        (maskChoices set)
                        [False, True]
                        SketchVectorRHS,
                      SketchWideningIntBinary
                        config
                        (HS.toList ops)
                        (destChoices set)
                        (maskChoices set)
                        [False, True]
                        SketchScalarRHS,
                      SketchWideningIntBinary
                        config
                        (HS.toList ops)
                        (destChoices set)
                        (maskChoices set)
                        [False, True]
                        (SketchImmRHS ArbitraryImmSpec)
                    ]
                    | not (null ops)
                  ]
              )
          )
        $ HS.toList
        $ wideVectorConfigFeatures set

wideningFMAGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchWideningMulAdd]
wideningFMAGroup set =
  let ops = wideningFMAValidOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                concat
                  [ [SketchWideningMulAdd config (HS.toList ops) (maskChoices set) SketchVectorRHS | not (null ops)],
                    [SketchWideningMulAdd config (HS.toList ops) (maskChoices set) SketchScalarRHS | not (null ops)],
                    [SketchWideningMulAdd config (HS.toList ops) (maskChoices set) (SketchImmRHS ArbitraryImmSpec) | not (null ops)]
                  ]
              )
          )
        $ HS.toList
        $ wideVectorConfigFeatures set

roundingModeFromFeature :: FixedPointRoundingModeFeature -> FixedPointRoundingMode
roundingModeFromFeature FixedRNUFeature = FixedRNU
roundingModeFromFeature FixedRNEFeature = FixedRNE
roundingModeFromFeature FixedRDNFeature = FixedRDN
roundingModeFromFeature FixedRODFeature = FixedROD

roundingModesFromFeatures :: FeatureSet -> [FixedPointRoundingMode]
roundingModesFromFeatures set =
  map roundingModeFromFeature $ HS.toList $ roundingModeFeatures set

narrowingGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchNarrowingRightShift]
narrowingGroup set =
  let nrOps = narrowingRightShiftOps set
   in HM.fromList
        $ fmap
          ( \config ->
              ( config,
                [SketchNarrowingRightShift config (HS.toList nrOps) (destChoices set) (maskChoices set) SketchScalarRHS | not (null nrOps)]
                  ++ [SketchNarrowingRightShift config (HS.toList nrOps) (destChoices set) (maskChoices set) (SketchImmRHS ArbitraryImmSpec) | not (null nrOps)]
              )
          )
        $ HS.toList
        $ narrowVectorConfigFeatures set

clippingGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchFixedPointClip]
clippingGroup set =
  HM.fromList
    $ fmap
      ( \config ->
          ( config,
            concat
              [ [ SketchFixedPointClip
                    config
                    (toList $ fixedPointClipRightShiftSignedChoices set)
                    (roundingModesFromFeatures set)
                    (destChoices set)
                    (maskChoices set)
                    SketchVectorRHS
                  | FixedPointClip `HS.member` opFeatures set
                ],
                [ SketchFixedPointClip
                    config
                    (toList $ fixedPointClipRightShiftSignedChoices set)
                    (roundingModesFromFeatures set)
                    (destChoices set)
                    (maskChoices set)
                    SketchScalarRHS
                  | FixedPointClip `HS.member` opFeatures set
                ],
                [ SketchFixedPointClip
                    config
                    (toList $ fixedPointClipRightShiftSignedChoices set)
                    (roundingModesFromFeatures set)
                    (destChoices set)
                    (maskChoices set)
                    (SketchImmRHS ArbitraryImmSpec)
                  | FixedPointClip `HS.member` opFeatures set
                ]
              ]
          )
      )
    $ HS.toList
    $ narrowVectorConfigFeatures set

maskSetMaskGroup :: Int -> FeatureSet -> HM.HashMap MaskMul (ChoiceTree SketchMiscMask)
maskSetMaskGroup base set =
  if SetMask `HS.member` opFeatures set
    then
      HM.fromList
        $ zipWith
          ( \n mmul ->
              ( mmul,
                Branch
                  (Split (base + n) False)
                  [ Leaf
                      [ SketchMaskSetBit
                          [BeforeFirst]
                          mmul
                          (destChoices set)
                          (maskChoices set)
                      ],
                    Leaf
                      [ SketchMaskSetBit
                          [IncludingFirst]
                          mmul
                          (destChoices set)
                          (maskChoices set)
                      ],
                    Leaf
                      [ SketchMaskSetBit
                          [OnlyFirst]
                          mmul
                          (destChoices set)
                          (maskChoices set)
                      ]
                  ]
              )
          )
          [0 ..]
        $ HS.toList
        $ maskMulFeatures set
    else HM.empty

maskElementWiseGroup1 :: Int -> WidthMul -> FeatureSet -> HM.HashMap MaskMul (ChoiceTree SketchDelegatedVectorBinaryOnMask)
maskElementWiseGroup1 base delegatedXMul set =
  let miShiftOps = maskElementWiseMIShiftOps set
      miMulOps = maskElementWiseMIMulOps set
   in if Masking `HS.member` opFeatures set
        then
          HM.fromList
            $ zipWith
              ( \n mmul ->
                  ( mmul,
                    Branch
                      (Split (base + n) False)
                      [ Leaf
                          [ SketchDelegatedVectorBinaryOnMask
                              delegatedXMul
                              mmul
                              (HS.toList miShiftOps)
                              (SketchImmRHS (BoundedImmSpec False 0 3))
                            | not (null miShiftOps)
                          ],
                        Leaf
                          [ SketchDelegatedVectorBinaryOnMask
                              delegatedXMul
                              mmul
                              (HS.toList miMulOps)
                              (SketchImmRHS (ConstImmSpec 3))
                            | not (null miMulOps)
                          ]
                      ]
                  )
              )
              [0 ..]
            $ HS.toList
            $ maskMulFeatures set
        else HM.empty

maskElementWiseGroup2 :: Int -> WidthMul -> FeatureSet -> HM.HashMap MaskMul (ChoiceTree SketchDelegatedVectorBinaryOnMask)
maskElementWiseGroup2 base delegatedXMul set =
  let mmOps = maskElementWiseMMOps set
      mxOps = maskElementWiseMXOps set
      miLogicalOps = maskElementWiseMILogicalOps set
   in if Masking `HS.member` opFeatures set
        then
          HM.fromList
            $ zipWith
              ( \n mmul ->
                  ( mmul,
                    Branch
                      (Split (base + n) False)
                      [ Leaf [SketchDelegatedVectorBinaryOnMask delegatedXMul mmul (HS.toList mmOps) SketchVectorRHS | not (null mmOps)],
                        Leaf [SketchDelegatedVectorBinaryOnMask delegatedXMul mmul (HS.toList mxOps) SketchScalarRHS | not (null mxOps)],
                        Leaf
                          [ SketchDelegatedVectorBinaryOnMask
                              delegatedXMul
                              mmul
                              (HS.toList miLogicalOps)
                              (SketchImmRHS ArbitraryImmSpec)
                            | not (null miLogicalOps)
                          ]
                      ]
                  )
              )
              [0 ..]
            $ HS.toList
            $ maskMulFeatures set
        else HM.empty

maskLogicalGroup :: Int -> FeatureSet -> HM.HashMap MaskMul (ChoiceTree SketchMaskLogical)
maskLogicalGroup base set =
  if Masking `HS.member` opFeatures set
    then
      HM.fromList
        $ zipWith
          ( \n mmul ->
              ( mmul,
                Branch
                  (Split (base + n) False)
                  [ Leaf
                      [ SketchMaskLogical
                          mmul
                          [MAnd, MNand, MAndn, MXor]
                      ],
                    Leaf
                      [ SketchMaskLogical
                          mmul
                          [MOr, MNor, MOrn, MXnor]
                      ]
                  ]
              )
          )
          [0 ..]
        $ HS.toList
        $ maskMulFeatures set
    else HM.empty

broadcastOpGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchMove]
broadcastOpGroup set =
  if Broadcast `HS.member` opFeatures set
    then
      HM.fromList
        $ fmap
          ( \config ->
              (config, [SketchMoveToVector config (destChoices set) SketchScalarRHS])
          )
        $ HS.toList
        $ vectorConfigFeatures set
    else HM.empty

vmergeOpGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchMerge]
vmergeOpGroup set =
  if hasVMerge set
    then
      HM.fromList
        $ fmap
          ( \config ->
              ( config,
                [ SketchMerge config (destChoices set) SketchVectorRHS,
                  SketchMerge config (destChoices set) SketchScalarRHS,
                  SketchMerge config (destChoices set) (SketchImmRHS ArbitraryImmSpec)
                ]
              )
          )
        $ HS.toList
        $ vectorConfigFeatures set
    else HM.empty

idOpGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchVectorIndex]
idOpGroup set
  | hasVId set =
      HM.fromList
        $ fmap
          ( \config ->
              ( config,
                [ SketchVectorId config (destChoices set) (maskChoices set),
                  SketchVectorIota config (destChoices set) (maskChoices set)
                ]
              )
          )
        $ HS.toList
        $ vectorConfigFeatures set
  | otherwise = HM.empty

slideOpGroup :: Int -> FeatureSet -> HM.HashMap VectorConfig (ChoiceTree SketchSlide)
slideOpGroup base set
  | hasVSlide set || hasVSlide1 set =
      HM.fromList
        $ zipWith
          ( \n config ->
              ( config,
                Branch
                  (Split (base + n * 10) False)
                  [ Branch (Split (base + n * 10 + 1) False) $
                      if hasVSlide set
                        then
                          [ Leaf
                              [ SketchSlide
                                  config
                                  [SlideUp, SlideDown]
                                  [UseUndefinedDest, UseProvidedDest]
                                  (maskChoices set)
                                  SketchScalarRHS
                              ],
                            Leaf
                              [ SketchSlide
                                  config
                                  [SlideUp, SlideDown]
                                  [UseUndefinedDest, UseProvidedDest]
                                  (maskChoices set)
                                  (SketchImmRHS ArbitraryImmSpec)
                              ]
                          ]
                        else [],
                    Branch (Split (base + n * 10 + 2) False) $
                      if hasVSlide1 set
                        then
                          [ Leaf
                              [ SketchSlide1
                                  config
                                  [SlideUp, SlideDown]
                                  [UseUndefinedDest, UseProvidedDest]
                                  (maskChoices set)
                                  SketchScalarRHS
                              ],
                            Leaf
                              [ SketchSlide1
                                  config
                                  [SlideUp, SlideDown]
                                  [UseUndefinedDest, UseProvidedDest]
                                  (maskChoices set)
                                  (SketchImmRHS ArbitraryImmSpec)
                              ]
                          ]
                        else []
                  ]
              )
          )
          [0 ..]
        $ HS.toList
        $ vectorConfigFeatures set
  | otherwise = HM.empty

isEmptyChoiceTree :: ChoiceTree op -> Bool
isEmptyChoiceTree (Leaf []) = True
isEmptyChoiceTree (Branch _ []) = True
isEmptyChoiceTree (Branch _ l) = all isEmptyChoiceTree l
isEmptyChoiceTree _ = False

simplifyChoiceTree :: ChoiceTree op -> ChoiceTree op
simplifyChoiceTree (Branch _ [l]) = simplifyChoiceTree l
simplifyChoiceTree (Branch meta l) =
  let filtered = filter (not . isEmptyChoiceTree) $ simplifyChoiceTree <$> l
   in if length filtered == 1
        then head filtered
        else Branch meta filtered
simplifyChoiceTree (Leaf l) = Leaf l

getSetVTypes :: FeatureSet -> HM.HashMap VectorConfig (HS.HashSet (VectorConfig, Int))
getSetVTypes set =
  HM.fromListWith
    (<>)
    ( [ (halfVectorConfig config, HS.singleton (config, 2))
        | HS.member Slide (opFeatures set) && HS.member VLToScalarFeature (opFeatures set),
          config <- HS.toList $ vectorConfigFeatures set
      ]
        ++ [ ( config0,
               HS.singleton
                 ( config1,
                   numerator $
                     getLengthMul $
                       lengthMul config1 / lengthMul config0
                 )
             )
             | HS.member GetSet (opFeatures set),
               config0 <- HS.toList $ vectorConfigFeatures set,
               config1 <- HS.toList $ vectorConfigFeatures set,
               lengthMul config0 < lengthMul config1
           ]
    )

vectorToVectorVTypes :: FeatureSet -> HM.HashMap VectorConfig (HS.HashSet VectorConfig)
vectorToVectorVTypes set =
  HM.fromListWith
    (<>)
    [ (config0, HS.singleton config1)
      | HS.member GetSet (opFeatures set),
        config0 <- HS.toList $ vectorConfigFeatures set,
        config1 <- HS.toList $ vectorConfigFeatures set,
        config0 /= config1,
        lengthMul config0 == lengthMul config1
    ]

insertOpGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchInsert]
insertOpGroup set =
  HM.mapWithKey
    ( \partConfig s ->
        ( \(fullConfig, n) ->
            [ SketchInsertVector
                partConfig
                fullConfig
                [UseProvidedDest, UseUndefinedDest]
                [0 .. n - 1]
            ]
        )
          =<< HS.toList s
    )
    vtype
  where
    vtype = getSetVTypes set

extractOpGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchExtract]
extractOpGroup set =
  HM.mapWithKey
    ( \partConfig s ->
        ( \(fullConfig, n) ->
            [SketchExtractVector partConfig fullConfig [0 .. n - 1]]
        )
          =<< HS.toList s
    )
    vtype
  where
    vtype = getSetVTypes set

vectorToVectorGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchReinterpret]
vectorToVectorGroup set =
  HM.mapWithKey
    ( \destVType s ->
        (\srcVType -> [SketchVectorToVector srcVType destVType]) =<< HS.toList s
    )
    vtype
  where
    vtype = vectorToVectorVTypes set

constantVectorOpGroup :: FeatureSet -> HM.HashMap VectorConfig [SketchMove]
constantVectorOpGroup set =
  let vtypes = vectorConfigFeatures set
   in HM.fromList
        [ (vtype, [SketchMoveToVector vtype (destChoices set) (SketchImmRHS ArbitraryImmSpec)])
          | Masking `HS.member` opFeatures set,
            vtype <- HS.toList vtypes
        ]

miscVectorChoiceTree :: FeatureSet -> SketchChoiceTree
miscVectorChoiceTree set =
  simplifyChoiceTree $
    Branch (Split 310 False) (zipWith opListByVType vtypes [0 ..])
  where
    slideOps = slideOpGroup 1400 set
    idOps = idOpGroup set
    vmergeOps = vmergeOpGroup set
    broadcastOps = broadcastOpGroup set
    extractOps = extractOpGroup set
    insertOps = insertOpGroup set
    vectorToVectorOps = vectorToVectorGroup set
    constantVectorOps = constantVectorOpGroup set
    vtypes =
      HS.toList $
        HM.keysSet slideOps
          <> HM.keysSet idOps
          <> HM.keysSet vmergeOps
          <> HM.keysSet broadcastOps
          <> HM.keysSet extractOps
          <> HM.keysSet insertOps
          <> HM.keysSet vectorToVectorOps
          <> HM.keysSet constantVectorOps
    opListByVType vtype n =
      Branch
        (Split (311 + n * 3) False)
        [ Branch
            (Split (311 + n * 3 + 1) False)
            $ (fmap inj <$> maybeToList (HM.lookup vtype slideOps))
              ++ [ Leaf (fmap inj . join $ maybeToList (HM.lookup vtype idOps)),
                   Leaf (fmap inj . join $ maybeToList (HM.lookup vtype vmergeOps)),
                   Leaf (fmap inj . join $ maybeToList (HM.lookup vtype broadcastOps))
                 ],
          Branch
            (Split (311 + n * 3 + 2) False)
            [ Branch
                NoSplit
                [ Leaf (fmap inj . join $ maybeToList (HM.lookup vtype extractOps)),
                  Leaf (fmap inj . join $ maybeToList (HM.lookup vtype insertOps))
                ],
              Leaf (fmap inj . join $ maybeToList (HM.lookup vtype vectorToVectorOps)),
              Leaf (fmap inj . join $ maybeToList (HM.lookup vtype constantVectorOps))
            ]
        ]

hasVLToScalar :: FeatureSet -> Bool
hasVLToScalar set = HS.member VLToScalarFeature (opFeatures set)

hasPartialVL :: FeatureSet -> Bool
hasPartialVL set = HS.member PartialVL (opFeatures set)

miscScalarChoiceTree :: FeatureSet -> SketchChoiceTree
miscScalarChoiceTree set =
  simplifyChoiceTree $
    Branch (Split 330 False) (zipWith scalarOpListByXMul xmuls [0 ..])
  where
    scalarTruncOps = scalarTruncGroup set
    vlenbOps = vlenbGroup set
    simpleScalarOps = simpleScalarOpGroup set
    scalarBinNonMulOps = scalarSingleWidthNonMulGroup set
    scalarBinMulOps = scalarSingleWidthMulGroup set
    xmuls =
      HS.toList $
        HM.keysSet simpleScalarOps
          <> HM.keysSet scalarBinNonMulOps
          <> HM.keysSet scalarBinMulOps
    scalarOpListByXMul xmul n =
      Branch
        (Split (331 + n) False)
        [ Branch
            NoSplit
            [ Leaf (fmap inj . join $ maybeToList (HM.lookup xmul simpleScalarOps)),
              Leaf (fmap inj . join $ maybeToList (HM.lookup xmul scalarTruncOps)),
              Leaf (fmap inj . join $ maybeToList (HM.lookup xmul vlenbOps))
            ],
          Leaf (fmap inj . join $ maybeToList (HM.lookup xmul scalarBinNonMulOps)),
          Leaf (fmap inj . join $ maybeToList (HM.lookup xmul scalarBinMulOps))
        ]

miscVLToScalarChoiceTree :: FeatureSet -> SketchChoiceTree
miscVLToScalarChoiceTree set =
  simplifyChoiceTree $
    Branch (Split 340 False) $
      [ Leaf $
          [inj $ SketchVLToScalar mmul | hasVLToScalar set]
            ++ (fmap inj . concat . maybeToList $ HM.lookup mmul scalarBinVLNonMulOps)
            ++ (fmap inj . concat . maybeToList $ HM.lookup mmul scalarBinVLMulOps)
        | mmul <- HS.toList mmuls
      ]
  where
    scalarBinVLNonMulOps = scalarSingleWidthNonMulVLGroup set
    scalarBinVLMulOps = scalarSingleWidthMulVLGroup set
    mmuls =
      maskMulFeatures set
        <> HM.keysSet scalarBinVLMulOps
        <> HM.keysSet scalarBinVLNonMulOps

miscMaskChoiceTree :: WidthMul -> FeatureSet -> SketchChoiceTree
miscMaskChoiceTree delegatedXMul set =
  simplifyChoiceTree $
    Branch (Split 350 False) (zipWith scalarOpListByXMul mmuls [0 ..])
  where
    constantMaskOps = constantMaskOpGroup delegatedXMul set
    mmuls = HS.toList $ HM.keysSet constantMaskOps
    scalarOpListByXMul mmul n =
      Branch
        (Split (351 + n) False)
        [ Leaf (fmap inj . join $ maybeToList (HM.lookup mmul constantMaskOps))
        ]

miscOpChoiceTree :: WidthMul -> FeatureSet -> SketchChoiceTree
miscOpChoiceTree delegatedXMul set =
  simplifyChoiceTree $
    Branch
      (Split 20 False)
      [ Branch
          (Split 21 False)
          [ miscVectorChoiceTree set,
            miscScalarChoiceTree set
          ],
        Branch
          (Split 22 False)
          [ miscVLToScalarChoiceTree set,
            miscMaskChoiceTree delegatedXMul set
          ]
      ]

elementWiseChoiceTree :: FeatureSet -> SketchChoiceTree
elementWiseChoiceTree set =
  simplifyChoiceTree $
    Branch (Split 10 False) (zipWith opListByVType vtypes [0 ..])
  where
    singleWidthCmpOps = singleWidthOpCmpGroup set
    singleWidthArithOps = singleWidthOpArithGroup set
    singleWidthLogicalOps = singleWidthOpLogicalGroup set
    singleWidthMulOps = singleWidthOpMulGroup set
    singleWidthFMAOps = singleWidthOpFMAGroup set
    wideningLinearOps = wideningLinearGroup set
    wideningMulOps = wideningMulGroup set
    wideningFMAOps = wideningFMAGroup set
    narrowingOps = narrowingGroup set
    clippingOps = clippingGroup set
    compareVVOps = compareOpVVGroup set
    compareVXVIOps = compareOpVXVIGroup set
    vtypes =
      HS.toList $
        HM.keysSet singleWidthArithOps
          <> HM.keysSet singleWidthLogicalOps
          <> HM.keysSet singleWidthMulOps
          <> HM.keysSet wideningLinearOps
          <> HM.keysSet wideningMulOps
          <> HM.keysSet wideningFMAOps
          <> HM.keysSet narrowingOps
          <> HM.keysSet clippingOps
          <> HM.keysSet compareVVOps
          <> HM.keysSet compareVXVIOps
    opListByVType vtype n =
      Branch
        (Split (11 + 10 * n) False)
        [ Branch
            (Split (12 + 10 * n) False)
            [ Branch
                (Split (15 + 10 * n) False)
                [ Branch
                    (Split (17 + 10 * n) False)
                    [ Leaf (fmap inj . join $ maybeToList (HM.lookup vtype singleWidthArithOps)),
                      Leaf (fmap inj . join $ maybeToList (HM.lookup vtype singleWidthLogicalOps)),
                      Leaf (fmap inj . join $ maybeToList (HM.lookup vtype singleWidthCmpOps))
                    ],
                  Branch
                    (Split (18 + 10 * n) False)
                    [ Leaf (fmap inj . join $ maybeToList (HM.lookup vtype singleWidthMulOps)),
                      Leaf (fmap inj . join $ maybeToList (HM.lookup vtype singleWidthFMAOps))
                    ]
                ],
              Branch
                (Split (16 + 10 * n) False)
                [ Leaf (fmap inj . join $ maybeToList (HM.lookup vtype wideningLinearOps)),
                  Leaf (fmap inj . join $ maybeToList (HM.lookup vtype wideningMulOps)),
                  Leaf (fmap inj . join $ maybeToList (HM.lookup vtype wideningFMAOps))
                ]
            ],
          Branch
            (Split (13 + 10 * n) False)
            [ Leaf (fmap inj . join $ maybeToList (HM.lookup vtype narrowingOps)),
              Leaf (fmap inj . join $ maybeToList (HM.lookup vtype clippingOps)),
              Branch
                (Split (14 + 10 * n) False)
                [ Leaf (fmap inj . join $ maybeToList (HM.lookup vtype compareVVOps)),
                  Leaf (fmap inj . join $ maybeToList (HM.lookup vtype compareVXVIOps))
                ]
            ]
        ]

maskElementWiseChoiceTree :: WidthMul -> FeatureSet -> SketchChoiceTree
maskElementWiseChoiceTree xmul set =
  simplifyChoiceTree $
    Branch (Split 100 False) (zipWith opListByVType vtypes [0 ..])
  where
    maskElementWiseOps1 = maskElementWiseGroup1 1000 xmul set
    maskElementWiseOps2 = maskElementWiseGroup2 1100 xmul set
    maskLogicalOps = maskLogicalGroup 1200 set
    maskSetMaskOps = maskSetMaskGroup 1300 set
    vtypes =
      HS.toList $
        HM.keysSet maskElementWiseOps1
          <> HM.keysSet maskElementWiseOps2
          <> HM.keysSet maskLogicalOps
          <> HM.keysSet maskSetMaskOps
    opListByVType vtype n =
      Branch
        (Split (101 + 3 * n) False)
        [ Branch
            (Split (102 + 3 * n) False)
            ( fmap inj
                <$> maybeToList (HM.lookup vtype maskElementWiseOps1)
                  ++ maybeToList (HM.lookup vtype maskElementWiseOps2)
            ),
          Branch
            (Split (103 + 3 * n) False)
            ( ( fmap inj
                  <$> maybeToList (HM.lookup vtype maskLogicalOps)
              )
                ++ ( fmap inj
                       <$> maybeToList (HM.lookup vtype maskSetMaskOps)
                   )
            )
        ]

vsetvlChoiceTree :: FeatureSet -> SketchChoiceTree
vsetvlChoiceTree set =
  simplifyChoiceTree $
    Branch (Split 1 False) (opListByXMul <$> xmuls)
  where
    vsetvlOps = vsetvlGroup set
    xmuls = HS.toList $ HM.keysSet vsetvlOps
    opListByXMul xmul =
      Leaf (fmap inj . join $ maybeToList (HM.lookup xmul vsetvlOps))

opListFromFeatures :: FeatureSet -> SketchChoiceTree
opListFromFeatures = opListFromFeatures' (1 / 8)

opListFromFeatures' :: WidthMul -> FeatureSet -> SketchChoiceTree
opListFromFeatures' delegatedXMul set =
  simplifyChoiceTree $
    Branch
      (Split 0 False)
      [ vsetvlChoiceTree set,
        Branch
          (Split 2 False)
          [ Branch
              (Split 3 False)
              [ elementWiseChoiceTree set,
                maskElementWiseChoiceTree delegatedXMul set
              ],
            miscOpChoiceTree delegatedXMul set
          ]
      ]

sketchFromFeatures ::
  WidthMul ->
  Int ->
  FeatureSet ->
  [ValueType] ->
  [ValueType] ->
  CompSketchSpec
sketchFromFeatures delegatedXMul num features argTypes resTypes =
  let opList = opListFromFeatures' delegatedXMul features
   in ComponentBag (toSym argTypes) [(opList, num)] (toSym resTypes)

allowPartialVLFromFeatures :: FeatureSet -> AllowPartialVL
allowPartialVLFromFeatures features =
  if hasPartialVL features then AllowPartialVL else DisallowPartialVL

inferRestrictedSketch ::
  WidthMul ->
  [Int] ->
  [[ValueType]] ->
  SymbolTable DefaultConProg ->
  T.Text ->
  [FeatureSet -> FeatureSet] ->
  SketchSpecTable (WordN 8)
inferRestrictedSketch
  delegatedXMul
  nums
  intermediates
  conTable
  symbol
  transformFeatures =
    let Right (TypeSignature argTypes resTypes) = symbolType conTable symbol
        features = extractProgTableFeature conTable
        argTypesList = argTypes : intermediates
        resTypesList = intermediates ++ [resTypes]
        progs =
          zipWith4
            (\num t -> sketchFromFeatures delegatedXMul num (t features))
            nums
            transformFeatures
            argTypesList
            resTypesList
        names = [symbol <> "_" <> T.pack (show i) | i <- [0 ..]]
        args =
          zipWith (\n ty -> ("arg" <> showAsText n, toSym ty)) [0 ..] argTypes
        nodes =
          zipWith3
            ( \name argTypes resTypes ->
                Concrete.noden
                  ( Leaf
                      [ inj $
                          Invoke
                            (toSym $ TypeSignature argTypes resTypes :: TypeSignature ValueType)
                            name
                      ]
                  )
                  (length resTypes)
            )
            names
            argTypesList
            resTypesList
     in SymbolTable $
          zipWith (\name prog -> (name, CompSpec prog)) names progs
            ++ [ ( symbol,
                   ConSpec $
                     Concrete.buildProg args $ \inputs -> do
                       result <- foldM (\acc f -> f acc) inputs nodes
                       return $ zip result (toSym resTypes)
                 )
               ]

inferSketch ::
  WidthMul ->
  Int ->
  SymbolTable DefaultConProg ->
  T.Text ->
  (FeatureSet -> FeatureSet) ->
  (AllowPartialVL, SketchSpecTable (WordN 8))
inferSketch delegatedXMul num conTable symbol transformFeatures =
  let Right (TypeSignature argTypes resTypes) = symbolType conTable symbol
      features = extractProgTableFeature conTable
      allowPartialVL = allowPartialVLFromFeatures features
   in ( allowPartialVL,
        SymbolTable $ do
          [ ( symbol,
              CompSpec $
                sketchFromFeatures
                  delegatedXMul
                  num
                  (transformFeatures features)
                  argTypes
                  resTypes
            )
            ]
      )

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs
  | n <= 0 = error "Chunk size must be positive"
  | otherwise = take n xs : chunk n (drop n xs)

treeToList :: ChoiceTree SketchOp -> [SketchOp]
treeToList (Leaf x) = concatMap splitChoice x
treeToList (Branch _ xs) = concatMap treeToList xs

toFlatCompChoiceTree :: Int -> ChoiceTree SketchOp -> ChoiceTree SketchOp
toFlatCompChoiceTree n tree =
  Branch
    (Split 0 False)
    (map Leaf splittedGroup)
  where
    list = treeToList tree
    splittedGroup = chunk n list

toFlatCompSketchSpec :: Int -> CompSketchSpec -> CompSketchSpec
toFlatCompSketchSpec n (ComponentBag a bag x) =
  ComponentBag a (first (toFlatCompChoiceTree n) <$> bag) x

toFlatSketchSpec :: Int -> SketchSpec conVarId -> SketchSpec conVarId
toFlatSketchSpec n (CompSpec comp) = CompSpec (toFlatCompSketchSpec n comp)
toFlatSketchSpec _ (ConProg con) = ConProg con
toFlatSketchSpec _ (ConSpec con) = ConSpec con

toFlatSketchSpecTable :: Int -> SketchSpecTable conVarId -> SketchSpecTable conVarId
toFlatSketchSpecTable n (SymbolTable table) =
  SymbolTable $ fmap (second $ toFlatSketchSpec n) table