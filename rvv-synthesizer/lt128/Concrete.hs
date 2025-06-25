{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Concrete
  ( lt128Symbol,
    lt128HighwayAsm,
    lt128Manual1,
    compareVVV,
    singleWidthVLongScalar,
    lt128Manual1NoSubProc,
    lt128OptimizeForX280UseShift,
    compareVVVShrink,
    lt128Manual1Shrink,
    lt128Manual1UseElementWiseMM,
  )
where

import Control.Monad.Identity (Identity)
import qualified Data.Text as T
import Grisette (WordN)
import HieraSynth.Combinator.Embed (type (:<:) (inj))
import HieraSynth.Combinator.Invoke (Invoke (Invoke))
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.ProgTyping (ProgTyping (typeProg))
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Policy (muPolicy)
import RVV.Semantics.VectorConfig (VectorConfig, vectorMaskMul)
import RVV.Synthesizer.Op (ConProg)
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS, VectorRHS))
import RVV.Synthesizer.Operator.DelegatedVectorBinaryOnMask (delegatedVectorBinaryOnMask)
import RVV.Synthesizer.Operator.Merge (merge)
import RVV.Synthesizer.Operator.Move (moveToVector)
import RVV.Synthesizer.Operator.Reinterpret (maskToVector, vectorToMask)
import RVV.Synthesizer.Operator.Scalar (scalarLongImm)
import RVV.Synthesizer.Operator.SetVectorLength (setMaxVectorLength)
import RVV.Synthesizer.Operator.SingleWidthIntBinary (singleWidthIntBinary)
import RVV.Synthesizer.Operator.Slide (slide1)
import RVV.Synthesizer.Operator.VectorCompare (vectorCompare)
import RVV.Synthesizer.Operator.VectorIndex (vectorId)
import RVV.Synthesizer.Parameter.Destination (Destination, pd, ud)
import RVV.Synthesizer.Parameter.IntCompareOpCode
  ( IntCompareOpCode,
    mkMSEq,
    mkMSLtu,
    mkMSNe,
  )
import RVV.Synthesizer.Parameter.Masking (Masking, fm, pm)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode,
    mkAnd,
    mkMul,
    mkOr,
    mkSrl,
  )
import RVV.Synthesizer.Parameter.SlideDirection
  ( mkSlideDown,
    mkSlideUp,
  )
import RVV.Synthesizer.Type (ValueType (MaskType, VLType, VectorType))
import Types (vtypeE1M1, vtypeE1MF4)

lt128Symbol :: T.Text
lt128Symbol = "lt128"

lt128HighwayAsm :: SymbolTable (ConProg (WordN 8))
lt128HighwayAsm =
  SymbolTable
    [ ( lt128Symbol,
        Concrete.buildProg
          [("ar", VectorType vtypeE1M1), ("br", VectorType vtypeE1M1)]
          $ \[ar, br] -> do
            vlmax <- Concrete.node1 (setMaxVectorLength @'C 1 muPolicy) []
            sltu <-
              Concrete.node1
                (vectorCompare @'C vtypeE1M1 mkMSLtu ud fm VectorRHS)
                [vlmax, ar, br]
            zeroVec <-
              Concrete.node1
                (moveToVector @'C vtypeE1M1 ud (ImmRHS (ConstImm 0)))
                [vlmax]
            vid <-
              Concrete.node1
                (vectorId @'C vtypeE1M1 ud fm)
                [vlmax]
            sltuVec <-
              Concrete.node1
                (merge @'C vtypeE1M1 ud (ImmRHS (ConstImm (-1))))
                [vlmax, sltu, zeroVec]
            seq <-
              Concrete.node1
                (vectorCompare @'C vtypeE1M1 mkMSEq ud fm VectorRHS)
                [vlmax, ar, br]
            zero <- Concrete.node1 (scalarLongImm @'C 1 (ConstImm 0)) []
            sltuSlide1up <-
              Concrete.node1
                (slide1 @'C vtypeE1M1 mkSlideUp ud fm ScalarRHS)
                [vlmax, sltuVec, zero]
            seqVec <-
              Concrete.node1
                (merge @'C vtypeE1M1 ud (ImmRHS (ConstImm (-1))))
                [vlmax, seq, zeroVec]
            and <-
              Concrete.node1
                ( singleWidthIntBinary @'C
                    vtypeE1M1
                    mkAnd
                    ud
                    fm
                    (ImmRHS (ConstImm 1))
                )
                [vlmax, vid]
            heqllt <-
              Concrete.node1
                ( singleWidthIntBinary @'C
                    vtypeE1M1
                    mkAnd
                    ud
                    fm
                    VectorRHS
                )
                [vlmax, seqVec, sltuSlide1up]
            odd <-
              Concrete.node1
                ( vectorCompare @'C vtypeE1M1 mkMSEq ud fm (ImmRHS (ConstImm 0))
                )
                [vlmax, and]
            sltFinal <-
              Concrete.node1
                ( singleWidthIntBinary @'C
                    vtypeE1M1
                    mkOr
                    ud
                    fm
                    VectorRHS
                )
                [vlmax, sltuVec, heqllt]
            slideDown <-
              Concrete.node1
                (slide1 @'C vtypeE1M1 mkSlideDown pd pm ScalarRHS)
                [vlmax, sltFinal, zero, sltFinal, odd]
            vmsne <-
              Concrete.node1
                ( vectorCompare @'C vtypeE1M1 mkMSNe ud fm (ImmRHS (ConstImm 0))
                )
                [vlmax, slideDown]
            return [(vmsne, MaskType 1)]
      )
    ]

compareVVV ::
  T.Text ->
  VectorConfig ->
  Identity IntCompareOpCode ->
  Identity Destination ->
  Identity Masking ->
  (T.Text, ConProg (WordN 8))
compareVVV
  key
  config
  op
  destination
  masking =
    ( key,
      Concrete.buildProg
        [ ("vl", VLType (vectorMaskMul config)),
          ("lhs", VectorType config),
          ("rhs", VectorType config)
        ]
        $ \[vl, lhs, rhs] -> do
          cmpm <-
            Concrete.node1
              (vectorCompare config op destination masking VectorRHS)
              [vl, lhs, rhs]
          cmpr <-
            Concrete.node1
              ( maskToVector
                  (vectorMaskMul config)
                  config
              )
              [cmpm]
          return [(cmpr, VectorType config)]
    )

compareVVVShrink ::
  T.Text ->
  VectorConfig ->
  VectorConfig ->
  Identity IntCompareOpCode ->
  Identity Destination ->
  Identity Masking ->
  (T.Text, ConProg (WordN 8))
compareVVVShrink
  key
  config
  configRet
  op
  destination
  masking =
    ( key,
      Concrete.buildProg
        [ ("vl", VLType (vectorMaskMul config)),
          ("lhs", VectorType config),
          ("rhs", VectorType config)
        ]
        $ \[vl, lhs, rhs] -> do
          cmpm <-
            Concrete.node1
              (vectorCompare config op destination masking VectorRHS)
              [vl, lhs, rhs]
          cmpr <-
            Concrete.node1
              ( maskToVector
                  (vectorMaskMul config)
                  configRet
              )
              [cmpm]
          return [(cmpr, VectorType configRet)]
    )

singleWidthVLongScalar ::
  T.Text ->
  VectorConfig ->
  Identity SingleWidthIntBinaryOpCode ->
  Identity Destination ->
  Identity Masking ->
  Imm 'C ->
  (T.Text, ConProg (WordN 8))
singleWidthVLongScalar
  key
  config
  op
  destination
  masking
  imm =
    ( key,
      Concrete.buildProg
        [ ("vl", VLType (vectorMaskMul config)),
          ("lhs", VectorType config)
        ]
        $ \[vl, lhs] -> do
          immVec <- Concrete.node1 (scalarLongImm 1 imm) []
          res <-
            Concrete.node1
              ( singleWidthIntBinary @'C
                  config
                  op
                  destination
                  masking
                  ScalarRHS
              )
              [vl, lhs, immVec]
          return [(res, VectorType config)]
    )

lt128Manual1 :: SymbolTable (ConProg (WordN 8))
lt128Manual1 =
  SymbolTable $ do
    let (mseqrSymbol, mseqr) = compareVVV "mseqr" vtypeE1M1 mkMSEq ud fm
    let (msltrSymbol, msltr) = compareVVV "msltr" vtypeE1M1 mkMSLtu ud fm
    let (eqrMaskedSymbol, eqrMasked) =
          singleWidthVLongScalar
            "eqrmasked"
            vtypeE1M1
            mkAnd
            ud
            fm
            (ConstImm 0x5)
    let (ltrMaskedSymbol, ltrMasked) =
          singleWidthVLongScalar
            "ltrmasked"
            vtypeE1M1
            mkAnd
            ud
            fm
            (ConstImm 0x5)
    let (finalSymbol, finalProg) =
          singleWidthVLongScalar
            "final"
            vtypeE1M1
            mkMul
            ud
            fm
            (ConstImm 0x3)
    [ (mseqrSymbol, mseqr),
      (msltrSymbol, msltr),
      (eqrMaskedSymbol, eqrMasked),
      (ltrMaskedSymbol, ltrMasked),
      (finalSymbol, finalProg),
      ( lt128Symbol,
        Concrete.buildProg
          [("ar", VectorType vtypeE1M1), ("br", VectorType vtypeE1M1)]
          $ \[ar, br] -> do
            vlmax <- Concrete.node1 (setMaxVectorLength @'C 1 muPolicy) []
            eqr <-
              Concrete.node1
                (inj @(Invoke ValueType) $ Invoke (typeProg mseqr) mseqrSymbol)
                [vlmax, ar, br]
            ltr <-
              Concrete.node1
                (inj @(Invoke ValueType) $ Invoke (typeProg msltr) msltrSymbol)
                [vlmax, ar, br]
            eqrShifted <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkSrl ud fm (ImmRHS (ConstImm 1)))
                [vlmax, eqr]
            ltrShifted <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkSrl ud fm (ImmRHS (ConstImm 1)))
                [vlmax, ltr]
            eqrShiftedMasked <-
              Concrete.node1
                (inj @(Invoke ValueType) $ Invoke (typeProg eqrMasked) eqrMaskedSymbol)
                [vlmax, eqrShifted]
            ltrShiftedMasked <-
              Concrete.node1
                (inj @(Invoke ValueType) $ Invoke (typeProg ltrMasked) ltrMaskedSymbol)
                [vlmax, ltrShifted]
            eqhiltlo <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkAnd ud fm VectorRHS)
                [vlmax, eqrShiftedMasked, ltr]
            good <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkOr ud fm VectorRHS)
                [vlmax, eqhiltlo, ltrShiftedMasked]
            final <-
              Concrete.node1
                (inj @(Invoke ValueType) $ Invoke (typeProg finalProg) finalSymbol)
                [vlmax, good]
            finalMask <-
              Concrete.node1
                (vectorToMask vtypeE1M1 1)
                [final]
            return [(finalMask, MaskType 1)]
      )
      ]

lt128Manual1NoSubProc :: SymbolTable (ConProg (WordN 8))
lt128Manual1NoSubProc =
  SymbolTable
    [ ( lt128Symbol,
        Concrete.buildProg
          [("ar", VectorType vtypeE1M1), ("br", VectorType vtypeE1M1)]
          $ \[ar, br] -> do
            vlmax <- Concrete.node1 (setMaxVectorLength @'C 1 muPolicy) []
            eqm <-
              Concrete.node1
                (vectorCompare @'C vtypeE1M1 mkMSEq ud fm VectorRHS)
                [vlmax, ar, br]
            ltm <-
              Concrete.node1
                (vectorCompare @'C vtypeE1M1 mkMSLtu ud fm VectorRHS)
                [vlmax, ar, br]
            eqr <-
              Concrete.node1
                (maskToVector 1 vtypeE1M1)
                [eqm]
            ltr <-
              Concrete.node1
                (maskToVector 1 vtypeE1M1)
                [ltm]
            eqrShifted <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkSrl ud fm (ImmRHS (ConstImm 1)))
                [vlmax, eqr]
            ltrShifted <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkSrl ud fm (ImmRHS (ConstImm 1)))
                [vlmax, ltr]
            mask <- Concrete.node1 (scalarLongImm @'C 1 (ConstImm 0x5)) []
            eqrShiftedMasked <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkAnd ud fm ScalarRHS)
                [vlmax, eqrShifted, mask]
            ltrShiftedMasked <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkAnd ud fm ScalarRHS)
                [vlmax, ltrShifted, mask]
            eqhiltlo <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkAnd ud fm VectorRHS)
                [vlmax, eqrShiftedMasked, ltr]
            good <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkOr ud fm VectorRHS)
                [vlmax, eqhiltlo, ltrShiftedMasked]
            three <- Concrete.node1 (scalarLongImm @'C 1 (ConstImm 3)) []
            final <-
              Concrete.node1
                (singleWidthIntBinary @'C vtypeE1M1 mkMul ud fm ScalarRHS)
                [vlmax, good, three]
            finalMask <-
              Concrete.node1
                (vectorToMask vtypeE1M1 1)
                [final]
            return [(finalMask, MaskType 1)]
      )
    ]

lt128OptimizeForX280UseShift :: SymbolTable (ConProg (WordN 8))
lt128OptimizeForX280UseShift = SymbolTable $ do
  let (mseqrSymbol, mseqr) = compareVVV "mseqr" vtypeE1M1 mkMSEq ud fm
  let (msltrSymbol, msltr) = compareVVV "msltr" vtypeE1M1 mkMSLtu ud fm
  let (eqrMaskedSymbol, eqrMasked) =
        singleWidthVLongScalar
          "eqrmasked"
          vtypeE1M1
          mkAnd
          ud
          fm
          (ConstImm 0x5)
  let (finalSymbol, finalProg) =
        singleWidthVLongScalar
          "final"
          vtypeE1M1
          mkMul
          ud
          fm
          (ConstImm 0x3)
  [ (mseqrSymbol, mseqr),
    (msltrSymbol, msltr),
    (eqrMaskedSymbol, eqrMasked),
    (finalSymbol, finalProg),
    ( lt128Symbol,
      Concrete.buildProg
        [("ar", VectorType vtypeE1M1), ("br", VectorType vtypeE1M1)]
        $ \[ar, br] -> do
          vlmax <- Concrete.node1 (setMaxVectorLength @'C 1 muPolicy) []
          eqr <-
            Concrete.node1
              (inj @(Invoke ValueType) $ Invoke (typeProg mseqr) mseqrSymbol)
              [vlmax, ar, br]
          ltr <-
            Concrete.node1
              (inj @(Invoke ValueType) $ Invoke (typeProg msltr) msltrSymbol)
              [vlmax, ar, br]
          eqrShifted <-
            Concrete.node1
              (singleWidthIntBinary @'C vtypeE1M1 mkSrl ud fm (ImmRHS (ConstImm 1)))
              [vlmax, eqr]
          ltrShifted <-
            Concrete.node1
              (singleWidthIntBinary @'C vtypeE1M1 mkSrl ud fm (ImmRHS (ConstImm 1)))
              [vlmax, ltr]
          eqhisltlo <-
            Concrete.node1
              (singleWidthIntBinary @'C vtypeE1M1 mkAnd ud fm VectorRHS)
              [vlmax, eqrShifted, ltr]
          good <-
            Concrete.node1
              (singleWidthIntBinary @'C vtypeE1M1 mkOr ud fm VectorRHS)
              [vlmax, eqhisltlo, ltrShifted]
          goodMasked <-
            Concrete.node1
              (inj @(Invoke ValueType) $ Invoke (typeProg eqrMasked) eqrMaskedSymbol)
              [vlmax, good]
          final <-
            Concrete.node1
              (inj @(Invoke ValueType) $ Invoke (typeProg finalProg) finalSymbol)
              [vlmax, goodMasked]
          finalMask <-
            Concrete.node1
              (maskToVector 1 vtypeE1M1)
              [final]
          return [(finalMask, MaskType 1)]
    )
    ]

lt128Manual1Shrink :: SymbolTable (ConProg (WordN 8))
lt128Manual1Shrink = SymbolTable $ do
  let (mseqrSymbol, mseqr) =
        compareVVVShrink
          "mseqr"
          vtypeE1M1
          vtypeE1MF4
          mkMSEq
          ud
          fm
  let (msltrSymbol, msltr) =
        compareVVVShrink
          "msltr"
          vtypeE1M1
          vtypeE1MF4
          mkMSLtu
          ud
          fm
  let (eqrMaskedSymbol, eqrMasked) =
        singleWidthVLongScalar
          "eqrmasked"
          vtypeE1MF4
          mkAnd
          ud
          fm
          (ConstImm 0x5)
  let (ltrMaskedSymbol, ltrMasked) =
        singleWidthVLongScalar
          "ltrmasked"
          vtypeE1MF4
          mkAnd
          ud
          fm
          (ConstImm 0x5)
  let (finalSymbol, finalProg) =
        singleWidthVLongScalar
          "final"
          vtypeE1MF4
          mkMul
          ud
          fm
          (ConstImm 0x3)
  [ (mseqrSymbol, mseqr),
    (msltrSymbol, msltr),
    (eqrMaskedSymbol, eqrMasked),
    (ltrMaskedSymbol, ltrMasked),
    (finalSymbol, finalProg),
    ( lt128Symbol,
      Concrete.buildProg
        [("ar", VectorType vtypeE1M1), ("br", VectorType vtypeE1M1)]
        $ \[ar, br] -> do
          vlmax4 <- Concrete.node1 (setMaxVectorLength @'C 1 muPolicy) []
          eqr <-
            Concrete.node1
              (inj @(Invoke ValueType) $ Invoke (typeProg mseqr) mseqrSymbol)
              [vlmax4, ar, br]
          ltr <-
            Concrete.node1
              (inj @(Invoke ValueType) $ Invoke (typeProg msltr) msltrSymbol)
              [vlmax4, ar, br]
          vlmax8 <-
            Concrete.node1
              (setMaxVectorLength @'C (1 / 4) muPolicy)
              []
          eqrShifted <-
            Concrete.node1
              (singleWidthIntBinary @'C vtypeE1MF4 mkSrl ud fm (ImmRHS (ConstImm 1)))
              [vlmax8, eqr]
          ltrShifted <-
            Concrete.node1
              (singleWidthIntBinary @'C vtypeE1MF4 mkSrl ud fm (ImmRHS (ConstImm 1)))
              [vlmax8, ltr]
          eqrShiftedMasked <-
            Concrete.node1
              (inj @(Invoke ValueType) $ Invoke (typeProg eqrMasked) eqrMaskedSymbol)
              [vlmax8, eqrShifted]
          ltrShiftedMasked <-
            Concrete.node1
              (inj @(Invoke ValueType) $ Invoke (typeProg ltrMasked) ltrMaskedSymbol)
              [vlmax8, ltrShifted]
          eqhiltlo <-
            Concrete.node1
              (singleWidthIntBinary @'C vtypeE1MF4 mkAnd ud fm VectorRHS)
              [vlmax8, eqrShiftedMasked, ltr]
          good <-
            Concrete.node1
              (singleWidthIntBinary @'C vtypeE1MF4 mkOr ud fm VectorRHS)
              [vlmax8, eqhiltlo, ltrShiftedMasked]
          final <-
            Concrete.node1
              (inj @(Invoke ValueType) $ Invoke (typeProg finalProg) finalSymbol)
              [vlmax8, good]
          finalMask <-
            Concrete.node1
              (vectorToMask vtypeE1MF4 1)
              [final]
          return [(finalMask, MaskType 1)]
    )
    ]

lt128Manual1UseElementWiseMM :: SymbolTable (ConProg (WordN 8))
lt128Manual1UseElementWiseMM =
  SymbolTable
    [ ( lt128Symbol,
        Concrete.buildProg
          [("ar", VectorType vtypeE1M1), ("br", VectorType vtypeE1M1)]
          $ \[ar, br] -> do
            vlmax4 <- Concrete.node1 (setMaxVectorLength @'C 1 muPolicy) []
            eqr <-
              Concrete.node1
                (vectorCompare @'C vtypeE1M1 mkMSEq ud fm VectorRHS)
                [vlmax4, ar, br]
            ltr <-
              Concrete.node1
                (vectorCompare @'C vtypeE1M1 mkMSLtu ud fm VectorRHS)
                [vlmax4, ar, br]
            eqrShifted <-
              Concrete.node1
                (delegatedVectorBinaryOnMask @'C 1 1 mkSrl (ImmRHS (ConstImm 1)))
                [eqr]
            ltrShifted <-
              Concrete.node1
                (delegatedVectorBinaryOnMask @'C 1 1 mkSrl (ImmRHS (ConstImm 1)))
                [ltr]
            eqrShiftedMasked <-
              Concrete.node1
                (delegatedVectorBinaryOnMask @'C 1 1 mkAnd (ImmRHS (ConstImm 0x5)))
                [eqrShifted]
            ltrShiftedMasked <-
              Concrete.node1
                (delegatedVectorBinaryOnMask @'C 1 1 mkAnd (ImmRHS (ConstImm 0x5)))
                [ltrShifted]
            eqhiltlo <-
              Concrete.node1
                (delegatedVectorBinaryOnMask @'C 1 1 mkAnd VectorRHS)
                [eqrShiftedMasked, ltr]
            good <-
              Concrete.node1
                (delegatedVectorBinaryOnMask @'C 1 1 mkOr VectorRHS)
                [eqhiltlo, ltrShiftedMasked]
            final <-
              Concrete.node1
                (delegatedVectorBinaryOnMask @'C 1 1 mkMul (ImmRHS (ConstImm 0x3)))
                [good]
            return [(final, MaskType 1)]
      )
    ]
