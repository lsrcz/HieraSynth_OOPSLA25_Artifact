{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.Text as T
import Grisette
  ( PPrint,
    WordN,
    derive,
  )
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree (Leaf))
import HieraSynth.Program.Choice.ComponentBag (ComponentBag (ComponentBag))
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import Options.Applicative
  ( Parser,
    help,
    long,
    switch,
  )
import RVV.App
  ( MainConfig
      ( MainConfig,
        defaultFuzzerMaxTests,
        defaultIRFile,
        defaultIRFuncName,
        defaultIRSpecScalingMethod,
        defaultImmScaleConfig,
        defaultLMulDownscaleRatio,
        defaultMachineConfigSpecs,
        defaultTemplateArgType,
        extraSketchArgsParser,
        maybeDefaultSpecification,
        overrideGenerators,
        sketchTable
      ),
    mainFunc,
  )
import RVV.App.TemplateArgType (TemplateArgType, toValueType)
import RVV.Semantics.MachineConfig (AllowPartialVL (DisallowPartialVL))
import RVV.Semantics.Policy (muPolicy)
import RVV.Semantics.VectorConfig (vectorMaskMul)
import RVV.Synthesizer.Feature.ToSketchOp (inferSketch)
import RVV.Synthesizer.Op
  ( ConSymbolTable,
    SketchSpec (CompSpec),
    SketchSpecTable,
  )
import RVV.Synthesizer.Operator.Common.ImmSpec
  ( ImmSpec (ArbitraryImmSpec),
  )
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( SketchRHSSpec (SketchImmRHS, SketchVectorRHS),
  )
import RVV.Synthesizer.Operator.Move (sketchMoveToMask)
import RVV.Synthesizer.Operator.Reinterpret (sketchVectorToVector)
import RVV.Synthesizer.Operator.SetVectorLength (sketchSetMaxVectorLength)
import RVV.Synthesizer.Operator.SingleWidthIntBinary (sketchSingleWidthIntBinary)
import RVV.Synthesizer.Operator.Slide (sketchSlide1)
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest, UseUndefinedDest),
  )
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask, UseProvidedMask))
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (Mul, Mulhu),
  )
import RVV.Synthesizer.Parameter.SlideDirection
  ( SlideDirection (SlideDown, SlideUp),
  )
import RVV.Synthesizer.Type (ValueType (VectorType))
import RVV.Util.Derive (deriveNoSymEval)

newtype Args = Args {useOptimalSketch :: Bool}

deriveNoSymEval [''Args]
derive [''Args] [''PPrint]

argsParser :: Parser Args
argsParser =
  Args
    <$> switch
      ( long "use-optimal-sketch"
          <> help
            ( "Use the optimal sketch for the function. "
                <> "Default is to use the inferred sketch."
            )
      )

mulEvenOptimalSketch :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
mulEvenOptimalSketch _ sketchSymbol templateArgTypes =
  let [destType@(VectorType destVtype), srcType@(VectorType srcVtype) :: ValueType] =
        toValueType <$> templateArgTypes
      mmul = vectorMaskMul srcVtype
      delegatedXMul = 1 / 8 -- 8-bit mask element width
   in SymbolTable
        [ ( sketchSymbol,
            CompSpec $
              ComponentBag
                [srcType, srcType]
                [ -- Set vector length (vl2 = vsetvlmax)
                  (Leaf [sketchSetMaxVectorLength mmul [muPolicy]], 1),
                  -- Regular multiplication (v3 = vmul.vv)
                  (Leaf [sketchSingleWidthIntBinary srcVtype [Mul] [UseUndefinedDest] [UseFullMask] SketchVectorRHS], 1),
                  -- High bits of unsigned multiplication (v4 = vmulhu.vv)
                  (Leaf [sketchSingleWidthIntBinary srcVtype [Mulhu] [UseUndefinedDest] [UseFullMask] SketchVectorRHS], 1),
                  -- Create mask (m5 = imm_to_mask)
                  (Leaf [sketchMoveToMask delegatedXMul mmul (SketchImmRHS ArbitraryImmSpec)], 1),
                  -- Slide elements up (v6 = vslide1up.vi)
                  (Leaf [sketchSlide1 srcVtype [SlideUp] [UseProvidedDest] [UseProvidedMask] (SketchImmRHS ArbitraryImmSpec)], 1),
                  -- Convert vector type (r7 = vec_to_vec)
                  (Leaf [sketchVectorToVector srcVtype destVtype], 1)
                ]
                [destType]
          )
        ]

mulOddOptimalSketch :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
mulOddOptimalSketch _ sketchSymbol templateArgTypes =
  let [destType@(VectorType destVtype), srcType@(VectorType srcVtype) :: ValueType] =
        toValueType <$> templateArgTypes
      mmul = vectorMaskMul srcVtype
      delegatedXMul = 1 / 8 -- 8-bit mask element width
   in SymbolTable
        [ ( sketchSymbol,
            CompSpec $
              ComponentBag
                [srcType, srcType]
                [ -- Create mask (m2 = imm_to_mask)
                  (Leaf [sketchMoveToMask delegatedXMul mmul (SketchImmRHS ArbitraryImmSpec)], 1),
                  -- Set vector length (vl3 = vsetvlmax)
                  (Leaf [sketchSetMaxVectorLength mmul [muPolicy]], 1),
                  -- Masked multiplication (v4 = vmul.vv)
                  (Leaf [sketchSingleWidthIntBinary srcVtype [Mul] [UseProvidedDest] [UseProvidedMask] SketchVectorRHS], 1),
                  -- Slide elements down (v5 = vslide1down.vi)
                  (Leaf [sketchSlide1 srcVtype [SlideDown] [UseProvidedDest] [UseFullMask] (SketchImmRHS ArbitraryImmSpec)], 1),
                  -- Masked high bits of unsigned multiplication (v6 = vmulhu.vv)
                  (Leaf [sketchSingleWidthIntBinary srcVtype [Mulhu] [UseProvidedDest] [UseProvidedMask] SketchVectorRHS], 1),
                  -- Convert vector type (r7 = vec_to_vec)
                  (Leaf [sketchVectorToVector srcVtype destVtype], 1)
                ]
                [destType]
          )
        ]

mulEvenOddSketch :: Args -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> (AllowPartialVL, SketchSpecTable (WordN 8))
mulEvenOddSketch args conTable sketchSymbol templateArgTypes =
  if useOptimalSketch args
    then
      -- Choose the optimal sketch based on the function name
      if "MulEven" `T.isInfixOf` sketchSymbol
        then (DisallowPartialVL, mulEvenOptimalSketch conTable sketchSymbol templateArgTypes)
        else (DisallowPartialVL, mulOddOptimalSketch conTable sketchSymbol templateArgTypes)
    else
      -- Use the inferred sketch
      inferSketch (1 / 8) 6 conTable sketchSymbol id

main :: IO ()
main =
  mainFunc $
    MainConfig
      { defaultIRFile = "mul_even_odd/mul_even_odd.sir",
        defaultIRFuncName = "MulEven",
        defaultTemplateArgType = Just "vuint16m1_t,vuint8m1_t",
        defaultIRSpecScalingMethod = "no",
        defaultMachineConfigSpecs = "vlen16f4,vlen32f4;vlen64,vlen128,vlen256,vlen512",
        defaultImmScaleConfig = Nothing,
        defaultLMulDownscaleRatio = 1,
        maybeDefaultSpecification = Nothing,
        overrideGenerators = Nothing,
        defaultFuzzerMaxTests = 1000,
        sketchTable = mulEvenOddSketch,
        extraSketchArgsParser = argsParser
      }
