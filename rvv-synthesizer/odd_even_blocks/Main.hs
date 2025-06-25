{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
import RVV.Synthesizer.Operator.Merge (sketchMerge)
import RVV.Synthesizer.Operator.SetVectorLength (sketchSetMaxVectorLength)
import RVV.Synthesizer.Operator.SingleWidthIntBinary (sketchSingleWidthIntBinary)
import RVV.Synthesizer.Operator.VectorCompare (sketchVectorCompare)
import RVV.Synthesizer.Operator.VectorIndex (sketchVectorId)
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest, UseUndefinedDest),
  )
import RVV.Synthesizer.Parameter.IntCompareOpCode
  ( IntCompareOpCode (MSLeu),
  )
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask))
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (Xor),
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

oddEvenBlocksOptimalSketch :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
oddEvenBlocksOptimalSketch _ sketchSymbol templateArgTypes =
  let [ty@(VectorType vtype) :: ValueType] =
        toValueType <$> templateArgTypes
      mmul = 4 -- As seen in the synthesized code
   in SymbolTable
        [ ( sketchSymbol,
            CompSpec $
              ComponentBag
                [ty, ty]
                [ -- Set vector length (vl2 = vsetvlmax)
                  (Leaf [sketchSetMaxVectorLength mmul [muPolicy]], 1),
                  -- Generate vector indices (v3 = vid.v)
                  (Leaf [sketchVectorId vtype [UseProvidedDest] [UseFullMask]], 1),
                  -- XOR with immediate (v4 = vxor.vi)
                  (Leaf [sketchSingleWidthIntBinary vtype [Xor] [UseUndefinedDest] [UseFullMask] (SketchImmRHS ArbitraryImmSpec)], 1),
                  -- Vector comparison (m5 = vmsleu.vv)
                  (Leaf [sketchVectorCompare vtype [MSLeu] [UseUndefinedDest] [UseFullMask] SketchVectorRHS], 1),
                  -- Merge vectors (v6 = vmerge.vvm)
                  (Leaf [sketchMerge vtype [UseProvidedDest] SketchVectorRHS], 1)
                ]
                [ty]
          )
        ]

oddEvenBlocksSketch :: Args -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> (AllowPartialVL, SketchSpecTable (WordN 8))
oddEvenBlocksSketch args conTable sketchSymbol templateArgTypes =
  if useOptimalSketch args
    then (DisallowPartialVL, oddEvenBlocksOptimalSketch conTable sketchSymbol templateArgTypes)
    else inferSketch (1 / 8) 5 conTable sketchSymbol id

main :: IO ()
main = do
  mainFunc $
    MainConfig
      { defaultIRFile = "odd_even_blocks/odd_even_blocks.sir",
        defaultIRFuncName = "OddEvenBlocks",
        defaultTemplateArgType = Just "vuint16m1_t",
        defaultIRSpecScalingMethod = "zext",
        defaultMachineConfigSpecs = "vlen32f4,vlen64f4,vlen128f4;vlen128,vlen256,vlen512",
        defaultLMulDownscaleRatio = 1,
        defaultImmScaleConfig = Nothing,
        overrideGenerators = Nothing,
        defaultFuzzerMaxTests = 200,
        maybeDefaultSpecification = Nothing,
        sketchTable = oddEvenBlocksSketch,
        extraSketchArgsParser = argsParser
      }
