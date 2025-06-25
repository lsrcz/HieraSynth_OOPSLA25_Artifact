{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( PPrint,
    WordN,
    derive,
  )
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree (Leaf))
import HieraSynth.Program.Choice.ComponentBag
  ( ComponentBag (ComponentBag),
  )
import HieraSynth.Program.Concrete
  ( Prog (progResList),
    ProgRes (progResType),
  )
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
import RVV.Semantics.VectorConfig (VectorConfig (lengthMul))
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
  ( SketchRHSSpec (SketchImmRHS),
  )
import RVV.Synthesizer.Operator.Extract (sketchExtractVector)
import RVV.Synthesizer.Operator.Insert (sketchInsertVector)
import RVV.Synthesizer.Operator.Move (sketchMoveToVector)
import RVV.Synthesizer.Operator.SetVectorLength (sketchSetMaxVectorLength)
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest, UseUndefinedDest),
  )
import RVV.Synthesizer.Type (ValueType (VectorType))
import RVV.Util.Derive (deriveNoSymEval)

newtype Args = Args {useOptimalSketch :: Bool}
  deriving (Generic)

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

-- Optimal sketch for converting between different element types (e.g., vuint8m1_t to vuint16mf2_t)
zeroExtendResizeBitCastShrinkingOptimalSketch :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
zeroExtendResizeBitCastShrinkingOptimalSketch _ sketchSymbol templateArgTypes =
  let [destType@(VectorType destVtype), srcType@(VectorType srcVtype) :: ValueType] =
        toValueType <$> templateArgTypes
   in SymbolTable
        [ ( sketchSymbol,
            CompSpec $
              ComponentBag
                [srcType]
                [ -- Get operation (r5 = vget)
                  (Leaf [sketchExtractVector destVtype srcVtype [0]], 1)
                ]
                [destType]
          )
        ]

-- Optimal sketch for converting between same element types with different LMUL (e.g., vuint8m1_t to vuint8m2_t)
zeroExtendResizeBitCastExtendingOptimalSketch :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
zeroExtendResizeBitCastExtendingOptimalSketch _ sketchSymbol templateArgTypes =
  let [destType@(VectorType destVtype), srcType@(VectorType srcVtype) :: ValueType] =
        toValueType <$> templateArgTypes
      mmul = 16 -- As seen in the synthesized code
   in SymbolTable
        [ ( sketchSymbol,
            CompSpec $
              ComponentBag
                [srcType]
                [ -- Set vector length (vl1 = vsetvlmax)
                  (Leaf [sketchSetMaxVectorLength mmul [muPolicy]], 1),
                  -- Create zero vector (v4 = imm_to_vec)
                  (Leaf [sketchMoveToVector destVtype [UseUndefinedDest] (SketchImmRHS ArbitraryImmSpec)], 1),
                  -- Set operation (r5 = vset)
                  (Leaf [sketchInsertVector srcVtype destVtype [UseProvidedDest] [0]], 1)
                ]
                [destType]
          )
        ]

-- Determine which sketch to use based on the template argument types
zeroExtendResizeBitCastOptimalSketch :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> (AllowPartialVL, SketchSpecTable (WordN 8))
zeroExtendResizeBitCastOptimalSketch conTable sketchSymbol templateArgTypes =
  let SymbolTable [(_, conProg)] = conTable
      [VectorType vtype] = progResType <$> progResList conProg
   in if lengthMul vtype < 1
        then (DisallowPartialVL, zeroExtendResizeBitCastShrinkingOptimalSketch conTable sketchSymbol templateArgTypes)
        else (DisallowPartialVL, zeroExtendResizeBitCastExtendingOptimalSketch conTable sketchSymbol templateArgTypes)

-- Main sketch selection function
zeroExtendResizeBitCastSketch :: Args -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> (AllowPartialVL, SketchSpecTable (WordN 8))
zeroExtendResizeBitCastSketch args conTable sketchSymbol templateArgTypes =
  if useOptimalSketch args
    then zeroExtendResizeBitCastOptimalSketch conTable sketchSymbol templateArgTypes
    else
      let SymbolTable [(_, conProg)] = conTable
          [VectorType vtype] = progResType <$> progResList conProg
       in if lengthMul vtype < 1
            then inferSketch (1 / 8) 1 conTable sketchSymbol id
            else inferSketch (1 / 8) 3 conTable sketchSymbol id

main :: IO ()
main =
  mainFunc $
    MainConfig
      { defaultIRFile =
          "zero_extend_resize_bit_cast/zero_extend_resize_bit_cast.sir",
        defaultIRFuncName = "ZeroExtendResizeBitCast",
        defaultTemplateArgType = Just "vuint16mf2_t,vuint8m1_t",
        defaultIRSpecScalingMethod = "no",
        defaultMachineConfigSpecs = "vlen32f2,vlen64;vlen128,vlen256,vlen512",
        defaultImmScaleConfig = Nothing,
        defaultLMulDownscaleRatio = 1,
        overrideGenerators = Nothing,
        maybeDefaultSpecification = Nothing,
        defaultFuzzerMaxTests = 100,
        sketchTable = zeroExtendResizeBitCastSketch,
        extraSketchArgsParser = argsParser
      }
