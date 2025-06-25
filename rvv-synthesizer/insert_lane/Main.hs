{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( PPrint,
    ToSym (toSym),
    WordN,
    derive,
  )
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree (Leaf))
import HieraSynth.Program.Choice.ComponentBag
  ( ComponentBag (ComponentBag),
  )
import HieraSynth.Program.Concrete
  ( Prog (progArgList, progResList),
    ProgArg (progArgType),
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
import RVV.App.TemplateArgType (TemplateArgType)
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
  ( SketchRHSSpec (SketchImmRHS, SketchScalarRHS),
  )
import RVV.Synthesizer.Operator.ScalarOperator (sketchScalarBin)
import RVV.Synthesizer.Operator.SetVectorLength (sketchSetMaxVectorLength)
import RVV.Synthesizer.Operator.Slide (sketchSlide, sketchSlide1)
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest, UseUndefinedDest),
  )
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask))
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode (SingleWidthIntBinaryOpCode (Add))
import RVV.Synthesizer.Parameter.SlideDirection
  ( SlideDirection (SlideDown, SlideUp),
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

-- Optimal sketch for the InsertLane function
insertLaneOptimalSketch :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
insertLaneOptimalSketch conProg sketchSymbol _ =
  let SymbolTable [(_, p)] = conProg
      argTypes@[VectorType vtype, _, _] = progArgType <$> progArgList p
      resTypes = progResType <$> progResList p
      mmul = vectorMaskMul vtype
   in SymbolTable
        [ ( sketchSymbol,
            CompSpec $
              ComponentBag
                (toSym argTypes)
                [ -- Set vector length (vl3 = vsetvlmax)
                  (Leaf [sketchSetMaxVectorLength mmul [muPolicy]], 1),
                  -- Scalar binary operation with immediate (r4 = add.xi or sub.xi)
                  (Leaf [sketchScalarBin [Add] 1 (SketchImmRHS ArbitraryImmSpec)], 1),
                  -- Slide down operation (v5 = vslidedown.vx)
                  (Leaf [sketchSlide vtype [SlideDown] [UseUndefinedDest] [UseFullMask] SketchScalarRHS], 1),
                  -- Slide up by 1 operation (v6 = vslide1up.vx)
                  (Leaf [sketchSlide1 vtype [SlideUp] [UseProvidedDest] [UseFullMask] SketchScalarRHS], 1),
                  -- Slide up operation (v7 = vslideup.vx)
                  (Leaf [sketchSlide vtype [SlideUp] [UseProvidedDest] [UseFullMask] SketchScalarRHS], 1)
                ]
                (toSym resTypes)
          )
        ]

-- Main sketch selection function
insertLaneSketch :: Args -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> (AllowPartialVL, SketchSpecTable (WordN 8))
insertLaneSketch args conTable sketchSymbol templateArgTypes =
  if useOptimalSketch args
    then (DisallowPartialVL, insertLaneOptimalSketch conTable sketchSymbol templateArgTypes)
    else inferSketch (1 / 8) 5 conTable sketchSymbol id

main :: IO ()
main =
  mainFunc $
    MainConfig
      { defaultIRFile = "insert_lane/insert_lane.sir",
        defaultIRFuncName = "InsertLane",
        defaultTemplateArgType = Just "vuint8m1_t,uint8_t",
        defaultIRSpecScalingMethod = "zext",
        defaultMachineConfigSpecs = "vlen32f4,vlen64f4;vlen128,vlen256,vlen512",
        defaultLMulDownscaleRatio = 1,
        defaultImmScaleConfig = Nothing,
        maybeDefaultSpecification = Nothing,
        overrideGenerators = Nothing,
        defaultFuzzerMaxTests = 1000,
        sketchTable = insertLaneSketch,
        extraSketchArgsParser = argsParser
      }
