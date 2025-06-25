{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Text as T
import Grisette (PPrint, ToSym (toSym), WordN, derive)
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree (Leaf))
import HieraSynth.Program.Choice.ComponentBag (ComponentBag (ComponentBag))
import HieraSynth.Program.Concrete (Prog (progArgList, progResList), ProgArg (progArgType), ProgRes (progResType))
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import Options.Applicative (Parser, help, long, switch)
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
import RVV.Semantics.MachineConfig (AllowPartialVL (DisallowPartialVL))
import RVV.Semantics.Policy (muPolicy)
import RVV.Semantics.VectorConfig (vectorMaskMul)
import RVV.Synthesizer.Feature.ToSketchOp (inferSketch)
import RVV.Synthesizer.Op (ConSymbolTable, SketchSpec (CompSpec), SketchSpecTable)
import RVV.Synthesizer.Operator.Common.ImmSpec (ImmSpec (ArbitraryImmSpec))
import RVV.Synthesizer.Operator.Common.RHSSpec (SketchRHSSpec (SketchImmRHS, SketchVectorRHS))
import RVV.Synthesizer.Operator.Move (sketchMoveToMask)
import RVV.Synthesizer.Operator.SetVectorLength (sketchSetMaxVectorLength)
import RVV.Synthesizer.Operator.SingleWidthIntBinary (sketchSingleWidthIntBinary)
import RVV.Synthesizer.Parameter.Destination (Destination (UseProvidedDest))
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask, UseProvidedMask))
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode (SingleWidthIntBinaryOpCode (Add, RSub))
import RVV.Synthesizer.Type (ValueType (VectorType))
import RVV.Util.Derive (deriveNoSymEval)

optimalSketchSpec ::
  ConSymbolTable (WordN 8) ->
  T.Text ->
  (AllowPartialVL, SketchSpecTable (WordN 8))
optimalSketchSpec conTable sketchSymbol =
  ( DisallowPartialVL,
    SymbolTable $ do
      let SymbolTable [(_, conProg)] = conTable
      let argTypes = progArgType <$> progArgList conProg
      let resTypes@[VectorType vtype] = progResType <$> progResList conProg
      [ ( sketchSymbol,
          CompSpec $
            ComponentBag
              (toSym argTypes)
              [ (Leaf [sketchSetMaxVectorLength (vectorMaskMul vtype) [muPolicy]], 1),
                ( Leaf
                    [ sketchMoveToMask
                        (1 / 8)
                        (vectorMaskMul vtype)
                        (SketchImmRHS ArbitraryImmSpec)
                    ],
                  1
                ),
                ( Leaf
                    [ sketchSingleWidthIntBinary
                        vtype
                        [RSub]
                        [UseProvidedDest]
                        [UseProvidedMask]
                        (SketchImmRHS ArbitraryImmSpec)
                    ],
                  1
                ),
                ( Leaf
                    [ sketchSingleWidthIntBinary
                        vtype
                        [Add]
                        [UseProvidedDest]
                        [UseFullMask]
                        SketchVectorRHS
                    ],
                  1
                )
              ]
              (toSym resTypes)
        )
        ]
  )

newtype Args = Args {useOptimalSketch :: Bool}

deriveNoSymEval [''Args]
derive [''Args] [''PPrint]

argsParser :: Parser Args
argsParser =
  Args
    <$> switch
      (long "use-optimal-sketch" <> help "Whether to use the optimal sketch")

main :: IO ()
main =
  mainFunc $
    MainConfig
      { defaultIRFile = "add_sub/add_sub.sir",
        defaultIRFuncName = "AddSub",
        defaultTemplateArgType = Just "vuint8m1_t",
        defaultIRSpecScalingMethod = "zext",
        defaultMachineConfigSpecs = "vlen4f4,vlen8f4,vlen16f4;vlen16,vlen32,vlen64,vlen128,vlen256,vlen512",
        defaultImmScaleConfig = Nothing,
        defaultLMulDownscaleRatio = 1,
        maybeDefaultSpecification = Nothing,
        overrideGenerators = Nothing,
        defaultFuzzerMaxTests = 1000,
        sketchTable = \args conTable sketchSymbol _ ->
          case args of
            Args {useOptimalSketch = True} ->
              optimalSketchSpec conTable sketchSymbol
            Args {useOptimalSketch = False} ->
              inferSketch (1 / 8) 4 conTable sketchSymbol id,
        extraSketchArgsParser = argsParser
      }
