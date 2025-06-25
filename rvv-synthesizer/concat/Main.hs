{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
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
import RVV.Semantics.VectorConfig (halfVectorConfig)
import RVV.Synthesizer.Feature.ToSketchOp (inferSketch)
import RVV.Synthesizer.Op (ConSymbolTable, SketchSpec (CompSpec), SketchSpecTable)
import RVV.Synthesizer.Operator.Extract (sketchExtractVector)
import RVV.Synthesizer.Operator.Insert (sketchInsertVector)
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest),
  )
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
      let half = halfVectorConfig vtype
      [ ( sketchSymbol,
          if
            | "ConcatLowerUpper" `T.isInfixOf` sketchSymbol ->
                CompSpec
                  ( ComponentBag
                      (toSym argTypes)
                      [ (Leaf [sketchExtractVector half vtype [0]], 1),
                        (Leaf [sketchExtractVector half vtype [1]], 1),
                        (Leaf [sketchInsertVector half vtype [UseProvidedDest] [0]], 1),
                        (Leaf [sketchInsertVector half vtype [UseProvidedDest] [1]], 1)
                      ]
                      (toSym resTypes)
                  )
            | "ConcatLowerLower" `T.isInfixOf` sketchSymbol ->
                CompSpec
                  ( ComponentBag
                      (toSym argTypes)
                      [ (Leaf [sketchExtractVector half vtype [0]], 1),
                        (Leaf [sketchInsertVector half vtype [UseProvidedDest] [1]], 1)
                      ]
                      (toSym resTypes)
                  )
            | "ConcatUpperLower" `T.isInfixOf` sketchSymbol ->
                CompSpec
                  ( ComponentBag
                      (toSym argTypes)
                      [ (Leaf [sketchExtractVector half vtype [0]], 1),
                        (Leaf [sketchInsertVector half vtype [UseProvidedDest] [0]], 1)
                      ]
                      (toSym resTypes)
                  )
            | "ConcatUpperUpper" `T.isInfixOf` sketchSymbol ->
                CompSpec
                  ( ComponentBag
                      (toSym argTypes)
                      [ (Leaf [sketchExtractVector half vtype [1]], 1),
                        (Leaf [sketchInsertVector half vtype [UseProvidedDest] [0]], 1)
                      ]
                      (toSym resTypes)
                  )
            | otherwise -> error $ "Unknown sketch symbol: " <> T.unpack sketchSymbol
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
      { defaultIRFile = "concat/concat.sir",
        defaultIRFuncName = "ConcatLowerLower",
        defaultTemplateArgType = Just "vuint8m1_t",
        defaultIRSpecScalingMethod = "no",
        defaultMachineConfigSpecs = "vlen64,vlen128;vlen256,vlen512",
        defaultLMulDownscaleRatio = 1,
        defaultImmScaleConfig = Nothing,
        overrideGenerators = Nothing,
        defaultFuzzerMaxTests = 100,
        maybeDefaultSpecification = Nothing,
        sketchTable = \args conTable sketchSymbol _ ->
          case args of
            Args {useOptimalSketch = True} ->
              optimalSketchSpec conTable sketchSymbol
            Args {useOptimalSketch = False} ->
              if "ConcatLowerUpper" `T.isInfixOf` sketchSymbol
                then inferSketch (1 / 8) 4 conTable sketchSymbol id
                else inferSketch (1 / 8) 2 conTable sketchSymbol id,
        extraSketchArgsParser = argsParser
      }
