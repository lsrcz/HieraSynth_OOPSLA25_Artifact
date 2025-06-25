{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Grisette (PPrint, derive)
import Options.Applicative (Parser, auto, help, long, option)
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
import RVV.Synthesizer.Feature.ToSketchOp (inferSketch)
import RVV.Util.Derive (deriveNoSymEval)

newtype Args = Args {_numInferredComponents :: Int}

deriveNoSymEval [''Args]
derive [''Args] [''PPrint]

argsParser :: Parser Args
argsParser =
  Args
    <$> option
      auto
      (long "num-inferred-components" <> help "Number of inferred components")

-- PrevValue_Ascending_128: 5
-- PrevValue_Ascending_64: 2
-- PrevValue_Ascending_32: 2

-- SwapAdjacentPairs_64: 6
-- SwapAdjacentPairs_32: 6

-- SwapAdjacentQuads_64: optimal
-- SwapAdjacentQuads_32: 6

-- SortPairsDistance1_Ascending_128: likely cannot synthesize
-- SortPairsDistance1_Ascending_64: 6
-- SortPairsDistance1_Ascending_32: 7 as 6 proven not viable

-- SortPairsDistance4_Ascending_64: 6
-- SortPairsDistance4_Ascending_32: likely cannot synthesize

main :: IO ()
main =
  mainFunc $
    MainConfig
      { defaultIRFile = "vqsort/vqsort.sir",
        defaultIRFuncName = "PrevValue_Ascending_128",
        defaultTemplateArgType = Nothing,
        defaultIRSpecScalingMethod = "zext",
        defaultMachineConfigSpecs = "vlen32f4,vlen64f4,vlen128f4;vlen128,vlen256,vlen512",
        defaultImmScaleConfig = Nothing,
        defaultLMulDownscaleRatio = 1,
        maybeDefaultSpecification = Nothing,
        overrideGenerators = Nothing,
        defaultFuzzerMaxTests = 1000,
        sketchTable = \(Args numInferredComponents) conTable sketchSymbol _ ->
          inferSketch (1 / 8) numInferredComponents conTable sketchSymbol id,
        extraSketchArgsParser = argsParser
      }
