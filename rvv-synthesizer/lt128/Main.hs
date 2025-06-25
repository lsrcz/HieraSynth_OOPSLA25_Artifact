{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (when)
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
import Sketch (argsParser, lt128Sketch)

main :: IO ()
main =
  when True $ do
    mainFunc $
      MainConfig
        { defaultIRFile = "lt128/compare.sir",
          defaultIRFuncName = "Lt128",
          defaultTemplateArgType = Just "vbool64_t,vuint64m1_t",
          defaultIRSpecScalingMethod = "zext",
          defaultMachineConfigSpecs = "vlen32f4,vlen64f4,vlen128f4;vlen128,vlen256,vlen512",
          defaultImmScaleConfig = Nothing,
          defaultLMulDownscaleRatio = 1,
          maybeDefaultSpecification = Nothing,
          overrideGenerators = Nothing,
          defaultFuzzerMaxTests = 100,
          -- sketchTable = lt128Sketch2ShrinkMMCombinedSpec2 2 6,
          sketchTable = lt128Sketch,
          -- sketchTable = lt128WithSubProg0 7,
          extraSketchArgsParser = argsParser
        }
