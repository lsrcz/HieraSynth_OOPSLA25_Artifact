{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    PPrint,
  )
import InferredSketch (getInferredSketch)
import OptimalSketch (getOptimalSketch)
import Options.Applicative
  ( Parser,
    auto,
    help,
    long,
    option,
    showDefault,
    switch,
  )
import Options.Applicative.Builder (value)
import OriginalSketch (getOriginalSketch)
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
import StdLibSketch (getStdLibSketch)

data Args = Args
  { useStandardLibApproach :: Bool,
    numOfInferredInst :: Int,
    hasDiv :: Bool,
    hasMul :: Bool,
    useOriginal :: Bool,
    useOptimal :: Bool,
    useOriginalFixedImm :: Bool
  }
  deriving (Generic)
  deriving (PPrint) via (Default Args)

argsParser :: Parser Args
argsParser =
  Args
    <$> switch
      ( long "use-stdlib"
          <> help "Whether to use the standard library approach"
      )
    <*> option
      auto
      ( long "num-of-inferred-inst"
          <> help "Number of inferred instructions"
          <> showDefault
          <> value 4
      )
    <*> switch
      ( long "has-div"
          <> help "Whether the instruction set has div"
      )
    <*> switch
      ( long "has-mul"
          <> help "Whether the instruction set has mul"
      )
    <*> switch
      ( long "use-original"
          <> help "Whether to use the original sketch"
      )
    <*> switch
      ( long "use-optimal"
          <> help "Whether to use the optimal sketch"
      )
    <*> switch
      ( long "use-fixed-imm"
          <> help "Whether to use the fixed immediate sketch"
      )

main :: IO ()
main = do
  mainFunc $
    MainConfig
      { defaultIRFile = "hacker/problems.sir",
        defaultIRFuncName = "p01",
        defaultTemplateArgType = Nothing,
        defaultIRSpecScalingMethod = "no",
        defaultMachineConfigSpecs = "vlen4f32,vlen8f16,vlen16f8;vlen128",
        defaultImmScaleConfig = Nothing,
        defaultLMulDownscaleRatio = 1,
        maybeDefaultSpecification = Nothing,
        overrideGenerators = Nothing,
        defaultFuzzerMaxTests = 50,
        sketchTable =
          \(Args stdLib n hasDiv hasMul useOriginal useOptimal useFixedImm)
           conTable
           sketchSymbol
           _ ->
              if length (filter id [useOriginal, useOptimal, stdLib]) > 1
                then error "Only one of use-original, use-optimal, and use-stdlib can be true"
                else case (stdLib, useOriginal, useOptimal) of
                  (True, _, _) -> (DisallowPartialVL, getStdLibSketch useFixedImm sketchSymbol)
                  (_, True, _) -> (DisallowPartialVL, getOriginalSketch useFixedImm sketchSymbol)
                  (_, _, True) -> (DisallowPartialVL, getOptimalSketch useFixedImm sketchSymbol)
                  _ -> (DisallowPartialVL, getInferredSketch n hasDiv hasMul conTable sketchSymbol),
        extraSketchArgsParser = argsParser
      }
