{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RVV.App
  ( Solver (..),
    Args (..),
    MainConfig (..),
    parser,
    mainFunc,
    UseArbitraryImmArg (..),
    useArbitraryImmArgParser,
  )
where

import Control.Monad (void)
import Data.Either (fromRight)
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust, fromMaybe, isJust, maybeToList)
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Void (Void)
import Demangler (demangle1, functionName)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    GrisetteSMTConfig (sbvConfig),
    PPrint (pformat),
    SMTConfig (extraArgs),
    SymWordN,
    ToSym (toSym),
    WordN,
    bitwuzla,
    boolector,
    nest,
    viaShow,
    vsep,
    yices,
    z3,
  )
import HieraSynth.Context (ConcreteContext)
import HieraSynth.Program.Choice.Counting
  ( CountNumProgsEvidence (CountNumProgsEvidence),
  )
import HieraSynth.Program.Concrete
  ( eliminateProgTableDeadCode,
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (PerStmtCostObj),
  )
import HieraSynth.Program.ProgCost (symbolCost)
import HieraSynth.Program.ProgParser (progTableParser)
import HieraSynth.Program.ProgTyping (symbolType)
import HieraSynth.Program.SymbolTable
  ( SymbolTable (SymbolTable),
    filterByReachableSymbols,
  )
import HieraSynth.Reasoning.Parallel.Scheduler
  ( ProcessConstraint,
    SchedulerConfig
      ( SchedulerConfig,
        biasedDrawProbability,
        cmdline,
        costObj,
        countNumProgsEvidence,
        doDeadCodeElimination,
        generalizationSketchFromFastResult,
        enablePlotting,
        enableTreeStats,
        exactCost,
        fastTrackTimeoutSeconds,
        initialMinimalCost,
        initialSplitRatio,
        initialTimeoutSeconds,
        logConfig,
        logger,
        parallelism,
        pollIntervalSeconds,
        referenceNumInsts,
        restartRunningTimeThresholdSeconds,
        schedulerRandomSeed,
        schedulerTimeoutSeconds,
        solverConfig,
        successNodeNewTimeoutSeconds,
        synthesisSketchSymbol,
        targetCost,
        transcriptSMT,
        verifiers
      ),
    getDefaultLogger,
    getLogConfig,
    runWithScheduler,
  )
import HieraSynth.Reasoning.Synthesis (SomeVerifier (SomeVerifier))
import HieraSynth.TypeSignature (TypeSignature (argTypes))
import HieraSynth.Util.Logging (logMultiLineDoc)
import Grisette.Unified (EvalModeTag (C, S))
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    option,
    short,
    showDefault,
    showDefaultWith,
    str,
    switch,
    value,
  )
import RVV.App.IRSpecScalingMethod
  ( IRSpecScalingMethod (NoScale, SextScale, ZextScale),
    irSpecScalingMethodParser,
  )
import RVV.App.MachineConfigSpec
  ( MachineConfigSpec,
    toMachineConfig,
    vconstSpecListParser,
  )
import RVV.App.Solver (Solver (Bitwuzla, Boolector, Yices, Z3), solverParser)
import RVV.App.TemplateArgType
  ( TemplateArgType,
    templateArgTypeListParser,
    toClangTemplateArgListTxt,
  )
import RVV.Semantics.MachineConfig
  ( AllowPartialVL,
    MachineBaseConfig
      ( MachineBaseConfig,
        machineMemoryBlockIds,
        machinePointerUnit,
        machineScalarImmLength,
        machineScalarLength,
        machineVectorImmLength
      ),
    MachineConfig,
  )
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.CostModel.X280ApproxModel
  ( x280ApproxModel,
  )
import RVV.Synthesizer.DefaultSynthType (DefaultConProg, DefaultSymProg)
import RVV.Synthesizer.Feature.ToSketchOp (toFlatSketchSpecTable)
import RVV.Synthesizer.Generator (randomValidInputsForSymbol)
import RVV.Synthesizer.Matcher (RegListMatcher)
import RVV.Synthesizer.Op
  ( ConProg,
    ConSymbolTable,
    SketchSpec,
    SketchSpecTable,
    SymProg,
  )
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.RefProg (fullMatchSpecFromRefProg)
import RVV.Synthesizer.Specification.Scale
  ( downscaleSpec',
    fullImm,
    scaleMachineConfig,
    scaleValue,
    sextUpscaleFun,
    zextUpscaleFun,
  )
import RVV.Synthesizer.Specification.ScaleConstants
  ( scaledFullMatchSpecFromRefProg,
    selectorAndNewImmMapParsers,
  )
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Synthesizer.Task
  ( CexReductionBehavior (SetToOne, ZeroOut),
    RVVCombinedVerifierFuzzer
      ( RVVCombinedVerifierFuzzer,
        rvvCombinedVerifierFuzzerCexReductionBehavior,
        rvvCombinedVerifierFuzzerFastVerifierTimeoutSeconds,
        rvvCombinedVerifierFuzzerFuzzerGenerators,
        rvvCombinedVerifierFuzzerFuzzerMaxTests,
        rvvCombinedVerifierFuzzerFuzzerSpec,
        rvvCombinedVerifierFuzzerLogger,
        rvvCombinedVerifierFuzzerMachineConfigs,
        rvvCombinedVerifierFuzzerSlowVerifierTimeoutSeconds,
        rvvCombinedVerifierFuzzerSolverConfig,
        rvvCombinedVerifierFuzzerVerifierInputs,
        rvvCombinedVerifierFuzzerVerifierSpec
      ),
  )
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value, freshValidValue, freshValidValue')
import RVV.Util.Demangle (demangleResultToText)
import RVV.Util.Parser (parseFullInputOrDie, parseOrDie)
import System.Environment (getArgs, getExecutablePath)
import System.FilePath (takeFileName)
import System.Log (Priority (NOTICE))
import Test.QuickCheck (Gen, resize)
import Text.Megaparsec (errorBundlePretty, parse)

data Args extraSketchArgs = Args
  { timeoutSeconds :: Int,
    schedulerTimeoutSeconds :: Maybe Int,
    solverConfig :: T.Text,
    parallelism :: Int,
    targetCost :: Int,
    fuzzerMaxTests :: Int,
    noGraduallyDecreaseMinimalCostByBestResult :: Bool,
    restartRunningTimeThresholdSeconds :: Maybe Int,
    successNodeNewTimeoutSeconds :: Maybe Int,
    fastTrackTimeoutSeconds :: Maybe Int,
    irFile :: FilePath,
    irFuncName :: T.Text,
    irSpecScalingMethod :: T.Text,
    forceIRSpec :: Bool,
    templateArgTypes :: Maybe T.Text,
    vconsts :: T.Text,
    logBaseDir :: Maybe FilePath,
    logProgName :: Maybe String,
    doResizeGenerators :: Bool,
    doImmSynth :: Bool,
    initialCost :: Maybe Int,
    initialCostOffset :: Int,
    extraSketchArgs :: extraSketchArgs,
    extraSolverArgs :: String,
    transcriptSMT :: Bool,
    immScalingConfig :: Maybe T.Text,
    lmulDownscaleRatio :: Int,
    verificationFastTimeout :: Double,
    verificationSlowTimeout :: Double,
    doDeadCodeElimination :: Bool,
    schedulerRandomSeed :: Int,
    initialSplitRatio :: Int,
    computeAdaptiveTimeoutAfter :: Int,
    useFullScalarCost :: Bool,
    pollIntervalSeconds :: Double,
    useExactCost :: Bool,
    biasedDrawProbability :: Double,
    useVLMask :: Bool,
    cexReductionBehavior :: T.Text,
    naiveSplitting :: Maybe Int,
    enablePlotting :: Bool,
    enableTreeStats :: Bool,
    enableDebugLogging :: Bool
  }
  deriving (Show, Generic)
  deriving (PPrint) via (Default (Args extraSketchArgs))

parser ::
  String ->
  T.Text ->
  Maybe T.Text ->
  T.Text ->
  T.Text ->
  Int ->
  Maybe T.Text ->
  Int ->
  Parser extraSketchArgs ->
  Parser (Args extraSketchArgs)
parser
  defaultIRFile
  defaultIRFuncName
  defaultTemplateArgType
  defaultIRSpecScalingMethod
  defaultMachineConfigSpecs
  defaultFuzzerMaxTests
  defaultImmScaleConfig
  defaultLMulDownscaleRatio
  extraSketchArgsParser =
    Args
      <$> option
        auto
        ( long "timeout"
            <> short 't'
            <> help "Timeout in seconds"
            <> showDefault
            <> value 500
        )
      <*> option
        (Just <$> auto)
        ( long "scheduler-timeout"
            <> help "Scheduler timeout in seconds"
            <> showDefault
            <> value Nothing
        )
      <*> option
        str
        ( long "solver"
            <> short 's'
            <> help "Solver to use"
            <> showDefault
            <> value "bitwuzla"
        )
      <*> option
        auto
        ( long "parallelism"
            <> short 'j'
            <> help "Parallelism"
            <> showDefault
            <> value 1
        )
      <*> option
        auto
        ( long "target-cost"
            <> short 'c'
            <> help "Target cost"
            <> showDefault
            <> value 0
        )
      <*> option
        auto
        ( long "fuzzer-max-tests"
            <> short 'f'
            <> help "Fuzzer max tests (non-negative value disables fuzzing)"
            <> showDefault
            <> value defaultFuzzerMaxTests
        )
      <*> switch
        ( long "no-decrease-cost"
            <> short 'n'
            <> help "Do not gradually decrease minimal cost by best result"
        )
      <*> option
        (Just <$> auto)
        ( long "restart-running-time-threshold"
            <> short 'r'
            <> help
              ( "Restart running time threshold (s), all pending tasks that "
                  <> "has run for less than this time and have no known result "
                  <> "will be restarted when a new cost is found."
              )
            <> showDefaultWith (const "0.2 * basic timeout")
            <> value Nothing
        )
      <*> option
        (Just <$> auto)
        ( long "success-node-new-timeout"
            <> help "New timeout for refining success node"
            <> showDefaultWith (const "3 * basic timeout")
            <> value Nothing
        )
      <*> option
        (Just <$> auto)
        ( long "fast-track-timeout"
            <> help "New timeout for synthesizing fast track node imms"
            <> showDefaultWith (const "basic timeout")
            <> value Nothing
        )
      <*> option
        str
        ( long "ir-file"
            <> help "IR file"
            <> showDefault
            <> value defaultIRFile
        )
      <*> option
        str
        ( long "ir-func-name"
            <> help "IR function name"
            <> showDefault
            <> value defaultIRFuncName
        )
      <*> option
        str
        ( long "ir-spec-scaling-method"
            <> help "IR spec scaling method [no|zext|sext]"
            <> showDefault
            <> value defaultIRSpecScalingMethod
        )
      <*> switch
        ( long "force-ir-spec"
            <> help
              ( "Force IR spec, ignore the provided default specification "
                  <> "in the configuration"
              )
        )
      <*> option
        (Just <$> str)
        ( long "template-arg-types"
            <> short 'a'
            <> help "Template argument types"
            <> showDefault
            <> value defaultTemplateArgType
        )
      <*> option
        str
        ( long "vconsts"
            <> help "MachineConfigs to use"
            <> showDefault
            <> value defaultMachineConfigSpecs
        )
      <*> option
        (Just <$> str)
        ( long "log-base-dir"
            <> help "Log base directory"
            <> showDefaultWith (const "The IR function name")
            <> value Nothing
        )
      <*> option
        (Just <$> str)
        ( long "log-prog-name"
            <> help "Log program name"
            <> showDefaultWith (const "The IR function name")
            <> value Nothing
        )
      <*> switch
        ( long "do-resize-generators"
            <> help
              ( "Do resize generators. This means that the fuzzer will not "
                  <> "start with small inputs like 1, 2, 3 as elements in the "
                  <> "inputs."
              )
        )
      <*> option
        auto
        ( long "do-imm-synth"
            <> help "Do imm synthesis"
            <> value True
        )
      <*> option
        (Just <$> auto)
        ( long "initial-cost"
            <> help "Initial cost"
            <> showDefaultWith (const "The cost of the function in the IR")
            <> value Nothing
        )
      <*> option
        auto
        ( long "initial-cost-offset"
            <> help
              ( "Initial cost offset. The initial cost would be adjusted "
                  <> "by this offset if we do not specify the initial cost from the command line"
              )
            <> showDefault
            <> value 0
        )
      <*> extraSketchArgsParser
      <*> option
        str
        ( long "extra-solver-args"
            <> help "Extra solver arguments"
            <> showDefault
            <> value ""
        )
      <*> switch
        ( long "transcript-smt"
            <> help "Transcript SMT"
        )
      <*> option
        (Just <$> str)
        ( long "imm-scaling-config"
            <> help
              ( "Imm scaling config, example: "
                  <> "s:[1:2@2|3:4~3]|g:[1:2,3:4@2], must be used with "
                  <> "--ir-spec-scaling-method=no"
              )
            <> showDefaultWith (T.unpack . fromMaybe "unspecified")
            <> value defaultImmScaleConfig
        )
      <*> option
        auto
        ( long "lmul-downscale-ratio"
            <> help "Downscale ratio for lmul"
            <> showDefault
            <> value defaultLMulDownscaleRatio
        )
      <*> option
        auto
        ( long "verification-fast-timeout"
            <> help
              ( "Verification fast timeout "
                  <> "(negative value disables fast verification)"
              )
            <> showDefault
            <> value 5
        )
      <*> option
        auto
        ( long "verification-slow-timeout"
            <> help
              ( "Verification slow timeout "
                  <> "(negative value disables slow verification)"
              )
            <> showDefault
            <> value 50
        )
      <*> option
        auto
        ( long "eliminate-dead-code"
            <> help "Eliminate dead code"
            <> showDefault
            <> value True
        )
      <*> option
        auto
        ( long "scheduler-random-seed"
            <> help "Scheduler random seed"
            <> showDefault
            <> value 137
        )
      <*> option
        auto
        ( long "initial-split-ratio"
            <> help "Initial split ratio"
            <> showDefault
            <> value 0
        )
      <*> option
        auto
        ( long "compute-adaptive-timeout-after"
            <> help "Compute adaptive timeout after this number of failures"
            <> showDefault
            <> value 200
        )
      <*> switch
        ( long "full-scalar-cost"
            <> help "Use scalar cost model"
            <> showDefault
        )
      <*> option
        auto
        ( long "poll-interval"
            <> help "Poll interval in seconds"
            <> showDefault
            <> value 1
        )
      <*> switch
        ( long "exact-cost"
            <> help "Use exact cost"
            <> showDefault
        )
      <*> option
        auto
        ( long "biased-draw-probability"
            <> help "Biased draw probability"
            <> showDefault
            <> value 0.5
        )
      <*> option
        auto
        ( long "use-vl-mask"
            <> help "Use VL mask"
            <> showDefault
            <> value False
        )
      <*> option
        str
        ( long "cex-reduction-behavior"
            <> help "Behavior for counter-example reduction (zero-out or set-to-one)"
            <> showDefault
            <> value "set-to-one"
        )
      <*> option
        (Just <$> auto)
        ( long "naive-splitting"
            <> help "Naive splitting"
            <> showDefault
            <> value Nothing
        )
      <*> switch
        ( long "enable-plotting"
            <> help "Enable plotting"
            <> showDefault
        )
      <*> switch
        ( long "enable-tree-stats"
            <> help "Enable tree stats"
            <> showDefault
        )
      <*> switch
        ( long "enable-debug-logging"
            <> help "Enable debug logging"
            <> showDefault
        )

data MainConfig extraSketchArgs = MainConfig
  { defaultIRFile :: String,
    defaultIRFuncName :: T.Text,
    defaultTemplateArgType :: Maybe T.Text,
    defaultIRSpecScalingMethod :: T.Text,
    defaultMachineConfigSpecs :: T.Text,
    defaultImmScaleConfig :: Maybe T.Text,
    defaultFuzzerMaxTests :: Int,
    defaultLMulDownscaleRatio :: Int,
    overrideGenerators ::
      Maybe
        [ ConSymbolTable (WordN 8) ->
          T.Text ->
          MachineConfig ->
          Gen [Value 'C]
        ],
    maybeDefaultSpecification ::
      Maybe
        ( ConSymbolTable (WordN 8) ->
          T.Text ->
          MachineConfig ->
          [Value 'C] ->
          ConcreteContext ([Value 'C], RegListMatcher)
        ),
    sketchTable ::
      extraSketchArgs ->
      ConSymbolTable (WordN 8) ->
      T.Text ->
      [TemplateArgType] ->
      (AllowPartialVL, SketchSpecTable (WordN 8)),
    extraSketchArgsParser :: Parser extraSketchArgs
  }

mayQuoteArgument :: String -> String
mayQuoteArgument s
  | any (`elem` [' ', '\t', '\n']) s = "\"" <> s <> "\""
  | otherwise = s

mainFunc :: (PPrint extraSketchArgs) => MainConfig extraSketchArgs -> IO ()
mainFunc MainConfig {..} = do
  exePath <- getExecutablePath
  args0 <- getArgs
  args@Args {..} <- execParser opts
  let logBaseDir' = fromMaybe "logs" logBaseDir
  let logProgName' = fromMaybe (T.unpack irFuncName) logProgName
  logConfig <- getLogConfig logBaseDir' logProgName'
  logger <- getDefaultLogger logConfig enableDebugLogging
  let cmdline = unwords $ takeFileName exePath : (mayQuoteArgument <$> args0)
  let parsedMachineConfigs =
        parseFullInputOrDie vconstSpecListParser vconsts
  let parsedTemplateArgTypes =
        parseFullInputOrDie templateArgTypeListParser <$> templateArgTypes
  let tys = toClangTemplateArgListTxt parsedTemplateArgTypes

  (convertedSym, conTable) <- do
    fileContent <- readFile irFile
    let table@(SymbolTable tbl) =
          parseOrDie progTableParser $ T.pack fileContent
    let nameWithDemangled =
          fmap (\(k, _) -> (k, demangle1 k)) tbl
    let nameFiltered =
          filter
            ( \(k, r) ->
                NonEmpty.head (fromJust $ functionName r) == irFuncName
                  && tys `T.isInfixOf` demangleResultToText (demangle1 k)
            )
            nameWithDemangled
    convertedSym <- case nameFiltered of
      [(k, _)] -> return k
      [] ->
        error $
          "The function name "
            <> T.unpack irFuncName
            <> T.unpack tys
            <> " is not found in the IR file."
      _ ->
        error $
          "More than one function names "
            <> T.unpack irFuncName
            <> T.unpack tys
            <> " are found in the IR file."
    let conTable =
          scaleLMul (1 % lmulDownscaleRatio) $
            eliminateProgTableDeadCode $
              fromRight undefined $
                filterByReachableSymbols
                  (HS.singleton convertedSym)
                  table
    return (convertedSym, conTable)

  let templateArgList = concat $ maybeToList parsedTemplateArgTypes
  let (allowPartialVL, sketch0) =
        sketchTable extraSketchArgs conTable convertedSym templateArgList
  let sketch = case naiveSplitting of
        Nothing -> sketch0
        Just n -> toFlatSketchSpecTable n sketch0
  let numInsts = case conTable of
        SymbolTable tbl -> sum $ length . Concrete.progStmtList . snd <$> tbl

  logMultiLineDoc logger NOTICE $
    nest 2 $
      vsep
        [ "Start RVV synthesis with arguments: ",
          "Command line: " <> pformat cmdline,
          "Resolved configs: " <> pformat args,
          nest 2 $
            vsep
              [ "scaled MachineConfigs: ",
                pformat (fmap (toMachineConfig useVLMask allowPartialVL) <$> parsedMachineConfigs)
              ],
          "Sketch: " <> pformat sketch,
          "Allow partial VL: " <> pformat allowPartialVL
        ]
  logMultiLineDoc logger NOTICE $
    nest 2 $
      vsep
        [ "Parsed function from the IR"
            <> ( if lmulDownscaleRatio /= 0
                   then
                     " (after "
                       <> pformat lmulDownscaleRatio
                       <> "x downscaling)"
                   else ""
               )
            <> ": ",
          pformat conTable
        ]
  let x280Model =
        PerStmtCostObj $
          scaleLMul
            (1 % lmulDownscaleRatio)
            (x280ApproxModel useFullScalarCost)
  let Right (cost :: Int) =
        symbolCost x280Model conTable convertedSym

  -- let featureSet = extractDefaultConProgTableFeature conTable

  logMultiLineDoc logger NOTICE $
    nest 2 $
      vsep
        [ -- "Features of the parsed function: ",
          -- pformat featureSet,
          "Cost of the parsed function: " <> pformat cost
        ]

  let base =
        MachineBaseConfig
          { machineScalarLength = 64,
            machineMemoryBlockIds = HS.empty,
            machinePointerUnit = 8,
            machineScalarImmLength = 64,
            machineVectorImmLength = 32
          }
  let parsedIRSpecScalingMethod =
        parseFullInputOrDie irSpecScalingMethodParser irSpecScalingMethod
  let solver0 = case parseFullInputOrDie solverParser solverConfig of
        Bitwuzla -> bitwuzla
        Boolector -> boolector
        Z3 -> z3
        Yices ->
          yices
            { sbvConfig =
                (sbvConfig yices)
                  { extraArgs = ["--smt2-model-format"]
                  }
            }
  let solver =
        solver0
          { sbvConfig =
              (sbvConfig solver0)
                { extraArgs =
                    words extraSolverArgs ++ extraArgs (sbvConfig solver0)
                }
          }

  let spec0 ::
        (SemConstraint mode ctx) =>
        MachineConfig ->
        [Value mode] ->
        ctx ([Value mode], RegListMatcher)
      spec0 =
        case (parsedIRSpecScalingMethod, immScalingConfig) of
          (NoScale, Nothing) -> fullMatchSpecFromRefProg conTable convertedSym
          (NoScale, Just config) -> do
            case parse @Void selectorAndNewImmMapParsers "--imm-scaling-config" config of
              Left bundle -> do
                error $
                  "Failed to parse imm scaling config\n"
                    <> errorBundlePretty bundle
              Right selectorsAndNewImm ->
                scaledFullMatchSpecFromRefProg base selectorsAndNewImm conTable convertedSym
          (ZextScale, Nothing) ->
            downscaleSpec'
              False
              ( scaleValue
                  zextUpscaleFun
                  (scaleMachineConfig False fullImm fullImm)
              )
              base
              useVLMask
              allowPartialVL
              $ fullMatchSpecFromRefProg conTable convertedSym
          (SextScale, Nothing) ->
            downscaleSpec'
              False
              ( scaleValue
                  sextUpscaleFun
                  (scaleMachineConfig False fullImm fullImm)
              )
              base
              useVLMask
              allowPartialVL
              $ fullMatchSpecFromRefProg conTable convertedSym
          _ ->
            error
              ( "--imm-scaling-config must be used with "
                  <> "--ir-spec-scaling-method=no"
              )
  let Right inputTypeSignature = symbolType conTable convertedSym

  let combinedVerifierFuzzer ::
        AllowPartialVL ->
        [MachineConfigSpec] ->
        IO
          ( RVVCombinedVerifierFuzzer
              (SymProg (WordN 8) (SymWordN 8))
              (ConProg (WordN 8))
          )
      combinedVerifierFuzzer allowPartialVL vconstSpecs = do
        let vconsts = toMachineConfig useVLMask allowPartialVL <$> vconstSpecs
        return $
          RVVCombinedVerifierFuzzer
            { rvvCombinedVerifierFuzzerSolverConfig = solver,
              rvvCombinedVerifierFuzzerFastVerifierTimeoutSeconds =
                verificationFastTimeout,
              rvvCombinedVerifierFuzzerSlowVerifierTimeoutSeconds =
                verificationSlowTimeout,
              rvvCombinedVerifierFuzzerMachineConfigs = vconsts,
              rvvCombinedVerifierFuzzerVerifierInputs =
                [ \nbit vconst -> do
                    traverse
                      ( \ty -> case nbit of
                          Just nbit -> freshValidValue' nbit vconst ty
                          Nothing -> freshValidValue vconst ty
                      )
                      (toSym $ argTypes inputTypeSignature :: [ValueType])
                ],
              rvvCombinedVerifierFuzzerFuzzerGenerators =
                case overrideGenerators of
                  Just generators -> (\f -> f conTable convertedSym) <$> generators
                  Nothing ->
                    [ \vconst ->
                        ( if doResizeGenerators
                            then resize 30
                            else id
                        )
                          $ randomValidInputsForSymbol vconst conTable convertedSym
                    ],
              rvvCombinedVerifierFuzzerFuzzerSpec =
                case (forceIRSpec, maybeDefaultSpecification) of
                  (False, Just spec) -> spec conTable convertedSym
                  _ -> spec0,
              rvvCombinedVerifierFuzzerVerifierSpec = spec0,
              rvvCombinedVerifierFuzzerFuzzerMaxTests = fuzzerMaxTests,
              rvvCombinedVerifierFuzzerLogger = Nothing,
              rvvCombinedVerifierFuzzerCexReductionBehavior =
                if cexReductionBehavior == "set-to-one" then SetToOne else ZeroOut
            }
  combinedVerifierFuzzers <- traverse (combinedVerifierFuzzer allowPartialVL) parsedMachineConfigs

  -- let inputToExample (vconst, input, matcher) =
  --       let p = runSymbol vconst conTable convertedSym input
  --           Right output = traceShow p p
  --        in Example
  --             vconst
  --             vconst
  --             (Proxy @(Value 'S))
  --             (IOPair input output)
  --             matcher

  let sconfig ::
        ( ProcessConstraint
            (SketchSpec (WordN 8))
            DefaultSymProg
            DefaultConProg
            costObj
            Int
            MachineConfig
            (Value 'S)
            MachineConfig
            (Value 'C)
            RegListMatcher
        ) =>
        costObj ->
        SchedulerConfig
          (SketchSpec (WordN 8))
          DefaultSymProg
          DefaultConProg
          costObj
          Int
          MachineConfig
          (Value 'S)
          MachineConfig
          (Value 'C)
          RegListMatcher
      sconfig costObj =
        SchedulerConfig
          { initialTimeoutSeconds = timeoutSeconds,
            solverConfig = solver,
            logConfig = logConfig,
            logger,
            parallelism,
            restartRunningTimeThresholdSeconds =
              fromMaybe
                (timeoutSeconds `div` 5)
                restartRunningTimeThresholdSeconds,
            successNodeNewTimeoutSeconds =
              fromMaybe (3 * timeoutSeconds) successNodeNewTimeoutSeconds,
            costObj = costObj,
            verifiers = \logger ->
              let setUpLogger v = v {rvvCombinedVerifierFuzzerLogger = Just logger}
               in (: []) . SomeVerifier . setUpLogger <$> combinedVerifierFuzzers,
            synthesisSketchSymbol = convertedSym,
            fastTrackTimeoutSeconds =
              fromMaybe timeoutSeconds fastTrackTimeoutSeconds,
            generalizationSketchFromFastResult =
              if doImmSynth
                then Just toFastSketch
                else Nothing,
            cmdline = Just cmdline,
            transcriptSMT = transcriptSMT,
            doDeadCodeElimination,
            schedulerRandomSeed,
            initialSplitRatio,
            pollIntervalSeconds,
            initialMinimalCost =
              if useExactCost
                then Just (targetCost + 1)
                else
                  if isJust initialCost
                    then initialCost
                    else
                      Just (cost + initialCostOffset),
            targetCost,
            exactCost = if useExactCost then Just targetCost else Nothing,
            schedulerTimeoutSeconds,
            biasedDrawProbability,
            countNumProgsEvidence = Just CountNumProgsEvidence,
            referenceNumInsts = Just numInsts,
            enablePlotting,
            enableTreeStats
          }
  -- let l = fullSplit $ sketchTable extraSketchArgs conTable convertedSym templateArgList
  -- pprint  $ sketchTable extraSketchArgs conTable convertedSym templateArgList
  -- print $ numNodes l
  -- print $ length $ latticeLeafNodes l
  -- undefined
  void $
    runWithScheduler
      (sconfig x280Model)
      [sketch]
  where
    opts =
      info
        ( helper
            <*> parser
              defaultIRFile
              defaultIRFuncName
              defaultTemplateArgType
              defaultIRSpecScalingMethod
              defaultMachineConfigSpecs
              defaultFuzzerMaxTests
              defaultImmScaleConfig
              defaultLMulDownscaleRatio
              extraSketchArgsParser
        )
        fullDesc

newtype UseArbitraryImmArg = UseArbitraryImmArg Bool

instance Show UseArbitraryImmArg where
  show (UseArbitraryImmArg True) = "arbitrary"
  show (UseArbitraryImmArg False) = "controlled"

instance PPrint UseArbitraryImmArg where
  pformat = viaShow

useArbitraryImmArgParser :: Parser UseArbitraryImmArg
useArbitraryImmArgParser =
  UseArbitraryImmArg
    <$> switch
      ( long "arbitrary-imm"
          <> help
            ( "Use arbitrary imm values in the sketch. "
                <> "Default is bounded imm values."
            )
      )
