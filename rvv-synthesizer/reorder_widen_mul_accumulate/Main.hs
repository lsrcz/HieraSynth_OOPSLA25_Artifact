{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Criterion (bench, bgroup, nfIO)
import Criterion.Main (defaultMain)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Ratio ((%))
import qualified Data.Text as T
import Grisette
  ( BV (bv),
    PPrint (pformat),
    Solvable (con),
    SymWordN,
    WordN,
    bitwuzla,
    boolector,
    mrgReturn,
    runFresh,
    z3,
  )
import HieraSynth.Context (ConcreteContext)
import HieraSynth.Program.ComponentSketch
  ( MkFreshProg (mkFreshProg),
    simpleFreshStmt,
  )
import qualified HieraSynth.Program.ComponentSketch as Component
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.ProgSemantics (ProgSemantics (runProg))
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.Reasoning.Synthesis
  ( SomeVerifier (SomeVerifier),
    SynthesisResult (SynthesisSuccess),
    SynthesisTask
      ( SynthesisTask,
        synthesisExtraConstraints,
        synthesisInitialExamples,
        synthesisPrecondition,
        synthesisSketchSymbol,
        synthesisSketchTable,
        synthesisVerifiers
      ),
    runSynthesisTask,
  )
import Grisette.Unified (EvalModeTag (C, S))
import RVV.Semantics.Imm (constImm)
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.VType
  ( VConst (VConst),
    VConstBase
      ( VConstBase,
        vconstMemoryBlockIds,
        vconstPtrUnit,
        vconstVImmLen,
        vconstXImmLen,
        vconstXlen
      ),
    VConstConfig
      ( VConstConfig,
        vconstMemoryBlockLengths,
        vconstVlen
      ),
    VType (VType, vtypeEMul, vtypeEewXlenMultiplier),
  )
import RVV.Semantics.Val (VReg (VReg), Vector (Vector))
import RVV.Synthesizer.Destination (pd, ud)
import RVV.Synthesizer.ElementOp
  ( SingleWidthBinOp (Srl),
    SlideDirection (SlideDown, SlideUp),
    WideningMultiplyAddOp (WMAccu),
    mkSlideDown,
    mkSlideUp,
    mkSrl,
    mkWMAccu,
  )
import RVV.Synthesizer.Generator (randomValidVector)
import RVV.Synthesizer.Masking (fm)
import RVV.Synthesizer.Matcher
  ( RegListMatcher (RegListMatcher),
    RegMatcher (MatchFull),
  )
import RVV.Synthesizer.Op
  ( ConOp,
    Op
      ( ScalarBinImm,
        UndefinedVector,
        VGet,
        VLMulExtend,
        VLMulTruncate,
        VLToScalar,
        VSet,
        VSetVLMax,
        VSlideVX,
        WideningMultiplyAddVV
      ),
    SymOp,
  )
import RVV.Synthesizer.Task
  ( RVVQuickCheckFuzzer
      ( RVVQuickCheckFuzzer,
        rvvQuickCheckFuzzerGenerators,
        rvvQuickCheckFuzzerMaxTests,
        rvvQuickCheckFuzzerSpec,
        rvvQuickCheckFuzzerVConsts
      ),
  )
import RVV.Synthesizer.Type (ValueType (VectorType))
import RVV.Synthesizer.Value (Value (VectorValue))
import Test.QuickCheck (Gen)

base :: VConstBase
base =
  VConstBase
    { vconstXlen = 8,
      vconstPtrUnit = 2,
      vconstXImmLen = 5,
      vconstMemoryBlockIds = S.empty,
      vconstVImmLen = 3
    }

config :: VConstConfig
config =
  VConstConfig
    { vconstVlen = 16,
      vconstMemoryBlockLengths = M.empty
    }

vtypeEF2M1 :: VType
vtypeEF2M1 =
  VType
    { vtypeEewXlenMultiplier = 1 % 2,
      vtypeEMul = 1
    }

vtypeE1M1 :: VType
vtypeE1M1 =
  VType
    { vtypeEewXlenMultiplier = 1,
      vtypeEMul = 1
    }

vtypeE1M2 :: VType
vtypeE1M2 =
  VType
    { vtypeEewXlenMultiplier = 1,
      vtypeEMul = 2
    }

type ConProg = Concrete.Prog ConOp (WordN 8) (ValueType 'C)

type Sketch = Component.Prog SymOp (SymWordN 8) (ValueType 'S)

reorderWidenMulAccumulate :: ConProg
reorderWidenMulAccumulate =
  Concrete.buildProg
    [ ("ar", VectorType False vtypeEF2M1),
      ("br", VectorType False vtypeEF2M1),
      ("s0r", VectorType False vtypeE1M1),
      ("s1r", VectorType False vtypeE1M1)
    ]
    $ \[ar, br, s0r, s1r] -> do
      s0s1r <- Concrete.node1 (UndefinedVector vtypeE1M2 False) []
      s0s1r' <-
        Concrete.node1 (VSet vtypeE1M1 vtypeE1M2 pd 0 False) [s0s1r, s0r]
      s0s1r'' <-
        Concrete.node1 (VSet vtypeE1M1 vtypeE1M2 pd 1 False) [s0s1r', s1r]
      vl4 <- Concrete.node1 (VSetVLMax 2 (nonePolicy @'C)) []
      cr <-
        Concrete.node1
          (WideningMultiplyAddVV vtypeE1M2 mkWMAccu fm False)
          [s0s1r'', ar, br, vl4]
      s0ret <- Concrete.node1 (VGet vtypeE1M1 vtypeE1M2 0 False) [cr]
      s1ret <- Concrete.node1 (VGet vtypeE1M1 vtypeE1M2 1 False) [cr]
      return
        [ (s0ret, VectorType False vtypeE1M1),
          (s1ret, VectorType False vtypeE1M1)
        ]

reorderWidenMulAccumulate2 :: ConProg
reorderWidenMulAccumulate2 =
  Concrete.buildProg
    [ ("ar", VectorType False vtypeEF2M1),
      ("br", VectorType False vtypeEF2M1),
      ("s0r", VectorType False vtypeE1M1),
      ("s1r", VectorType False vtypeE1M1)
    ]
    $ \[ar, br, s0r, s1r] -> do
      s0rm2 <-
        Concrete.node1 (VLMulExtend vtypeE1M1 vtypeE1M2 False) [s0r]
      s1rm2 <-
        Concrete.node1 (VLMulExtend vtypeE1M1 vtypeE1M2 False) [s1r]
      vl4 <- Concrete.node1 (VSetVLMax 2 (nonePolicy @'C)) []
      vl4x <- Concrete.node1 (VLToScalar 2) [vl4]
      vl4half <- Concrete.node1 (ScalarBinImm mkSrl 1 (constImm 1)) [vl4x]
      s0s1r <-
        Concrete.node1
          (VSlideVX vtypeE1M2 mkSlideUp pd fm False)
          [s0rm2, s1rm2, vl4half, vl4]
      cr <-
        Concrete.node1
          (WideningMultiplyAddVV vtypeE1M2 mkWMAccu fm False)
          [s0s1r, ar, br, vl4]
      s0ret <-
        Concrete.node1 (VLMulTruncate vtypeE1M1 vtypeE1M2 False) [cr]
      s1m2 <-
        Concrete.node1
          (VSlideVX vtypeE1M2 mkSlideDown ud fm False)
          [cr, vl4half, vl4]
      s1ret <-
        Concrete.node1 (VLMulTruncate vtypeE1M1 vtypeE1M2 False) [s1m2]
      return
        [ (s0ret, VectorType False vtypeE1M1),
          (s1ret, VectorType False vtypeE1M1)
        ]

reorderWidenMulAccumulateSketch :: Sketch
reorderWidenMulAccumulateSketch =
  flip runFresh "sketch" $
    mkFreshProg
      [ VectorType (con False) vtypeEF2M1,
        VectorType (con False) vtypeEF2M1,
        VectorType (con False) vtypeE1M1,
        VectorType (con False) vtypeE1M1
      ]
      [ simpleFreshStmt (mrgReturn $ UndefinedVector vtypeE1M2 (con False)),
        simpleFreshStmt (mrgReturn $ VSet vtypeE1M1 vtypeE1M2 pd 0 (con False)),
        simpleFreshStmt (mrgReturn $ VSet vtypeE1M1 vtypeE1M2 pd 1 (con False)),
        simpleFreshStmt (mrgReturn $ VSetVLMax 2 nonePolicy),
        simpleFreshStmt
          (mrgReturn $ WideningMultiplyAddVV vtypeE1M2 (mrgReturn WMAccu) fm (con False)),
        simpleFreshStmt (mrgReturn $ VGet vtypeE1M1 vtypeE1M2 0 (con False)),
        simpleFreshStmt (mrgReturn $ VGet vtypeE1M1 vtypeE1M2 1 (con False))
      ]
      [VectorType (con False) vtypeE1M1, VectorType (con False) vtypeE1M1]

reorderWidenMulAccumulateSketch2 :: Sketch
reorderWidenMulAccumulateSketch2 =
  flip runFresh "sketch" $
    mkFreshProg
      [ VectorType (con False) vtypeEF2M1,
        VectorType (con False) vtypeEF2M1,
        VectorType (con False) vtypeE1M1,
        VectorType (con False) vtypeE1M1
      ]
      [ simpleFreshStmt
          (mrgReturn $ VLMulExtend vtypeE1M1 vtypeE1M2 (con False)),
        simpleFreshStmt
          (mrgReturn $ VLMulExtend vtypeE1M1 vtypeE1M2 (con False)),
        simpleFreshStmt (mrgReturn $ VSetVLMax 2 nonePolicy),
        simpleFreshStmt (mrgReturn $ VLToScalar 2),
        simpleFreshStmt (mrgReturn $ ScalarBinImm (mrgReturn Srl) 1 (constImm 1)),
        simpleFreshStmt
          (mrgReturn $ VSlideVX vtypeE1M2 (mrgReturn SlideUp) pd fm (con False)),
        simpleFreshStmt
          (mrgReturn $ WideningMultiplyAddVV vtypeE1M2 (mrgReturn WMAccu) fm (con False)),
        simpleFreshStmt
          (mrgReturn $ VLMulTruncate vtypeE1M1 vtypeE1M2 (con False)),
        simpleFreshStmt
          (mrgReturn $ VLMulTruncate vtypeE1M1 vtypeE1M2 (con False)),
        simpleFreshStmt
          (mrgReturn $ VSlideVX vtypeE1M2 (mrgReturn SlideDown) ud fm (con False))
      ]
      [VectorType (con False) vtypeE1M1, VectorType (con False) vtypeE1M1]

spec ::
  VConst -> [Value 'C] -> ConcreteContext ([Value 'C], RegListMatcher)
spec vconst l =
  case runProg vconst mempty reorderWidenMulAccumulate l of
    Left err -> error $ T.unpack err
    Right v -> return (v, RegListMatcher vconst [MatchFull, MatchFull])

gen :: VConst -> Gen [Value 'C]
gen vconst = do
  ar <- randomValidVector vconst vtypeEF2M1
  br <- randomValidVector vconst vtypeEF2M1
  s0r <- randomValidVector vconst vtypeE1M1
  s1r <- randomValidVector vconst vtypeE1M1
  return
    [ VectorValue ar,
      VectorValue br,
      VectorValue s0r,
      VectorValue s1r
    ]

main :: IO ()
main = do
  let ar = VectorValue $ Vector vtypeEF2M1 [VReg (bv 16 0x1234) (bv 16 0)]
  let br = VectorValue $ Vector vtypeEF2M1 [VReg (bv 16 0x5abd) (bv 16 0)]
  let s0r = VectorValue $ Vector vtypeE1M1 [VReg (bv 16 0x4d7a) (bv 16 0)]
  let s1r = VectorValue $ Vector vtypeE1M1 [VReg (bv 16 0) (bv 16 0)]

  print $ pformat reorderWidenMulAccumulate
  print $ pformat reorderWidenMulAccumulate2
  print
    ( runProg
        (VConst base config)
        mempty
        reorderWidenMulAccumulate2
        [ar, br, s0r, s1r] ::
        ConcreteContext [Value 'C]
    )
  print
    ( runProg
        (VConst base config)
        mempty
        reorderWidenMulAccumulate
        [ar, br, s0r, s1r] ::
        ConcreteContext [Value 'C]
    )

  let fuzzer :: RVVQuickCheckFuzzer Sketch ConProg
      fuzzer =
        RVVQuickCheckFuzzer
          { rvvQuickCheckFuzzerVConsts = [VConst base config],
            rvvQuickCheckFuzzerMaxTests = 100,
            rvvQuickCheckFuzzerGenerators = [gen],
            rvvQuickCheckFuzzerSpec = spec
          }

  let task table key =
        SynthesisTask
          { synthesisVerifiers = [SomeVerifier fuzzer],
            synthesisInitialExamples = [],
            synthesisPrecondition = con True,
            synthesisSketchTable = table,
            synthesisSketchSymbol = key,
            synthesisExtraConstraints = const $ return $ con True
          }
  r <-
    runSynthesisTask
      bitwuzla
      (task (SymbolTable [("prog", reorderWidenMulAccumulateSketch)]) "prog")
  case r of
    SynthesisSuccess prog -> do
      print $ pformat (prog :: SymbolTable ConProg)
    _ -> print r
  r <-
    runSynthesisTask
      bitwuzla
      (task (SymbolTable [("prog", reorderWidenMulAccumulateSketch2)]) "prog")
  case r of
    SynthesisSuccess prog -> print $ pformat (prog :: SymbolTable ConProg)
    _ -> print r

  defaultMain $ do
    (sketchName, sketch) <-
      [ ("reorderWidenMulAccumulateSketch", reorderWidenMulAccumulateSketch),
        ("reorderWidenMulAccumulateSketch2", reorderWidenMulAccumulateSketch2)
        ]
    return $ bgroup sketchName $ do
      (configName, config) <-
        [("bitwuzla", bitwuzla), ("boolector", boolector), ("z3", z3)]
      return $ bench configName $ nfIO $ do
        r <- runSynthesisTask config (task (SymbolTable [("prog", sketch)]) "prog")
        case r of
          SynthesisSuccess p -> return (p :: SymbolTable ConProg)
          _ -> error "Should not happen"
