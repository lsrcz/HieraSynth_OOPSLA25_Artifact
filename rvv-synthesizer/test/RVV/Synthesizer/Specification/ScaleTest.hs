{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Specification.ScaleTest (scaleTest) where

import Control.Monad.Identity (Identity (Identity))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Grisette (BV (bv), WordN)
import HieraSynth.Context (ConcreteContext)
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.MachineConfig
  ( AllowPartialVL (AllowPartialVL, DisallowPartialVL),
    MachineBaseConfig
      ( MachineBaseConfig,
        machineMemoryBlockIds,
        machinePointerUnit,
        machineScalarImmLength,
        machineScalarLength,
        machineVectorImmLength
      ),
    MachineConfig (MachineConfig, allowPartialVL, baseConfig, scalableConfig, useVLMask),
    MachineScalableConfig
      ( MachineScalableConfig,
        machineMemoryBlockLengths,
        machineVectorLength
      ),
  )
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.Value (Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfig (VectorConfig (VectorConfig))
import RVV.Synthesizer.Generator (randomValidVector)
import RVV.Synthesizer.Matcher
  ( RegListMatcher (RegListMatcher),
    RegMatcher (MatchFull),
  )
import RVV.Synthesizer.Op (ConProg)
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (ImmRHS),
  )
import RVV.Synthesizer.Operator.SetVectorLength (setMaxVectorLength)
import RVV.Synthesizer.Operator.SingleWidthIntBinary (singleWidthIntBinary)
import RVV.Synthesizer.Parameter.Destination (Destination (UseUndefinedDest))
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask))
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (Sra),
  )
import RVV.Synthesizer.Specification.Scale
  ( baseConfigScaleRatio,
    downscaleSpec',
    fullImm,
    scaleMachineConfig,
    scaleValue,
    scaleVector,
    sextUpscaleFun,
    vconstScaleRatio,
  )
import RVV.Synthesizer.Type (ValueType (VectorType))
import RVV.Synthesizer.Value (Value (VectorValue))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Gen, forAll, ioProperty)

baseConfigBasic128Bit :: MachineBaseConfig
baseConfigBasic128Bit =
  MachineBaseConfig
    { machineScalarLength = 64,
      machineMemoryBlockIds = HS.fromList [0, 1],
      machinePointerUnit = 8,
      machineVectorImmLength = 5,
      machineScalarImmLength = 12
    }

vconstBasic128Bit :: Bool -> AllowPartialVL -> MachineConfig
vconstBasic128Bit useVLMask allowPartialVL =
  MachineConfig
    { baseConfig = baseConfigBasic128Bit,
      scalableConfig =
        MachineScalableConfig
          { machineVectorLength = 128,
            machineMemoryBlockLengths = HM.fromList [(0, 512), (1, 1024)]
          },
      useVLMask,
      allowPartialVL
    }

baseConfigBasic128BitHalf :: MachineBaseConfig
baseConfigBasic128BitHalf =
  MachineBaseConfig
    { machineScalarLength = 32,
      machineMemoryBlockIds = HS.fromList [0, 1],
      machinePointerUnit = 4,
      machineVectorImmLength = 5,
      machineScalarImmLength = 12
    }

vconstBasic128BitHalf :: Bool -> AllowPartialVL -> MachineConfig
vconstBasic128BitHalf useVLMask allowPartialVL =
  MachineConfig
    { baseConfig = baseConfigBasic128BitHalf,
      scalableConfig =
        MachineScalableConfig
          { machineVectorLength = 64,
            machineMemoryBlockLengths = HM.fromList [(0, 256), (1, 512)]
          },
      useVLMask,
      allowPartialVL
    }

prog :: ConProg (WordN 8)
prog = Concrete.buildProg
  [("a", VectorType $ VectorConfig (1 / 4) 1)]
  $ \[a] -> do
    vlmax <- Concrete.node1 (setMaxVectorLength @'C 4 nonePolicy) []
    r <-
      Concrete.node1
        ( singleWidthIntBinary @'C
            (VectorConfig (1 / 4) 1)
            (Identity Sra)
            (Identity UseUndefinedDest)
            (Identity UseFullMask)
            (ImmRHS (ConstImm 1))
        )
        [vlmax, a]
    return [(r, VectorType $ VectorConfig (1 / 4) 1)]

gen :: MachineConfig -> Gen [Value 'C]
gen vconst = do
  ar <- randomValidVector vconst $ VectorConfig (1 / 4) 1
  return [VectorValue ar]

spec :: MachineConfig -> [Value 'C] -> ConcreteContext ([Value 'C], RegListMatcher)
spec vconst l = case runProg vconst mempty prog l of
  Left err -> error $ T.unpack err
  Right v -> return (v, RegListMatcher vconst [MatchFull])

scaleTest :: Test
scaleTest =
  testGroup
    "Scale"
    [ testCase "baseConfigScaleRatio" $ do
        baseConfigScaleRatio True baseConfigBasic128BitHalf baseConfigBasic128Bit
          @?= Right (1 / 2)
        baseConfigScaleRatio True baseConfigBasic128Bit baseConfigBasic128BitHalf
          @?= Right 2,
      testCase "vconstScaleRatio" $ do
        vconstScaleRatio True (vconstBasic128BitHalf False AllowPartialVL) (vconstBasic128Bit False AllowPartialVL) @?= Right (1 / 2)
        vconstScaleRatio True (vconstBasic128Bit False DisallowPartialVL) (vconstBasic128BitHalf False DisallowPartialVL) @?= Right 2,
      testCase "scaleMachineConfig" $ do
        scaleMachineConfig True fullImm fullImm 2 (vconstBasic128BitHalf False AllowPartialVL)
          @?= Right
            (vconstBasic128Bit False AllowPartialVL)
              { baseConfig =
                  baseConfigBasic128Bit
                    { machineVectorImmLength = 64,
                      machineScalarImmLength = 64
                    }
              }
        scaleMachineConfig True fullImm fullImm (1 / 2) (vconstBasic128Bit True DisallowPartialVL)
          @?= Right
            (vconstBasic128BitHalf True DisallowPartialVL)
              { baseConfig =
                  baseConfigBasic128BitHalf
                    { machineVectorImmLength = 32,
                      machineScalarImmLength = 32
                    }
              },
      testGroup "scaleVector" $ do
        let doScale =
              scaleVector @'C @ConcreteContext
                sextUpscaleFun
                (scaleMachineConfig True fullImm fullImm)
        let halfVector =
              Vector
                (VectorConfig (1 / 4) 1)
                [VectorReg (bv 64 0x123456789abcdef0) (bv 64 0)]
        let fullVector =
              Vector
                (VectorConfig (1 / 4) 1)
                [ VectorReg
                    (bv 128 0x0012003400560078ff9affbcffdefff0)
                    (bv 128 0)
                ]
        [ testCase "upscale" $ do
            doScale 2 (vconstBasic128BitHalf False AllowPartialVL) halfVector @?= Right fullVector,
          testCase "downscale" $ do
            doScale (1 / 2) (vconstBasic128Bit True DisallowPartialVL) fullVector @?= Right halfVector
          ],
      testProperty "downscaleSpec" $
        forAll (gen (vconstBasic128BitHalf False AllowPartialVL)) $ \i -> ioProperty $ do
          let newSpec =
                downscaleSpec'
                  True
                  (scaleValue sextUpscaleFun (scaleMachineConfig True fullImm fullImm))
                  baseConfigBasic128Bit
                  False
                  AllowPartialVL
                  spec
          let r = newSpec (vconstBasic128BitHalf False AllowPartialVL) i
          (fst <$> spec (vconstBasic128BitHalf False AllowPartialVL) i) @?= (fst <$> r)
    ]
