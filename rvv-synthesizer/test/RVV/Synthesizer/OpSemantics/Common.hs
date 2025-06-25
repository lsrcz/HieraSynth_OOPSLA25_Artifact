{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Synthesizer.OpSemantics.Common
  ( SemanticsTest (..),
    semanticsTest,
    TypingTest (..),
    typingTest,
  )
where

import Data.Either (isLeft)
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping
  ( OpTyping (typeOp),
  )
import HieraSynth.TypeSignature (TypeSignature)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Element (vlValueToVLMask)
import RVV.Semantics.MachineConfig (MachineConfig (useVLMask))
import RVV.Semantics.Value (VL (VL, vlMaskMul, vlValue))
import RVV.Synthesizer.Op (ConOp)
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value (VLValue))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.HUnit.Base (assertBool)

data SemanticsTest = SemanticsTest
  { semanticsTestName :: String,
    semanticsTestMachineConfig :: MachineConfig,
    semanticsTestOp :: ConOp,
    semanticsTestInput :: [Value 'C],
    semanticsTestExpected :: Maybe [Value 'C]
  }

valueToVLMask :: MachineConfig -> Value 'C -> Value 'C
valueToVLMask machine (VLValue vl@VL {..}) =
  VLValue $ vl {vlValue = vlValueToVLMask machine vlMaskMul vlValue}
valueToVLMask _ v = v

semanticsTest :: SemanticsTest -> Test
semanticsTest (SemanticsTest name vconst op input expectedMaybe) =
  testGroup
    name
    [ testCase "Original" $ do
        let actual = applyOp vconst mempty op input
        case expectedMaybe of
          Just expected -> actual @?= Right expected
          Nothing -> assertBool "Should fail" $ isLeft actual,
      testCase "Use VLMask" $ do
        let vconst' = vconst {useVLMask = True}
        let actual =
              applyOp vconst' mempty op $
                map (valueToVLMask vconst') input
        case expectedMaybe of
          Just expected -> actual @?= Right (valueToVLMask vconst' <$> expected)
          Nothing -> assertBool "Should fail" $ isLeft actual
    ]

data TypingTest = TypingTest
  { typingTestName :: String,
    typingTestOp :: ConOp,
    typingTestExpected :: TypeSignature ValueType
  }

typingTest :: TypingTest -> Test
typingTest (TypingTest name op expected) =
  testCase name $ do
    let actual = typeOp op
    actual @?= Right expected
