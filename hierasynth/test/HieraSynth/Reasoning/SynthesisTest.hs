module HieraSynth.Reasoning.SynthesisTest (synthesisTest) where

import HieraSynth.Reasoning.Synthesis.ByteCodeSketchTest
  ( byteCodeSketchTest,
  )
import HieraSynth.Reasoning.Synthesis.ComponentSketchTest
  ( componentSketchTest,
  )
import Test.Framework (Test, testGroup)

synthesisTest :: Test
synthesisTest =
  testGroup
    "HieraSynth.Reasoning.Synthesis"
    [ byteCodeSketchTest,
      componentSketchTest
    ]
