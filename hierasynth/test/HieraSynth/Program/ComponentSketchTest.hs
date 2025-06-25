{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ComponentSketchTest
  ( componentSketchTest,
  )
where

import HieraSynth.Program.ComponentSketch.BuilderTest (builderTest)
import HieraSynth.Program.ComponentSketch.GenIntermediateTest
  ( genIntermediateTest,
  )
import HieraSynth.Program.ComponentSketch.ProgCostTest (progCostTest)
import HieraSynth.Program.ComponentSketch.ProgUtilTest (progUtilTest)
import HieraSynth.Program.ComponentSketch.SemanticsTest (semanticsTest)
import HieraSynth.Program.ComponentSketch.ToConTest (toConTest)
import HieraSynth.Program.ComponentSketch.ToSymTest (toSymTest)
import HieraSynth.Program.ComponentSketch.TypingTest (typingTest)
import Test.Framework (Test, testGroup)

componentSketchTest :: Test
componentSketchTest =
  testGroup
    "HieraSynth.Program.ComponentSketch"
    [ genIntermediateTest,
      toConTest,
      toSymTest,
      semanticsTest,
      typingTest,
      builderTest,
      progUtilTest,
      progCostTest
    ]
