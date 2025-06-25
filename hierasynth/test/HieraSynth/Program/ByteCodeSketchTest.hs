{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ByteCodeSketchTest (byteCodeSketchTest) where

import HieraSynth.Program.ByteCodeSketch.ProgUtilTest (progUtilTest)
import HieraSynth.Program.ByteCodeSketch.SemanticsTest (semanticsTest)
import HieraSynth.Program.ByteCodeSketch.ToConTest (toConTest)
import HieraSynth.Program.ByteCodeSketch.TypingTest (typingTest)
import Test.Framework (Test, testGroup)

byteCodeSketchTest :: Test
byteCodeSketchTest =
  testGroup
    "HieraSynth.Program.ByteCodeSketch"
    [ toConTest,
      semanticsTest,
      typingTest,
      progUtilTest
    ]
