module Main (main) where

import HieraSynth.Operator.OpPPrintTest (opPPrintTest)
import HieraSynth.Operator.OpSemanticsTest (opSemanticsTest)
import HieraSynth.Operator.OpToDotTest (opToDotTest)
import HieraSynth.Operator.OpTypingTest (opTypingTest)
import HieraSynth.Program.ByteCodeSketchTest (byteCodeSketchTest)
import HieraSynth.Program.ChoiceTest (choiceTest)
import HieraSynth.Program.ComponentSketchTest (componentSketchTest)
import HieraSynth.Program.ConcreteTest (concreteTest)
import HieraSynth.Reasoning.FuzzingTest (fuzzingTest)
import HieraSynth.Reasoning.Parallel.Scheduler.DCTreeTest (dcTreeTest)
import HieraSynth.Reasoning.SynthesisTest (synthesisTest)
import HieraSynth.Util.PrettyTest (prettyTest)
import HieraSynth.Util.ShowTest (showTest)
import Test.Framework (Test, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ prettyTest,
    showTest,
    opPPrintTest,
    opToDotTest,
    opTypingTest,
    opSemanticsTest,
    concreteTest,
    byteCodeSketchTest,
    componentSketchTest,
    fuzzingTest,
    synthesisTest,
    dcTreeTest,
    choiceTest
    -- builtinProgConstraintsTest
  ]
