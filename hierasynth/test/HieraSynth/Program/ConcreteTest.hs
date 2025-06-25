module HieraSynth.Program.ConcreteTest (concreteTest) where

import HieraSynth.Program.Concrete.BuilderTest (builderTest)
import HieraSynth.Program.Concrete.EliminateDeadCodeTest
  ( eliminateDeadCodeTest,
  )
import HieraSynth.Program.Concrete.FlattenTest (flattenTest)
import HieraSynth.Program.Concrete.GenSymSimpleTest (genSymSimpleTest)
import HieraSynth.Program.Concrete.MayMultiPathTest (mayMultiPathTest)
import HieraSynth.Program.Concrete.ParserTest (parserTest)
import HieraSynth.Program.Concrete.PartitionTest (partitionTest)
import HieraSynth.Program.Concrete.PrettyTest (prettyTest)
import HieraSynth.Program.Concrete.ProgCostTest (progCostTest)
import HieraSynth.Program.Concrete.ProgUtilTest (progUtilTest)
import HieraSynth.Program.Concrete.SemanticsTest (semanticsTest)
import HieraSynth.Program.Concrete.ToDotTest (toDotTest)
import HieraSynth.Program.Concrete.TypingTest (typingTest)
import Test.Framework (Test, testGroup)

concreteTest :: Test
concreteTest =
  testGroup
    "HieraSynth.Program.Concrete"
    [ prettyTest,
      toDotTest,
      -- topologicalSortTest,
      semanticsTest,
      typingTest,
      mayMultiPathTest,
      builderTest,
      progUtilTest,
      flattenTest,
      progCostTest,
      eliminateDeadCodeTest,
      partitionTest,
      genSymSimpleTest,
      parserTest
    ]
