module HieraSynth.Program.ChoiceTest (choiceTest) where

import HieraSynth.Program.Choice.ChoiceTreeTest (choiceTreeTest)
import HieraSynth.Program.Choice.ComponentBagTest (componentBagTest)
import Test.Framework (Test, testGroup)

choiceTest :: Test
choiceTest = testGroup "Choice" [choiceTreeTest, componentBagTest]
