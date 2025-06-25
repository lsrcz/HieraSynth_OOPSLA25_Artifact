module HieraSynth.Reasoning.Synthesis.Problem
  ( addThenDoubleSpec,
    addThenDoubleGen,
    divModTwiceSpec,
    divModTwiceGen,
    addThenDoubleReverseSpec,
    addThenDivModSpec,
    addThenDivModGen,
    times4Spec,
    times4Gen,
  )
where

import HieraSynth.Context (ConcreteContext)
import HieraSynth.Reasoning.Matcher (EqMatcher (EqMatcher))
import HieraSynth.Reasoning.ReverseMatcher
  ( ReverseMatcher (ReverseMatcher),
  )
import Test.QuickCheck.Counterexamples
  ( Arbitrary (arbitrary),
    Gen,
    suchThat,
    vectorOf,
  )

addThenDoubleSpec :: [Integer] -> ConcreteContext ([Integer], EqMatcher)
addThenDoubleSpec [x, y] = return ([x + y, 2 * (x + y)], EqMatcher)
addThenDoubleSpec _ = error "Error"

addThenDoubleReverseSpec ::
  [Integer] -> ConcreteContext ([Integer], ReverseMatcher)
addThenDoubleReverseSpec [x, y] =
  return ([2 * (x + y), x + y], ReverseMatcher)
addThenDoubleReverseSpec _ = error "Error"

addThenDoubleGen :: Gen [Integer]
addThenDoubleGen = vectorOf 2 arbitrary

addThenDivModSpec :: [Integer] -> ConcreteContext ([Integer], EqMatcher)
addThenDivModSpec [x, y, z] =
  return ([(x + y) `div` z, (x + y) `mod` z], EqMatcher)
addThenDivModSpec _ = error "Error"

addThenDivModGen :: Gen [Integer]
addThenDivModGen = flip suchThat noException $ vectorOf 3 arbitrary
  where
    noException [_, _, c] = c /= 0
    noException _ = False

divModTwiceSpec :: [Integer] -> ConcreteContext ([Integer], EqMatcher)
divModTwiceSpec [x, y] =
  let [a, b] = [x `div` y, x `mod` y]
   in return ([a `div` b, a `mod` b], EqMatcher)
divModTwiceSpec _ = error "Error"

divModTwiceGen :: Gen [Integer]
divModTwiceGen = flip suchThat noException $ vectorOf 2 arbitrary
  where
    noException [a, b] = b /= 0 && a `mod` b /= 0
    noException _ = False

times4Spec :: [Integer] -> ConcreteContext ([Integer], EqMatcher)
times4Spec [x] = return ([4 * x], EqMatcher)
times4Spec _ = error "Error"

times4Gen :: Gen [Integer]
times4Gen = vectorOf 1 arbitrary
