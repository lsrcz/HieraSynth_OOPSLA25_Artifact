{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HieraSynth.Reasoning.ReverseMatcher (ReverseMatcher (..)) where

import Control.DeepSeq (NFData (rnf))
import Grisette (SymBool, SymEq ((.==)))
import HieraSynth.Reasoning.Matcher (Matcher (match))

data ReverseMatcher = ReverseMatcher deriving (Eq)

instance (Eq a) => Matcher ReverseMatcher Bool a where
  match _ actual expected = actual == reverse expected

instance (SymEq a) => Matcher ReverseMatcher SymBool a where
  match _ actual expected = actual .== reverse expected

instance NFData ReverseMatcher where
  rnf ReverseMatcher = ()
