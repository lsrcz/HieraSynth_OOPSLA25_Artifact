{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HieraSynth.Reasoning.Matcher
  ( Matcher (..),
    EqMatcher (..),
  )
where

import GHC.Generics (Generic)
import Grisette (SymBool, SymEq ((.==)), allClasses0, derive)

class Matcher matcher bool a where
  match :: matcher -> [a] -> [a] -> bool

data EqMatcher = EqMatcher
  deriving (Generic)

derive [''EqMatcher] allClasses0

instance (SymEq a) => Matcher EqMatcher SymBool a where
  match _ = (.==)

instance (Eq a) => Matcher EqMatcher Bool a where
  match _ = (==)
