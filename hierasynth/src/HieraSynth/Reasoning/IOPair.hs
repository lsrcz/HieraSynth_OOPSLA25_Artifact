{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HieraSynth.Reasoning.IOPair (IOPair (..)) where

import Grisette (allClasses01, derive)

data IOPair val = IOPair {ioPairInputs :: [val], ioPairOutputs :: [val]}

derive [''IOPair] allClasses01
