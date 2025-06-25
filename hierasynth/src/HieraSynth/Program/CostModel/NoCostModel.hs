{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HieraSynth.Program.CostModel.NoCostModel (NoCostObj (..)) where

import Grisette (Mergeable, mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.Program.ProgCost (ProgCost (progCost))

data NoCostObj = NoCostObj

instance
  (MonadContext ctx, Mergeable cost, Num cost) =>
  ProgCost NoCostObj prog cost ctx
  where
  progCost _ _ _ = mrgReturn 0
