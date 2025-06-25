{-# LANGUAGE DataKinds #-}

module Spec
  ( spec,
    gen,
  )
where

import Concrete (lt128HighwayAsm, lt128Symbol)
import qualified Data.Text as T
import Grisette (BV (bv))
import HieraSynth.Program.ProgSemantics (runSymbol)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.MachineConfig
  ( MachineConfig (scalableConfig),
    MachineScalableConfig (machineVectorLength),
  )
import RVV.Synthesizer.Generator (randomValidVector)
import RVV.Synthesizer.Matcher
  ( RegListMatcher (RegListMatcher),
    RegMatcher (MatchMasked),
  )
import RVV.Synthesizer.Value (Value (VectorValue))
import Test.QuickCheck (Gen)
import Types (vtypeE1M1)

spec ::
  MachineConfig -> [Value 'C] -> ([Value 'C], RegListMatcher)
spec vconst l =
  case runSymbol vconst lt128HighwayAsm lt128Symbol l of
    Left err -> error $ T.unpack err
    Right v ->
      ( v,
        RegListMatcher
          vconst
          [MatchMasked (bv (machineVectorLength $ scalableConfig vconst) 0xf)]
      )

gen :: MachineConfig -> Gen [Value 'C]
gen vconst = do
  ar <- randomValidVector vconst vtypeE1M1
  br <- randomValidVector vconst vtypeE1M1
  return [VectorValue ar, VectorValue br]
