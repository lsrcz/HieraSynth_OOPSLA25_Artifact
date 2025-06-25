{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Arith (ConProg, Sketch)
import Data.GraphViz.Printing (PrintDot (toDot), renderDot)
import qualified Data.Text.Lazy as TL
import EvalMode (MonadEvalContext)
import Grisette
  ( GenSymSimple (simpleFresh),
    PPrint (pformat),
    SimpleListSpec (SimpleListSpec),
    Solvable (con),
    SymInteger,
    mrgReturn,
    z3,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (ConcreteContext)
import HieraSynth.Operator.OpSemantics (DefaultSem (DefaultSem))
import HieraSynth.Operator.OpTyping (DefaultType (DefaultType))
import qualified HieraSynth.Program.ComponentSketch as Component
import HieraSynth.Program.ProgSemantics (runSymbol)
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.Reasoning.Fuzzing
  ( defaultSemQuickCheckFuzzer,
  )
import HieraSynth.Reasoning.Synthesis
  ( SynthesisResult (SynthesisSuccess),
    SynthesisTask
      ( SynthesisTask,
        synthesisExtraConstraints,
        synthesisInitialExamples,
        synthesisPrecondition,
        synthesisSketchSymbol,
        synthesisSketchTable,
        synthesisVerifiers
      ),
    runSynthesisTask,
  )
import HieraSynth.Reasoning.Verification (defaultSemSMTVerifier)
import Grisette.Unified (EvalModeTag (C, S), GetInteger)
import Operator.Add (add)
import Operator.AddImm (addImm)
import Operator.Mul (mul)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)

sketchTable :: SymbolTable Sketch
sketchTable =
  SymbolTable
    [ ( "prog",
        Component.mkSimpleSketch
          "prog"
          [DefaultType, DefaultType]
          [add, mul, addImm @'S "a"]
          [DefaultType]
      )
    ]

spec ::
  (MonadEvalContext mode ctx) => [GetInteger mode] -> ctx [GetInteger mode]
spec [a, b] = mrgReturn [a * (a + b) + 2]
spec _ = mrgThrowError "Invalid inputs"

gen :: Gen [Integer]
gen = vectorOf 2 arbitrary

main :: IO ()
main = do
  r <-
    runSynthesisTask z3 $
      SynthesisTask
        { synthesisVerifiers =
            [ defaultSemQuickCheckFuzzer @SymInteger gen (spec @'C),
              defaultSemSMTVerifier @Integer
                z3
                Nothing
                [simpleFresh (SimpleListSpec 2 ())]
                (spec @'S)
            ],
          synthesisInitialExamples = [],
          synthesisSketchTable = sketchTable,
          synthesisSketchSymbol = "prog",
          synthesisPrecondition = con True,
          synthesisExtraConstraints = const $ return $ con True
        }
  case r of
    SynthesisSuccess (table :: SymbolTable ConProg) -> do
      print $ pformat table
      writeFile "/tmp/arith.dot" $ TL.unpack $ renderDot $ toDot table
      let input = [5, 20]
      print $ spec @'C @ConcreteContext input
      print
        (runSymbol DefaultSem table "prog" input :: ConcreteContext [Integer])
    _ -> print r
