{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Arith (OpCode (Minus, Mul, Plus))
import Grisette
  ( GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    SimpleListSpec (SimpleListSpec),
    Solvable (con),
    SymInteger,
    mrgReturn,
    z3,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (ConcreteContext, MonadContext)
import HieraSynth.Operator.OpSemantics (DefaultSem (DefaultSem))
import HieraSynth.Operator.OpTyping (DefaultType (DefaultType))
import qualified HieraSynth.Program.ComponentSketch as Component
import qualified HieraSynth.Program.Concrete as Concrete
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
import Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)

type ConProg = Concrete.Prog OpCode Integer DefaultType

type Sketch = Component.Prog OpCode SymInteger DefaultType

-- The sketch is currently all manually constructed for the purpose of showing
-- the structure of a sketch. The symbolic constants are assigned with unique
-- names manually (e.g. @stmt1'arg0@).
--
-- A generator for a sketch with components is under development.
sketchTable :: SymbolTable Sketch
sketchTable =
  SymbolTable
    [ ( "prog",
        Component.mkSimpleSketch
          -- The name of the program. If your program supports procedure calls, you
          -- should make sure that the programs has a unique name.
          "prog"
          -- The types of the arguments to the program.
          [DefaultType, DefaultType]
          -- The components of the program. Each component is a statement.
          --
          -- The synthesizer could
          -- \* reorder the components, and
          -- \* choose whether or now to disable a component, and
          -- \* choose the arguments of a component.
          [Minus, Mul, Plus]
          -- The program result type.
          [DefaultType]
      )
    ]

-- The specification specifies the expected behavior. Here, we want to synthesis
-- a program that computes the expression a * (a + b) - b.
spec :: (Num a, Mergeable a, MonadContext ctx) => [a] -> ctx [a]
spec [a, b] = mrgReturn [a * (a + b) - b]
spec _ = mrgThrowError "Invalid inputs"

-- The generator generates concrete inputs to fuzz the synthesized program
-- against the specification.
gen :: Gen [Integer]
gen = vectorOf 2 arbitrary

main :: IO ()
main = do
  r <-
    runSynthesisTask z3 $
      SynthesisTask
        { synthesisVerifiers =
            [ defaultSemQuickCheckFuzzer @SymInteger gen (spec @Integer),
              defaultSemSMTVerifier @Integer
                z3
                Nothing
                [simpleFresh (SimpleListSpec 2 ())]
                (spec @SymInteger)
            ],
          synthesisInitialExamples = [],
          synthesisSketchTable = sketchTable,
          synthesisSketchSymbol = "prog",
          synthesisPrecondition = con True,
          synthesisExtraConstraints = const $ return $ con True
        }
  case r of
    SynthesisSuccess (table :: SymbolTable ConProg) -> do
      -- def test(x: int, y: int):
      --   r2 = plus(lhs=y, rhs=x)
      --   r3 = mul(lhs=r2, rhs=x)
      --   r4 = minus(lhs=r3, rhs=y)
      --   return r4
      print $ pformat table
      -- writeFile "/tmp/arith.dot" $ TL.unpack $ renderDot $ toDot prog
      let input = [5, 20]
      print $ spec @Integer @ConcreteContext input
      print (runSymbol DefaultSem table "prog" input :: ConcreteContext [Integer])
    _ -> print r
