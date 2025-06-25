{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RVV.Synthesizer.SketchGen.ToFastSketch
  ( ToFastSketch (..),
  )
where

import Data.Bifunctor (Bifunctor (second))
import Grisette (ToSym (toSym))
import HieraSynth.Combinator.Invoke (Invoke (Invoke))
import HieraSynth.Combinator.Null (Null)
import HieraSynth.Combinator.Sum (type (:|) (InLeft, InRight))
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree (Leaf))
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))

class ToFastSketch prog sketch where
  toFastSketch :: prog -> sketch

instance ToFastSketch (Null a) (Null b) where
  toFastSketch _ = error "Should not happen"

instance
  (ToFastSketch l0 l1, ToFastSketch r0 r1) =>
  ToFastSketch (l0 :| r0) (l1 :| r1)
  where
  toFastSketch (InLeft l) = InLeft $ toFastSketch l
  toFastSketch (InRight r) = InRight $ toFastSketch r

instance (ToSym ta tb) => ToFastSketch (Invoke ta) (Invoke tb) where
  toFastSketch (Invoke ta name) = Invoke (toSym ta) name

instance
  (ToFastSketch op sketchOp, ToSym conTy symTy) =>
  ToFastSketch
    (Concrete.Prog op varId conTy)
    (Concrete.Prog (ChoiceTree sketchOp) varId symTy)
  where
  toFastSketch (Concrete.Prog args stmts res) =
    Concrete.Prog (toSym args) (go <$> stmts) (toSym res)
    where
      go (Concrete.Stmt op argIds resIds) =
        Concrete.Stmt (Leaf [toFastSketch op]) argIds resIds

instance
  (ToFastSketch prog sketch) =>
  ToFastSketch (SymbolTable prog) (SymbolTable sketch)
  where
  toFastSketch (SymbolTable table) = SymbolTable $ fmap (second toFastSketch) table
