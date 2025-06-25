{-# LANGUAGE TypeOperators #-}

module RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (..)) where

import Data.Bifunctor (Bifunctor (first), second)
import Data.Ratio (Ratio)
import HieraSynth.Combinator.Invoke (Invoke (Invoke))
import HieraSynth.Combinator.Null (Null)
import HieraSynth.Combinator.Sum (type (:|) (InLeft, InRight))
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree (Branch, Leaf))
import HieraSynth.Program.Choice.ComponentBag (ComponentBag (ComponentBag))
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))

class ScaleLMul a where
  scaleLMul :: Ratio Int -> a -> a

instance ScaleLMul (Null ty) where
  scaleLMul _ _ = error "Should not happen"

instance (ScaleLMul l, ScaleLMul r) => ScaleLMul (l :| r) where
  scaleLMul ratio (InLeft l) = InLeft $ scaleLMul ratio l
  scaleLMul ratio (InRight r) = InRight $ scaleLMul ratio r

instance (ScaleLMul ty) => ScaleLMul (Invoke ty) where
  scaleLMul ratio (Invoke ty name) = Invoke (scaleLMul ratio ty) name

instance (ScaleLMul prog) => ScaleLMul (SymbolTable prog) where
  scaleLMul ratio (SymbolTable table) =
    SymbolTable $ fmap (second $ scaleLMul ratio) table

instance (ScaleLMul ty) => ScaleLMul (TypeSignature ty) where
  scaleLMul ratio (TypeSignature a r) =
    TypeSignature (scaleLMul ratio <$> a) (scaleLMul ratio <$> r)

instance (ScaleLMul ty, ScaleLMul op) => ScaleLMul (Concrete.Prog op varId ty) where
  scaleLMul ratio (Concrete.Prog args stmts ress) =
    Concrete.Prog (scaleArg <$> args) (scaleStmt <$> stmts) (scaleRes <$> ress)
    where
      scaleArg arg@(Concrete.ProgArg _ _ ty) =
        arg {Concrete.progArgType = scaleLMul ratio ty}
      scaleRes res@(Concrete.ProgRes _ ty) =
        res {Concrete.progResType = scaleLMul ratio ty}
      scaleStmt
        stmt@(Concrete.Stmt op _ _) =
          stmt {Concrete.stmtOp = scaleLMul ratio op}

instance (ScaleLMul op) => ScaleLMul (ChoiceTree op) where
  scaleLMul ratio (Leaf l) = Leaf $ scaleLMul ratio <$> l
  scaleLMul ratio (Branch meta l) = Branch meta $ scaleLMul ratio <$> l

instance (ScaleLMul op, ScaleLMul ty) => ScaleLMul (ComponentBag op ty) where
  scaleLMul ratio (ComponentBag argTypes components resTypes) =
    ComponentBag
      (scaleLMul ratio <$> argTypes)
      (first (scaleLMul ratio) <$> components)
      (scaleLMul ratio <$> resTypes)
