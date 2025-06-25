{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module RVV.Synthesizer.Feature.ExtractFeature
  ( ExtractFeature (..),
    extractProgTableFeature,
  )
where

import HieraSynth.Combinator.Invoke (Invoke (Invoke))
import HieraSynth.Combinator.Null (Null)
import HieraSynth.Combinator.Sum (type (:|) (InLeft, InRight))
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))

class (Monoid featureSet) => ExtractFeature ty featureSet where
  extractFeature :: ty -> featureSet

instance (Monoid featureSet) => ExtractFeature (Null ty) featureSet where
  extractFeature _ = error "Should not happen"

instance
  (ExtractFeature ty featureSet) =>
  ExtractFeature (Invoke ty) featureSet
  where
  extractFeature (Invoke (TypeSignature argTypes resTypes) _) =
    mconcat $ map extractFeature (argTypes ++ resTypes)

instance
  (ExtractFeature a featureSet, ExtractFeature b featureSet) =>
  ExtractFeature (a :| b) featureSet
  where
  extractFeature (InLeft a) = extractFeature a
  extractFeature (InRight b) = extractFeature b

instance
  (ExtractFeature op featureSet, ExtractFeature ty featureSet) =>
  ExtractFeature (Concrete.Prog op varId ty) featureSet
  where
  extractFeature (Concrete.Prog args stmts ress) =
    mconcat $
      map (extractFeature . Concrete.stmtOp) stmts
        ++ map (extractFeature . Concrete.progArgType) args
        ++ map (extractFeature . Concrete.progResType) ress

extractProgTableFeature ::
  (ExtractFeature prog featureSet) => SymbolTable prog -> featureSet
extractProgTableFeature (SymbolTable table) =
  mconcat $ map (extractFeature . snd) table
