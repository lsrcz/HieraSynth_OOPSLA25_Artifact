{-# LANGUAGE FlexibleContexts #-}

module RVV.Synthesizer.Operator.Common.NoDescription (noDescription) where

import qualified Data.Text as T
import HieraSynth.Context (ConcreteContext)
import HieraSynth.Operator.OpTyping (OpTyping (typeOp))
import HieraSynth.Program.Concrete (OpPPrintError (PPrintTypingError))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))

noDescription ::
  (OpTyping op ConcreteContext) =>
  op ->
  Either (OpPPrintError varId op) [Maybe T.Text]
noDescription op = case typeOp op of
  Left err -> Left $ PPrintTypingError op err
  Right (TypeSignature argTypes _) -> Right $ Nothing <$ argTypes
