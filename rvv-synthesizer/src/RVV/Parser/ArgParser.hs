{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.Parser.ArgParser
  ( ratioParser,
    widthMulParser,
    maskMulParser,
    lengthMulParser,
    vectorConfigParser,
    tailPolicyParser,
    maskPolicyParser,
    policyParser,
    destinationParser,
    maskingParser,
    boolParser,
    hexadecimalParser,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor.Identity (Identity (Identity))
import Data.Ratio (Ratio)
import HieraSynth.Util.Parser (CharParser, symbol)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Multiplier
  ( LengthMul (LengthMul),
    MaskMul (MaskMul),
    WidthMul (WidthMul),
  )
import RVV.Semantics.Policy
  ( MaskPolicy (MaskAgnostic, MaskUndisturbed),
    Policy (Policy),
    TailPolicy (TailAgnostic, TailUndisturbed),
  )
import RVV.Semantics.VectorConfig (VectorConfig (VectorConfig))
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest, UseUndefinedDest),
  )
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask, UseProvidedMask))
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L

boolParser :: (CharParser e s m) => m Bool
boolParser =
  ((symbol "True" <|> string "true") >> return True)
    <|> ((symbol "False" <|> string "false") >> return False)

hexadecimalParser :: (Num a, CharParser e s m) => m a
hexadecimalParser = char '0' >> char 'x' >> L.hexadecimal

ratioParser :: (CharParser e s m) => m (Ratio Int)
ratioParser = (string "f" >> fmap (1 /) L.decimal) <|> L.decimal

widthMulParser :: (CharParser e s m) => m WidthMul
widthMulParser = WidthMul <$> ratioParser

maskMulParser :: (CharParser e s m) => m MaskMul
maskMulParser = MaskMul <$> ratioParser

lengthMulParser :: (CharParser e s m) => m LengthMul
lengthMulParser = LengthMul <$> ratioParser

vectorConfigParser :: (CharParser e s m) => m VectorConfig
vectorConfigParser = do
  string "e"
  e <- widthMulParser
  string "m"
  VectorConfig e <$> lengthMulParser

tailPolicyParser :: (CharParser e s m) => m (Identity TailPolicy)
tailPolicyParser =
  Identity <$> do
    (string "ta" >> return TailAgnostic) <|> (string "tu" >> return TailUndisturbed)

maskPolicyParser :: (CharParser e s m) => m (Identity MaskPolicy)
maskPolicyParser =
  Identity <$> do
    (string "ma" >> return MaskAgnostic) <|> (string "mu" >> return MaskUndisturbed)

policyParser :: (CharParser e s m) => m (Policy 'C)
policyParser = do
  tailPolicy <- tailPolicyParser
  Policy tailPolicy <$> maskPolicyParser

destinationParser :: (CharParser e s m) => m (Identity Destination)
destinationParser =
  Identity <$> do
    (symbol "ud" >> return UseUndefinedDest)
      <|> (symbol "pd" >> return UseProvidedDest)

maskingParser :: (CharParser e s m) => m (Identity Masking)
maskingParser =
  Identity <$> do
    (symbol "fm" >> return UseFullMask)
      <|> (symbol "pm" >> return UseProvidedMask)
