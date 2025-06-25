{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.App.Solver (Solver (..), solverParser) where

import Control.Applicative (Alternative ((<|>)))
import Grisette (PPrint (pformat), viaShow)
import HieraSynth.Util.Parser (CharParser, symbol)

data Solver = Bitwuzla | Boolector | Z3 | Yices

instance Show Solver where
  show Bitwuzla = "bitwuzla"
  show Boolector = "boolector"
  show Z3 = "z3"
  show Yices = "yices"

instance PPrint Solver where
  pformat = viaShow

solverParser :: (CharParser e s m) => m Solver
solverParser =
  (symbol "bitwuzla" >> return Bitwuzla)
    <|> (symbol "boolector" >> return Boolector)
    <|> (symbol "z3" >> return Z3)
    <|> (symbol "yices" >> return Yices)
