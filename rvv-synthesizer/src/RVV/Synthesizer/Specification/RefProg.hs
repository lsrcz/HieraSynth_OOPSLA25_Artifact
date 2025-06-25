{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module RVV.Synthesizer.Specification.RefProg
  ( specFromRefProg,
    fullMatchSpecFromRefProg,
    -- symSpecFromRefProg,
    -- fullMatchSymSpecFromRefProg,
  )
where

import qualified Data.Text as T
import Grisette (ToSym (toSym), WordN)
import HieraSynth.Program.ProgSemantics (runSymbol)
import HieraSynth.Program.ProgTyping (symbolType)
import HieraSynth.TypeSignature (TypeSignature (resTypes))
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.Matcher
  ( RegListMatcher (RegListMatcher),
    RegMatcher (MatchFull),
  )
import RVV.Synthesizer.Op (ConSymbolTable)
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value)

specFromRefProg ::
  (SemConstraint mode ctx) =>
  ConSymbolTable (WordN 8) ->
  T.Text ->
  RegListMatcher ->
  MachineConfig ->
  [Value mode] ->
  ctx ([Value mode], RegListMatcher)
specFromRefProg table key matchers vconst l =
  (,matchers) <$> runSymbol vconst table key l

fullMatchSpecFromRefProg ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  ConSymbolTable (WordN 8) ->
  T.Text ->
  MachineConfig ->
  [Value mode] ->
  ctx ([Value mode], RegListMatcher)
fullMatchSpecFromRefProg table key vconst =
  case toSym <$> symbolType table key of
    Left err ->
      error $ T.unpack err
    Right (types :: TypeSignature ValueType) ->
      specFromRefProg
        table
        key
        (RegListMatcher vconst $ replicate (length $ resTypes types) MatchFull)
        vconst
