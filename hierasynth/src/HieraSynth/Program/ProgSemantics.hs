{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ProgSemantics
  ( ProgSemantics (..),
    EvaledSymbolTable (..),
    evalSymbolTable,
    runEvaledSymbol,
    runSymbol,
  )
where

import Data.Bifunctor (Bifunctor (second))
import qualified Data.Text as T
import Grisette (Mergeable)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (MonadContext)
import HieraSynth.Program.ProgUtil (ProgUtil)
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))

class
  (MonadContext ctx, ProgUtil prog) =>
  ProgSemantics semObj prog val ctx
  where
  runProg ::
    semObj ->
    EvaledSymbolTable val ctx ->
    prog ->
    [val] ->
    ctx [val]

newtype EvaledSymbolTable val ctx
  = EvaledSymbolTable [(T.Text, [val] -> ctx [val])]
  deriving newtype (Semigroup, Monoid)

evalSymbolTable ::
  (ProgSemantics semObj prog val ctx) =>
  semObj ->
  SymbolTable prog ->
  EvaledSymbolTable val ctx
evalSymbolTable semObj (SymbolTable table) =
  let res =
        EvaledSymbolTable $
          fmap
            (second $ runProg semObj res)
            table
   in res

runEvaledSymbol ::
  (MonadContext ctx, Mergeable val) =>
  EvaledSymbolTable val ctx ->
  T.Text ->
  [val] ->
  ctx [val]
runEvaledSymbol (EvaledSymbolTable table) symbol inputs = go table
  where
    go [] = mrgThrowError $ "Symbol " <> symbol <> "not found"
    go ((sym, f) : rest) = if sym == symbol then f inputs else go rest

runSymbol ::
  ( MonadContext ctx,
    Mergeable val,
    ProgSemantics sem prog val ctx
  ) =>
  sem ->
  SymbolTable prog ->
  T.Text ->
  [val] ->
  ctx [val]
runSymbol sem table = runEvaledSymbol (evalSymbolTable sem table)
