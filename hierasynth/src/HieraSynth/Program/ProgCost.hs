{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ProgCost
  ( ProgCost (..),
    SymbolCostTable (..),
    symbolCostTable,
    lookupCost,
    symbolCost,
  )
where

import Data.Bifunctor (Bifunctor (second))
import qualified Data.Text as T
import Grisette (Mergeable)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (MonadContext)
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))

class (MonadContext ctx) => ProgCost costObj prog cost ctx where
  progCost :: costObj -> SymbolCostTable cost ctx -> prog -> ctx cost

newtype SymbolCostTable cost ctx = SymbolCostTable [(T.Text, ctx cost)]
  deriving newtype (Semigroup, Monoid)

symbolCostTable ::
  (ProgCost costObj prog cost ctx) =>
  costObj ->
  SymbolTable prog ->
  SymbolCostTable cost ctx
symbolCostTable costObj (SymbolTable table) =
  let res =
        SymbolCostTable $
          fmap (second (progCost costObj res)) table
   in res

lookupCost ::
  (MonadContext ctx, Mergeable cost) =>
  SymbolCostTable cost ctx ->
  T.Text ->
  ctx cost
lookupCost (SymbolCostTable table) symbol = go table
  where
    go [] = mrgThrowError $ "Symbol " <> symbol <> "not found"
    go ((sym, f) : rest) = if sym == symbol then f else go rest

symbolCost ::
  (MonadContext ctx, Mergeable cost, ProgCost costObj prog cost ctx) =>
  costObj ->
  SymbolTable prog ->
  T.Text ->
  ctx cost
symbolCost costObj table = lookupCost (symbolCostTable costObj table)
