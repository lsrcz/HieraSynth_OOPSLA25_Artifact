{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Semantics
  ( IfContext (..),
    Equals (..),
    HasSemantics,
    applyPlus,
    applyEquals,
    applyMinus,
    applyIntConst,
    applyIf,
  )
where

import qualified Data.Text as T
import Grisette
  ( Mergeable,
    MonadUnion,
    SymBool,
    SymEq ((.==)),
    SymInteger,
    mrgIf,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (MonadContext)
import HieraSynth.Program.ProgSemantics
  ( EvaledSymbolTable,
    runEvaledSymbol,
  )
import HieraSynth.Util.Show (showAsText)
import Value
  ( ValueBuilder (BoolValType, IntValType, mkBool, mkInt),
    ValueExtractor (getBool, getInt),
  )

intInt2IntOp ::
  (MonadContext ctx, Num i, ValueExtractor val ctx, i ~ IntValType val) =>
  T.Text ->
  (i -> i -> i) ->
  [val] ->
  ctx [val]
intInt2IntOp _ f [a, b] = do
  aInt <- getInt a
  bInt <- getInt b
  mrgReturn [mkInt $ f aInt bInt]
intInt2IntOp opName _ operands =
  mrgThrowError $
    opName <> " cannot accept " <> showAsText (length operands) <> " values"

intInt2BoolOp ::
  ( MonadContext ctx,
    ValueExtractor val ctx,
    i ~ IntValType val,
    b ~ BoolValType val
  ) =>
  T.Text ->
  (i -> i -> b) ->
  [val] ->
  ctx [val]
intInt2BoolOp _ f [a, b] = do
  aInt <- getInt a
  bInt <- getInt b
  mrgReturn [mkBool $ f aInt bInt]
intInt2BoolOp opName _ operands =
  mrgThrowError $
    opName <> " cannot accept " <> showAsText (length operands) <> " values"

class IfContext bool ctx where
  ifC :: (Mergeable val) => bool -> ctx val -> ctx val -> ctx val

instance IfContext Bool ctx where
  ifC True true _ = true
  ifC False _ false = false

instance (MonadUnion ctx) => IfContext SymBool ctx where
  ifC = mrgIf

class Equals bool v where
  equals :: v -> v -> bool

instance Equals Bool Integer where
  equals = (==)

instance Equals SymBool Integer where
  equals = (.==)

instance Equals SymBool SymInteger where
  equals = (.==)

type HasSemantics val ctx =
  ( ValueExtractor val ctx,
    Num (IntValType val),
    Equals (BoolValType val) (IntValType val),
    ValueBuilder val,
    IfContext (BoolValType val) ctx,
    Mergeable (IntValType val),
    Mergeable (BoolValType val),
    SymEq (IntValType val),
    SymEq (BoolValType val),
    Show (IntValType val),
    Show (BoolValType val)
  )

applyPlus :: (HasSemantics val ctx) => [val] -> ctx [val]
applyPlus = intInt2IntOp "plus" (+)

applyEquals :: (HasSemantics val ctx) => [val] -> ctx [val]
applyEquals = intInt2BoolOp "equals" equals

applyMinus :: (HasSemantics val ctx) => [val] -> ctx [val]
applyMinus = intInt2IntOp "minus" (-)

applyIntConst :: (HasSemantics val ctx, i ~ IntValType val) => i -> [val] -> ctx [val]
applyIntConst i [] = mrgReturn [mkInt i]
applyIntConst _ _ = mrgThrowError "const op should accept no operands"

applyIf ::
  (HasSemantics val ctx) =>
  EvaledSymbolTable val ctx ->
  T.Text ->
  T.Text ->
  [val] ->
  ctx [val]
applyIf table symbolTrue symbolFalse (cond : operands) = do
  c <- getBool cond
  ifC
    c
    (runEvaledSymbol table symbolTrue operands)
    (runEvaledSymbol table symbolFalse operands)
applyIf _ _ _ _ =
  mrgThrowError "the first operand to the if op must be a boolean value"
