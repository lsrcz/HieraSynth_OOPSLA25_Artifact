{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Value
  ( Value (..),
    ValueBuilder (..),
    ValueExtractor (..),
    SymValue,
  )
where

import Grisette
  ( Mergeable,
    MonadUnion,
    Union,
    allClasses0,
    derive,
    liftToMonadUnion,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (MonadContext)

data Value intVal boolVal
  = IntValue intVal
  | BoolValue boolVal

derive [''Value] allClasses0

class (Mergeable val) => ValueBuilder val where
  type IntValType val
  type BoolValType val
  mkInt :: IntValType val -> val
  mkBool :: BoolValType val -> val

class (MonadContext ctx, ValueBuilder val) => ValueExtractor val ctx where
  getInt :: val -> ctx (IntValType val)
  getBool :: val -> ctx (BoolValType val)

instance
  (MonadContext ctx, Mergeable intVal, Mergeable boolVal) =>
  ValueExtractor (Value intVal boolVal) ctx
  where
  getInt (IntValue i) = mrgReturn i
  getInt _ = mrgThrowError "Not an integer"
  getBool (BoolValue b) = mrgReturn b
  getBool _ = mrgThrowError "Not a boolean"

instance
  (Mergeable intVal, Mergeable boolVal) =>
  ValueBuilder (Value intVal boolVal)
  where
  type IntValType (Value intVal boolVal) = intVal
  type BoolValType (Value intVal boolVal) = boolVal
  mkInt = IntValue
  mkBool = BoolValue

type SymValue intVal boolVal = Union (Value intVal boolVal)

instance
  (Mergeable intVal, Mergeable boolVal) =>
  ValueBuilder (SymValue intVal boolVal)
  where
  type IntValType (SymValue intVal boolVal) = intVal
  type BoolValType (SymValue intVal boolVal) = boolVal
  mkInt = mrgReturn . IntValue
  mkBool = mrgReturn . BoolValue

instance
  (MonadContext ctx, MonadUnion ctx, Mergeable intVal, Mergeable boolVal) =>
  ValueExtractor (SymValue intVal boolVal) ctx
  where
  getInt x = liftToMonadUnion x >>= getInt
  getBool x = liftToMonadUnion x >>= getBool
