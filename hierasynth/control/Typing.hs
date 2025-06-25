{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Typing
  ( Type (..),
    typePlus,
    typeEquals,
    typeMinus,
    typeIntConst,
    typeIf,
  )
where

import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    allClasses0,
    derive,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Value (Value, ValueBuilder (mkBool, mkInt))

data Type = IntType | BoolType

derive [''Type] (filter (/= ''PPrint) allClasses0)

instance PPrint Type where
  pformat IntType = "int"
  pformat BoolType = "bool"

typePlus :: (MonadContext ctx) => ctx (TypeSignature Type)
typePlus = mrgReturn $ TypeSignature [IntType, IntType] [IntType]

typeEquals :: (MonadContext ctx) => ctx (TypeSignature Type)
typeEquals = mrgReturn $ TypeSignature [IntType, IntType] [BoolType]

typeMinus :: (MonadContext ctx) => ctx (TypeSignature Type)
typeMinus = mrgReturn $ TypeSignature [IntType, IntType] [IntType]

typeIntConst :: (MonadContext ctx) => ctx (TypeSignature Type)
typeIntConst = mrgReturn $ TypeSignature [] [IntType]

typeIf :: TypeSignature Type -> TypeSignature Type
typeIf (TypeSignature args ress) = TypeSignature (BoolType : args) ress

instance
  ( Mergeable intVal,
    Mergeable boolVal,
    GenSymSimple () intVal,
    GenSymSimple () boolVal
  ) =>
  GenSym Type (Value intVal boolVal)
  where
  fresh IntType = mkInt <$> simpleFresh ()
  fresh BoolType = mkBool <$> simpleFresh ()
