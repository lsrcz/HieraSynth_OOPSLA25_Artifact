{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ConProg (Op (..), Prog) where

-- applyIf,

import qualified Data.HashSet as HS
import Data.List ((\\))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (PPrint (pformat), allClasses01, derive, mrgReturn, pprintClasses)
import HieraSynth.Context (MonadContext)
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics
  ( DefaultSem,
    OpSemantics (applyOp),
  )
import HieraSynth.Operator.OpTyping
  ( OpTyping (OpTypeType, typeOp),
  )
import HieraSynth.Program.Concrete
  ( OpPPrint (describeArguments),
    PrefixByType (prefixByType),
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.TypeSignature (TypeSignature (argTypes))
import HieraSynth.Util.Pretty (parenCommaList)
import Semantics
  ( HasSemantics,
    applyEquals,
    applyIf,
    applyIntConst,
    applyMinus,
    applyPlus,
  )
import Typing (Type, typeEquals, typeIf, typeIntConst, typeMinus, typePlus)
import Value (Value)

data Op intVal
  = Plus
  | Equals
  | Minus
  | IntConst intVal
  | If (TypeSignature Type) T.Text T.Text
  deriving (Generic)

derive [''Op] (allClasses01 \\ pprintClasses)

type Prog varId intVal = Concrete.Prog (Op intVal) varId Type

instance (PPrint intVal) => PPrint (Op intVal) where
  pformat Plus = "plus"
  pformat Equals = "equals"
  pformat Minus = "Minus"
  pformat (IntConst n) = "const" <> parenCommaList [pformat n]
  pformat (If _ true false) =
    "if" <> parenCommaList (pformat <$> [true, false])

instance
  (PPrint intVal, Show intVal) =>
  OpPPrint (Op intVal)
  where
  describeArguments Plus = return $ replicate 2 Nothing
  describeArguments Equals = return $ replicate 2 Nothing
  describeArguments Minus = return $ replicate 2 Nothing
  describeArguments (IntConst _) = return []
  describeArguments (If sig _ _) =
    return $ Just "cond" : (Nothing <$ argTypes sig)

instance PrefixByType Type where
  prefixByType _ = "r"

instance (MonadContext ctx) => OpTyping (Op intVal) ctx where
  type OpTypeType (Op intVal) = Type
  typeOp Plus = typePlus
  typeOp Equals = typeEquals
  typeOp Minus = typeMinus
  typeOp IntConst {} = typeIntConst
  typeOp (If sig _ _) = mrgReturn $ typeIf sig

instance
  (HasSemantics (Value intVal boolVal) ctx) =>
  OpSemantics DefaultSem (Op intVal) (Value intVal boolVal) ctx
  where
  applyOp _ _ Plus = applyPlus
  applyOp _ _ Equals = applyEquals
  applyOp _ _ Minus = applyMinus
  applyOp _ _ (IntConst c) = applyIntConst c
  applyOp _ table (If _ true false) = applyIf table true false

instance OpReachableSymbols (Op intVal) where
  opReachableSymbols (If _ t f) = HS.fromList [t, f]
  opReachableSymbols _ = HS.empty
