{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (..),
    TestSemanticsObj (..),
    TestSemanticsType (..),
    TestSemanticsCost (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq (NFData (rnf))
import Control.Exception (ArithException)
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import GHC.Generics (Generic)
import Grisette
  ( GenSym,
    GenSymSimple (simpleFresh),
    LogicalOp (false),
    MonadUnion,
    SafeDiv (safeDivMod),
    SymInteger,
    Union,
    allClasses0,
    derive,
    liftToMonadUnion,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (MonadContext)
import HieraSynth.Operator.OpParser (OpParser (opParser))
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping
  ( OpTyping (OpTypeType, typeOp),
    simpleTyping,
  )
import HieraSynth.Program.ComponentSketch
  ( GenIntermediate (genIntermediate),
  )
import HieraSynth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import HieraSynth.Program.Concrete
  ( OpPPrint (describeArguments, pformatOp, prefixResults),
  )
import HieraSynth.Program.Concrete.Flatten
  ( OpFlatten (opForwardedSubProg),
  )
import HieraSynth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import HieraSynth.Type.TypeParser (TypeParser (typeParser))
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import HieraSynth.Util.Parser (symbol)
import HieraSynth.Util.Show (showAsText)
import Text.Megaparsec (MonadParsec (try))

data TestSemanticsOp = Add | DivMod | Inc | Double deriving (Generic)

derive [''TestSemanticsOp] allClasses0

instance OpPPrint TestSemanticsOp where
  prefixResults _ = return []
  describeArguments _ = return []
  pformatOp Add = "add"
  pformatOp DivMod = "divmod"
  pformatOp Inc = "inc"
  pformatOp Double = "double"

instance OpParser TestSemanticsOp where
  opParser =
    try (symbol "add" >> return Add)
      <|> try (symbol "divmod" >> return DivMod)
      <|> try (symbol "inc" >> return Inc)
      <|> try (symbol "double" >> return Double)

instance GenSymSimple TestSemanticsOp TestSemanticsOp where
  simpleFresh = return

instance GenSym TestSemanticsOp TestSemanticsOp

instance OpReachableSymbols TestSemanticsOp where
  opReachableSymbols _ = mempty

instance OpSymmetryReduction TestSemanticsOp where
  opUnreorderable _ _ = false
  opCommutativeArgPos Add = mrgReturn [[0, 1]]
  opCommutativeArgPos _ = mrgReturn []

instance OpFlatten TestSemanticsOp TestSemanticsOp where
  opForwardedSubProg op = return $ Right op

data TestSemanticsObj = TestSemanticsObj deriving (Eq)

instance NFData TestSemanticsObj where
  rnf TestSemanticsObj = ()

data TestSemanticsType = IntType deriving (Generic)

derive [''TestSemanticsType] allClasses0

instance TypeParser TestSemanticsType where
  typeParser = try (symbol "IntType") >> return IntType

instance GenSymSimple TestSemanticsType TestSemanticsType where
  simpleFresh = return

instance GenSym TestSemanticsType TestSemanticsType

instance
  (MonadContext ctx) =>
  OpSemantics TestSemanticsObj TestSemanticsOp Integer ctx
  where
  applyOp _ _ Add [x, y] = return [x + y]
  applyOp _ _ Add l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showAsText (length l)
        <> " arguments."
  applyOp _ _ DivMod [x, y] = do
    when (y == 0) $ mrgThrowError "ArithException: divide by zero"
    return [x `div` y, x `mod` y]
  applyOp _ _ DivMod l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showAsText (length l)
        <> " arguments."
  applyOp _ _ Inc [x] = return [x + 1]
  applyOp _ _ Inc l =
    mrgThrowError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showAsText (length l)
        <> " arguments."
  applyOp _ _ Double [x] = return [x + x]
  applyOp _ _ Double l =
    mrgThrowError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showAsText (length l)
        <> " arguments."

instance
  (MonadContext ctx, MonadUnion ctx) =>
  OpSemantics TestSemanticsObj TestSemanticsOp SymInteger ctx
  where
  applyOp _ _ Add [x, y] = return [x + y]
  applyOp _ _ Add l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showAsText (length l)
        <> " arguments."
  applyOp _ _ DivMod [x, y] = do
    r <- liftToMonadUnion (runExceptT @ArithException @Union $ safeDivMod x y)
    case r of
      Left (e :: ArithException) -> mrgThrowError $ "ArithException: " <> showAsText e
      Right (d, m) -> mrgReturn [d, m]
  applyOp _ _ DivMod l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showAsText (length l)
        <> " arguments."
  applyOp _ _ Inc [x] = return [x + 1]
  applyOp _ _ Inc l =
    mrgThrowError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showAsText (length l)
        <> " arguments."
  applyOp _ _ Double [x] = return [x + x]
  applyOp _ _ Double l =
    mrgThrowError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showAsText (length l)
        <> " arguments."

instance (MonadContext ctx) => OpTyping TestSemanticsOp ctx where
  type OpTypeType TestSemanticsOp = TestSemanticsType
  typeOp = simpleTyping $ \case
    Add -> TypeSignature [IntType, IntType] [IntType]
    DivMod -> TypeSignature [IntType, IntType] [IntType, IntType]
    Inc -> TypeSignature [IntType] [IntType]
    Double -> TypeSignature [IntType] [IntType]

instance GenIntermediate TestSemanticsObj TestSemanticsType SymInteger where
  genIntermediate _ _ = simpleFresh ()

data TestSemanticsCost = TestSemanticsCost

instance
  (MonadContext ctx) =>
  OpCost TestSemanticsCost TestSemanticsOp SymInteger ctx
  where
  opCost _ _ Add = return 2
  opCost _ _ Double = return 1
  opCost _ _ Inc = return 1
  opCost _ _ DivMod = return 5
