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
{-# LANGUAGE UndecidableInstances #-}

module HieraSynth.Program.Concrete.MayMultiPathTest
  ( mayMultiPathTest,
  )
where

import Control.Monad.Except (runExceptT)
import Data.List ((\\))
import GHC.Generics (Generic)
import Grisette
  ( MonadUnion,
    Solvable (ssym),
    SymBool,
    Union,
    basicClasses0,
    derive,
    identifier,
    liftToMonadUnion,
    mrgIf,
    ordClasses,
    unifiedSymOrdClasses,
  )
import Grisette.Internal.Core.Control.Monad.Union (isMerged)
import HieraSynth.Context (MonadContext, SymbolicContext)
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgMayMultiPath (ProgMayMultiPath),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.Program.ProgSemantics (ProgSemantics (runProg))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import HieraSynth.Util.Show (showAsText)
import Test.Framework (Test, TestOptions' (topt_timeout), plusTestOptions)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)

newtype MayAddOneOp = MayAddOneOp SymBool deriving (Generic)

derive [''MayAddOneOp] (basicClasses0 \\ (ordClasses ++ unifiedSymOrdClasses))

data Sem = Sem

data IntType = IntType deriving (Generic)

derive [''IntType] basicClasses0

mayAddOne :: SymBool -> Int -> Union Int
mayAddOne s x = mrgIf s (return x) (return $ x + 1)

instance
  (MonadContext ctx, MonadUnion ctx) =>
  OpTyping MayAddOneOp ctx
  where
  type OpTypeType MayAddOneOp = IntType
  typeOp _ = return $ TypeSignature [IntType] [IntType]

instance
  (MonadContext ctx, MonadUnion ctx) =>
  OpSemantics Sem MayAddOneOp Int ctx
  where
  applyOp _ _ (MayAddOneOp s) [x] = do
    r <- liftToMonadUnion $ mayAddOne s x
    return [r]
  applyOp _ _ _ _ =
    error "Incorrect number of arguments for MayAddOne, expected 1 argument."

prog :: Int -> Prog MayAddOneOp Int IntType
prog n =
  Prog
    [ProgArg "x" 0 IntType]
    ( fmap
        ( \i ->
            Stmt
              (MayAddOneOp $ ssym $ identifier $ "s" <> showAsText i)
              [i]
              [i + 1]
        )
        [0 .. n]
    )
    [ProgRes n IntType]

mayMultiPathTest :: Test
mayMultiPathTest =
  plusTestOptions (mempty {topt_timeout = Just $ Just 5000000}) $
    testCase "ProgMayMultiPath should not have path explosion" $ do
      let actual =
            runProg Sem mempty (ProgMayMultiPath $ prog 100) [0] :: SymbolicContext [Int]
      assertBool "isMerged" $ isMerged $ runExceptT actual
