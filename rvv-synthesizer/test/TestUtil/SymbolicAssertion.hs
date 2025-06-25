{-# LANGUAGE OverloadedStrings #-}

module TestUtil.SymbolicAssertion ((.@?=), symShouldEq) where

import GHC.Stack (HasCallStack)
import Grisette
  ( EvalSym (evalSym),
    Model,
    SymEq ((./=), (.==)),
    solve,
    z3,
  )

(.@?=) :: (HasCallStack, Show a, SymEq a, EvalSym a) => a -> a -> IO ()
(.@?=) actual expected =
  symShouldEq
    actual
    expected
    ( \m ->
        "Can be not equal, model: "
          <> show m
          <> ". Actual value: "
          <> show (evalSym False m actual)
          <> ". Expected value: "
          <> show (evalSym False m expected)
    )

symShouldEq ::
  (HasCallStack, Show a, SymEq a, EvalSym a) =>
  a ->
  a ->
  (Model -> String) ->
  IO ()
symShouldEq actual expected notEqualCaseMessage = do
  canBeNotEqual <- solve z3 $ actual ./= expected
  canBeEqual <- solve z3 $ actual .== expected
  case (canBeNotEqual, canBeEqual) of
    (Left _, Right _) -> return ()
    (Right m, _) -> fail $ notEqualCaseMessage m
    (_, Left _) -> fail "Cannot be equal"
