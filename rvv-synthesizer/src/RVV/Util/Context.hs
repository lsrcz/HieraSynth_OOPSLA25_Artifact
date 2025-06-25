{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module RVV.Util.Context (assert) where

import qualified Data.Text as T
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (MonadContext)
import Grisette.Unified (GetBool, mrgIf)
import RVV.EvalMode (MonadEvalMode)

assert ::
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  T.Text ->
  GetBool mode ->
  ctx ()
-- assert msg cond = mrgIf cond (return ()) (mrgThrowError msg)

assert msg cond = mrgIf cond (return ()) (mrgThrowError $ msg <> "\n" <> stack)
  where
   stack = T.pack (prettyCallStack callStack)
