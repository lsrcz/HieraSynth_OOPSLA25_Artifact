{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module HieraSynth.VarId
  ( ConcreteVarId,
    SymbolicVarId,
    RelatedVarId,
  )
where

import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import Grisette (GenSymSimple, Mergeable, PPrint, SymOrd, ToCon, ToSym)

type ConcreteVarId varId =
  ( PPrint varId,
    Show varId,
    Eq varId,
    Hashable varId,
    Integral varId,
    Typeable varId,
    Mergeable varId
  )

type SymbolicVarId varId =
  ( Show varId,
    Eq varId,
    Num varId,
    SymOrd varId,
    Mergeable varId,
    Typeable varId,
    GenSymSimple () varId
  )

type RelatedVarId conVarId symVarId =
  ( ConcreteVarId conVarId,
    SymbolicVarId symVarId,
    ToSym conVarId symVarId,
    ToCon symVarId conVarId
  )
