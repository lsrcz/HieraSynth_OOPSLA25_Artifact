{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.Memory
  ( BlockId,
    MemoryBlock (..),
    Memory (..),
    Ptr (..),
    getMemoryBlock,
    ptrWidth,
    freshValidMemoryBlock,
    freshValidMemory,
  )
where

import Control.DeepSeq (NFData)
import Data.Bytes.Serial (Serial)
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.Ratio (numerator)
import Grisette
  ( BV (bv),
    EvalSym (evalSym),
    GenSym,
    GenSymSimple (simpleFresh),
    LogicalOp ((.&&)),
    Mergeable (rootStrategy),
    MergingStrategy (SortedStrategy),
    PPrint,
    Solvable (con),
    SymEq ((.==)),
    ToCon (toCon),
    ToSym (toSym),
    deriveWith,
    wrapStrategy,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (MonadAngelicContext, MonadContext)
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified
  ( EvalModeConvertible (withModeConvertible'),
    EvalModeTag (S),
    GetBool,
    GetSomeWordN,
  )
import RVV.EvalMode (EvalMode, MonadEvalMode)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (MachineBaseConfig, machineScalarLength),
    MachineConfig (MachineConfig, baseConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineMemoryBlockLengths),
  )
import RVV.Semantics.Multiplier (WidthMul (getWidthMul))
import RVV.Util.Derive (deriveFull, firstModeDeriveConfig)

type BlockId = Int

data MemoryBlock mode = MemoryBlock
  { memoryBlockData :: GetSomeWordN mode,
    memoryBlockUninitialized :: GetSomeWordN mode
  }

deriveFull firstModeDeriveConfig [''MemoryBlock]
deriveWith
  firstModeDeriveConfig
  [''MemoryBlock]
  [''PPrint]

instance GenSym (MachineConfig, Int) (MemoryBlock 'S)

instance GenSymSimple (MachineConfig, Int) (MemoryBlock 'S) where
  simpleFresh (MachineConfig {scalableConfig = MachineScalableConfig {..}}, blockId) =
    MemoryBlock <$> simpleFresh len <*> simpleFresh len
    where
      len = machineMemoryBlockLengths M.! blockId

freshValidMemoryBlock ::
  (MonadAngelicContext ctx) =>
  MachineConfig ->
  BlockId ->
  ctx (MemoryBlock 'S)
freshValidMemoryBlock vconst blockId =
  MemoryBlock <$> simpleFresh len <*> return (bv len 0)
  where
    len = machineMemoryBlockLengths (scalableConfig vconst) M.! blockId

newtype Memory mode
  = Memory {memoryBlocks :: M.HashMap BlockId (MemoryBlock mode)}

deriveWith
  firstModeDeriveConfig
  [''Memory]
  [''Show, ''Eq, ''PPrint, ''NFData, ''Hashable, ''Serial]

instance
  (EvalModeConvertible mode' mode) =>
  ToCon (Memory mode) (Memory mode')
  where
  toCon (Memory m) = withModeConvertible' @mode' @mode r r r
    where
      r :: (Mergeable (Memory mode')) => Maybe (Memory mode')
      r =
        Memory . M.fromList
          <$> traverse
            (\(k, v) -> (k,) <$> toCon v)
            (M.toList m)

instance
  (EvalModeConvertible mode mode') =>
  ToSym (Memory mode) (Memory mode')
  where
  toSym (Memory m) = withModeConvertible' @mode @mode' r r r
    where
      r :: (Mergeable (Memory mode')) => Memory mode'
      r = Memory $ M.map toSym m

getMemoryBlock ::
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  BlockId ->
  Memory mode ->
  ctx (MemoryBlock mode)
getMemoryBlock bid (Memory blocks) =
  case M.lookup bid blocks of
    Just block -> return block
    Nothing -> mrgThrowError $ "Block " <> showAsText bid <> " not found"

instance (EvalMode mode) => SymEq (Memory mode) where
  Memory lhs .== Memory rhs =
    toSym (M.keysSet lhs == M.keysSet rhs)
      .&& M.foldlWithKey'
        (\acc k v -> acc .&& (v .== rhs M.! k))
        (con True)
        lhs

instance (EvalMode mode) => Mergeable (Memory mode) where
  rootStrategy =
    SortedStrategy
      (\(Memory v) -> M.keysSet v)
      ( \_ ->
          wrapStrategy
            rootStrategy
            (Memory . M.fromList)
            (\(Memory l) -> sortOn fst $ M.toList l)
      )

instance (EvalMode mode) => EvalSym (Memory mode) where
  evalSym fillDefault model (Memory m) =
    Memory $ M.map (evalSym fillDefault model) m

instance GenSym MachineConfig (Memory 'S)

instance GenSymSimple MachineConfig (Memory 'S) where
  simpleFresh vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}} =
    Memory
      <$> M.traverseWithKey
        (\k _ -> simpleFresh (vconst, k))
        machineMemoryBlockLengths

freshValidMemory ::
  (MonadAngelicContext ctx) =>
  MachineConfig ->
  ctx (Memory 'S)
freshValidMemory vconst@MachineConfig {scalableConfig = MachineScalableConfig {..}} =
  Memory
    <$> M.traverseWithKey
      (\k _ -> freshValidMemoryBlock vconst k)
      machineMemoryBlockLengths

data Ptr mode = Ptr
  { ptrWidthXlenMultiplier :: WidthMul,
    ptrBlockId :: BlockId,
    ptrOffset :: GetSomeWordN mode,
    ptrUninitialized :: GetBool mode
  }

ptrWidth :: MachineConfig -> Ptr mode -> Int
ptrWidth MachineConfig {baseConfig = MachineBaseConfig {..}} Ptr {..} =
  numerator $
    fromIntegral machineScalarLength * getWidthMul ptrWidthXlenMultiplier

deriveFull firstModeDeriveConfig [''Ptr]
deriveWith firstModeDeriveConfig [''Ptr] [''PPrint]
