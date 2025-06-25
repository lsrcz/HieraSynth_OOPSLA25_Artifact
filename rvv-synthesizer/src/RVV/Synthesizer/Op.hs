{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module RVV.Synthesizer.Op
  ( OpWrapper (..),
    Op,
    ConOp,
    SymOp,
    SketchOp,
    ConProg,
    SymOpConProg,
    SymOpSymProg,
    SymProg,
    ConSymbolTable,
    SymSymbolTable,
    SketchChoiceTree,
    SketchSpec (..),
    CompSketchSpec,
    ConSketchSpec,
    SketchSpecTable,
  )
where

import Control.Monad.Except (runExceptT)
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    LogicalOp (false, true),
    PPrint (pformat),
    Solvable (con),
    SymBool,
    Union,
    mrgFmap,
    simpleMerge,
  )
import HieraSynth.Combinator.Invoke (Invoke (Invoke))
import HieraSynth.Combinator.Null (Null)
import HieraSynth.Combinator.Sum
  ( type (:<:) (inj, prj),
    type (:|) (InLeft, InRight),
  )
import HieraSynth.Operator.OpParser (OpParser (opParser))
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree)
import HieraSynth.Program.Choice.ComponentBag (ComponentBag)
import HieraSynth.Program.Choice.Counting
  ( CountNumProgs
      ( countNumComponentChoices,
        countNumChoices,
        countNumInsts,
        countNumProgs
      ),
    SplitChoice (splitChoice),
  )
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
  )
import HieraSynth.Program.ComponentSketch (OpSymmetryReduction)
import qualified HieraSynth.Program.ComponentSketch as Component
import HieraSynth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import HieraSynth.Program.Concrete
  ( OpPPrint (describeArguments, pformatOp, prefixResults),
    ProgPPrint (pformatProg),
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import HieraSynth.Program.SymbolTable (SymbolTable)
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import HieraSynth.VarId (ConcreteVarId, SymbolicVarId)
import Grisette.Unified (EvalModeTag (C, S), UnifiedSymEq)
import RVV.EvalMode (MonadEvalMode)
import RVV.Synthesizer.CostModel.CostModel (CostModel)
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Operator.Common.OpSymmetryReduction
  ( opUnreorderableByType,
  )
import RVV.Synthesizer.Operator.DelegatedVectorBinaryOnMask
  ( DelegatedVectorBinaryOnMask,
    SketchDelegatedVectorBinaryOnMask,
  )
import RVV.Synthesizer.Operator.Extract (Extract, SketchExtract)
import RVV.Synthesizer.Operator.FixedPointClip (FixedPointClip, SketchFixedPointClip)
import RVV.Synthesizer.Operator.Insert (Insert, SketchInsert, SketchVectorExtend, VectorExtend)
import RVV.Synthesizer.Operator.Load (Load, SketchLoad)
import RVV.Synthesizer.Operator.MaskLogical
  ( MaskLogical,
    SketchMaskLogical,
  )
import RVV.Synthesizer.Operator.Merge (Merge, SketchMerge)
import RVV.Synthesizer.Operator.MiscMask (MiscMask, SketchMiscMask)
import RVV.Synthesizer.Operator.Move (Move, MoveFirstElementToScalar, SketchMove, SketchMoveFirstElementToScalar)
import RVV.Synthesizer.Operator.NarrowingRightShift (NarrowingRightShift, SketchNarrowingRightShift)
import RVV.Synthesizer.Operator.Reinterpret (Reinterpret, SketchReinterpret)
import RVV.Synthesizer.Operator.Scalar (Scalar, SketchScalar)
import RVV.Synthesizer.Operator.ScalarOperator (ScalarOperator, SketchScalarOperator)
import RVV.Synthesizer.Operator.ScalarTrunc (ScalarTrunc, SketchScalarTrunc)
import RVV.Synthesizer.Operator.SetVectorLength
  ( SetVectorLength,
    SketchSetVectorLength,
  )
import RVV.Synthesizer.Operator.SingleWidthIntBinary
  ( SingleWidthIntBinary,
    SketchSingleWidthIntBinary,
  )
import RVV.Synthesizer.Operator.SingleWidthMulAdd (SingleWidthMulAdd, SketchSingleWidthMulAdd)
import RVV.Synthesizer.Operator.Slide (SketchSlide, Slide)
import RVV.Synthesizer.Operator.Store (SketchStore, Store)
import RVV.Synthesizer.Operator.Undefined (SketchUndefined, Undefined)
import RVV.Synthesizer.Operator.VectorCompare (SketchVectorCompare, VectorCompare)
import RVV.Synthesizer.Operator.VectorIndex (SketchVectorIndex, VectorIndex)
import RVV.Synthesizer.Operator.Vlenb (SketchVlenb, Vlenb)
import RVV.Synthesizer.Operator.WideningIntBinary
  ( SketchWideningIntBinary,
    WideningIntBinary,
  )
import RVV.Synthesizer.Operator.WideningMulAdd (SketchWideningMulAdd, WideningMulAdd)
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Synthesizer.Type (ValueType, isMemType, isVLType)
import RVV.Util.Derive (deriveFullExcept, deriveNoSymEval, noModeDeriveConfig)

type AllOp mode =
  SetVectorLength mode
    :| Undefined
    :| Load mode
    :| Store mode
    :| SingleWidthIntBinary mode
    :| WideningIntBinary mode
    :| SingleWidthMulAdd mode
    :| WideningMulAdd mode
    :| NarrowingRightShift mode
    :| FixedPointClip mode
    :| DelegatedVectorBinaryOnMask mode
    :| MaskLogical mode
    :| Vlenb
    :| Extract
    :| Insert mode
    :| VectorExtend
    :| Reinterpret
    :| ScalarOperator mode
    :| ScalarTrunc
    :| Slide mode
    :| VectorCompare mode
    :| VectorIndex mode
    :| Merge mode
    :| MiscMask mode
    :| Move mode
    :| MoveFirstElementToScalar
    :| Scalar mode
    -- :| BaseOp mode
    :| Invoke ValueType
    :| Null ValueType

type AllSketchOp =
  SketchSetVectorLength
    :| SketchUndefined
    :| SketchLoad
    :| SketchStore
    :| SketchSingleWidthIntBinary
    :| SketchWideningIntBinary
    :| SketchSingleWidthMulAdd
    :| SketchWideningMulAdd
    :| SketchNarrowingRightShift
    :| SketchFixedPointClip
    :| SketchDelegatedVectorBinaryOnMask
    :| SketchMaskLogical
    :| SketchVlenb
    :| SketchExtract
    :| SketchInsert
    :| SketchVectorExtend
    :| SketchReinterpret
    :| SketchScalarOperator
    :| SketchScalarTrunc
    :| SketchSlide
    :| SketchVectorCompare
    :| SketchVectorIndex
    :| SketchMerge
    :| SketchMiscMask
    :| SketchMove
    :| SketchMoveFirstElementToScalar
    :| SketchScalar
    -- :| SketchBaseOp
    :| Invoke ValueType
    :| Null ValueType

newtype OpWrapper opBase = Op opBase

deriveFullExcept noModeDeriveConfig [''OpWrapper] [''UnifiedSymEq]

instance
  (OpSemantics sem opBase value ctx) =>
  OpSemantics sem (OpWrapper opBase) value ctx
  where
  applyOp vconst table (Op op) = applyOp vconst table op

instance (OpTyping opBase ctx) => OpTyping (OpWrapper opBase) ctx where
  type OpTypeType (OpWrapper opBase) = OpTypeType opBase
  typeOp (Op op) = typeOp op

instance (MonadEvalMode mode Union) => OpSymmetryReduction (Op mode) where
  opUnreorderable (prj -> Just (Invoke _ _ :: Invoke ValueType)) r =
    case r of
      (prj -> Just (Invoke _ _ :: Invoke ValueType)) -> true
      _ -> simpleMerge $ do
        r <- runExceptT $ do
          TypeSignature _ op2Ress <- typeOp r
          return $ con $ any isVLType op2Ress || any isMemType op2Ress
        case r of
          Left _ -> return false
          Right v -> return v :: Union SymBool
  opUnreorderable l (prj -> Just (Invoke _ _ :: Invoke ValueType)) =
    simpleMerge $ do
      r <- runExceptT $ do
        TypeSignature op1Args _ <- typeOp l
        return $ con $ any isVLType op1Args || any isMemType op1Args
      case r of
        Left _ -> return false
        Right v -> return v :: Union SymBool
  opUnreorderable (Op l) (Op r) = opUnreorderableByType l r
  opCommutativeArgPos (Op op) = opCommutativeArgPos op

instance (PPrint opBase) => PPrint (OpWrapper opBase) where
  pformat (Op op) = pformat op

instance (OpPPrint opBase) => OpPPrint (OpWrapper opBase) where
  describeArguments (Op op) = case describeArguments op of
    Left err -> Left $ Op <$> err
    Right args -> Right args
  prefixResults (Op op) = case prefixResults op of
    Left err -> Left $ Op <$> err
    Right res -> Right res
  pformatOp (Op op) = pformatOp op

instance (OpParser opBase) => OpParser (OpWrapper opBase) where
  opParser = Op <$> opParser

instance
  (OpReachableSymbols opBase) =>
  OpReachableSymbols (OpWrapper opBase)
  where
  opReachableSymbols (Op op) = opReachableSymbols op

instance
  (OpCost CostModel opBase cost ctx) =>
  OpCost CostModel (OpWrapper opBase) cost ctx
  where
  opCost costObj table (Op op) = opCost costObj table op

instance
  (ExtractFeature opBase featureSet) =>
  ExtractFeature (OpWrapper opBase) featureSet
  where
  extractFeature (Op op) = extractFeature op

instance (ScaleLMul opBase) => ScaleLMul (OpWrapper opBase) where
  scaleLMul ratio (Op op) = Op $ scaleLMul ratio op

instance
  (GenSym opBase0 opBase1) =>
  GenSym (OpWrapper opBase0) (OpWrapper opBase1)
  where
  fresh (Op op) = mrgFmap Op <$> fresh op

instance (SplitChoice opBase) => SplitChoice (OpWrapper opBase) where
  splitChoice (Op op) = Op <$> splitChoice op

instance
  (ToFastSketch opBase0 opBase1) =>
  ToFastSketch (OpWrapper opBase0) (OpWrapper opBase1)
  where
  toFastSketch (Op op) = Op $ toFastSketch op

type Op mode = OpWrapper (AllOp mode)

instance (a :<: AllOp mode) => a :<: Op mode where
  inj = Op . inj
  prj (Op x) = prj x

type ConOp = Op 'C

type SymOp = Union (Op 'S)

type SketchOp = OpWrapper AllSketchOp

instance (a :<: AllSketchOp) => a :<: SketchOp where
  inj = Op . inj
  prj (Op x) = prj x

type ConProg conVarId = Concrete.Prog ConOp conVarId ValueType

type SymOpConProg conVarId = Concrete.Prog SymOp conVarId ValueType

type SymOpSymProg symVarId = Component.Prog SymOp symVarId ValueType

type SymProg conVarId symVarId = SymOpConProg conVarId :| SymOpSymProg symVarId

type ConSymbolTable conVarId = SymbolTable (ConProg conVarId)

type SymSymbolTable conVarId symVarId = SymbolTable (SymProg conVarId symVarId)

type SketchChoiceTree = ChoiceTree SketchOp

type CompSketchSpec = ComponentBag SketchOp ValueType

type ConSketchSpec conVarId =
  Concrete.Prog SketchChoiceTree conVarId ValueType

data SketchSpec conVarId
  = CompSpec CompSketchSpec
  | ConSpec (ConSketchSpec conVarId)
  | ConProg (ConProg conVarId)

deriveNoSymEval [''SketchSpec]

instance
  (ConcreteVarId conVarId, SymbolicVarId symVarId) =>
  GenSymSimple (SketchSpec conVarId) (SymProg conVarId symVarId)
  where
  simpleFresh (CompSpec comp) = InRight <$> simpleFresh comp
  simpleFresh (ConSpec con) = InLeft <$> simpleFresh con
  simpleFresh (ConProg con) = InLeft <$> simpleFresh con

type SketchSpecTable conVarId = SymbolTable (SketchSpec conVarId)

instance PartitionSpec (SketchSpec conVarId) where
  partitionSpec seqNum (CompSpec comp) = CompSpec <$> partitionSpec seqNum comp
  partitionSpec seqNum (ConSpec con) = ConSpec <$> partitionSpec seqNum con
  partitionSpec _ (ConProg con) = [ConProg con]

instance LowestSeqNum (SketchSpec conVarId) where
  lowestSeqNum success (CompSpec comp) = lowestSeqNum success comp
  lowestSeqNum success (ConSpec con) = lowestSeqNum success con
  lowestSeqNum _ (ConProg _) = Nothing

instance (ConcreteVarId conVarId) => ProgPPrint (SketchSpec conVarId) where
  pformatProg name (CompSpec comp) = pformatProg name comp
  pformatProg name (ConSpec con) = pformatProg name con
  pformatProg name (ConProg con) = pformatProg name con

instance CountNumProgs (SketchSpec conVarId) where
  countNumProgs (CompSpec c) = countNumProgs c
  countNumProgs (ConSpec c) = countNumProgs c
  countNumProgs (ConProg _) = 1
  countNumChoices (CompSpec c) = countNumChoices c
  countNumChoices (ConSpec c) = countNumChoices c
  countNumChoices (ConProg _) = 1
  countNumComponentChoices (CompSpec c) = countNumComponentChoices c
  countNumComponentChoices (ConSpec c) = countNumComponentChoices c
  countNumComponentChoices (ConProg _) = mempty
  countNumInsts (CompSpec c) = countNumInsts c
  countNumInsts (ConSpec c) = countNumInsts c
  countNumInsts (ConProg _) = 1

instance ScaleLMul (SketchSpec conVarId) where
  scaleLMul ratio (CompSpec compSpec) =
    CompSpec $ scaleLMul ratio compSpec
  scaleLMul ratio (ConSpec conSpec) =
    ConSpec $ scaleLMul ratio conSpec
  scaleLMul ratio (ConProg conProg) =
    ConProg $ scaleLMul ratio conProg

instance ToFastSketch (ConProg conVarId) (SketchSpec conVarId) where
  toFastSketch = ConSpec . toFastSketch
