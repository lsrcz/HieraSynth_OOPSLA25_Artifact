{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.Undefined
  ( Undefined (..),
    SketchUndefined (..),
    undefinedVector,
    undefinedMask,
    sketchUndefinedVector,
    sketchUndefinedMask,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (makeFieldsNoPrefix)
import GHC.Generics (Generic)
import Grisette
  ( GenSym,
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    mrgReturn,
  )
import HieraSynth.Combinator.Embed ((:<:) (inj))
import HieraSynth.Context (MonadAngelicContext, MonadContext)
import HieraSynth.Operator.OpParser (OpParser (opParser))
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.Program.Choice.Counting (SplitChoice (splitChoice))
import HieraSynth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import HieraSynth.Program.Concrete
  ( OpPPrint (describeArguments, prefixResults),
  )
import HieraSynth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import HieraSynth.Util.Parser (betweenBrackets, named, symbol)
import Grisette.Unified (UnifiedSymEq)
import RVV.Parser.ArgParser (maskMulParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel (CostModel (CostModel))
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet (FeatureSet)
import RVV.Synthesizer.Lens
  ( HasMaskMul (maskMul),
    HasVectorConfig (vectorConfig),
  )
import RVV.Synthesizer.OpSemantics.Misc
  ( applyUndefinedMask,
    applyUndefinedVector,
    typeUndefinedMask,
    typeUndefinedVector,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    withName,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value)
import RVV.Util.Derive (deriveFullExcept, deriveNoSymEval, noModeDeriveConfig)

data Undefined
  = UndefinedVector {_vectorConfig :: VectorConfig}
  | UndefinedMask {_maskMul :: MaskMul}
  deriving (Generic)

data SketchUndefined
  = SketchUndefinedVector {_vectorConfig :: VectorConfig}
  | SketchUndefinedMask {_maskMul :: MaskMul}
  deriving (Generic)

deriveFullExcept noModeDeriveConfig [''Undefined] [''UnifiedSymEq]
deriveNoSymEval [''SketchUndefined]
makeFieldsNoPrefix ''Undefined
makeFieldsNoPrefix ''SketchUndefined

undefinedVector :: (Undefined :<: op) => VectorConfig -> op
undefinedVector = inj . UndefinedVector

undefinedMask :: (Undefined :<: op) => MaskMul -> op
undefinedMask = inj . UndefinedMask

sketchUndefinedVector :: (SketchUndefined :<: op) => VectorConfig -> op
sketchUndefinedVector = inj . SketchUndefinedVector

sketchUndefinedMask :: (SketchUndefined :<: op) => MaskMul -> op
sketchUndefinedMask = inj . SketchUndefinedMask

instance
  (SemConstraint mode ctx) =>
  OpSemantics MachineConfig Undefined (Value mode) ctx
  where
  applyOp machine _ op values = case op of
    UndefinedVector config -> applyUndefinedVector machine config values
    UndefinedMask maskMul -> applyUndefinedMask machine maskMul values

instance (MonadContext ctx) => OpTyping Undefined ctx where
  type OpTypeType Undefined = ValueType
  typeOp op = case op of
    UndefinedVector config -> typeUndefinedVector config
    UndefinedMask mul -> typeUndefinedMask mul

instance (MonadAngelicContext ctx) => OpTyping SketchUndefined ctx where
  type OpTypeType SketchUndefined = ValueType
  typeOp = typeSketchOp @Undefined

instance OpSymmetryReduction Undefined where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchUndefined Undefined

instance GenSymSimple SketchUndefined Undefined where
  simpleFresh (SketchUndefinedVector config) = return $ UndefinedVector config
  simpleFresh (SketchUndefinedMask mul) = return $ UndefinedMask mul

instance GenSym Undefined Undefined

instance GenSymSimple Undefined Undefined where
  simpleFresh = return . toSym

instance PPrint Undefined where
  pformat (UndefinedVector config) = "undefined" <> pformatArgList config
  pformat (UndefinedMask mul) =
    "undefined_mask" <> pformatArgList (withName "mmul" mul)

instance OpPPrint Undefined where
  describeArguments _ = return []

instance PPrint SketchUndefined where
  pformat (SketchUndefinedVector config) = "undefined" <> pformatArgList config
  pformat (SketchUndefinedMask mul) =
    "undefined_mask" <> pformatArgList (withName "mmul" mul)

instance OpPPrint SketchUndefined where
  describeArguments _ = return []
  prefixResults _ = return []

instance OpParser Undefined where
  opParser = undefinedVectorParser <|> undefinedMaskParser
    where
      undefinedVectorParser = do
        symbol "undefined"
        config <- betweenBrackets vectorConfigParser
        return $ UndefinedVector config
      undefinedMaskParser = do
        symbol "undefined_mask"
        mmul <- betweenBrackets $ named "mmul" maskMulParser
        return $ UndefinedMask mmul

instance OpReachableSymbols Undefined where
  opReachableSymbols = mempty

instance SplitChoice SketchUndefined where
  splitChoice op = [op]

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel Undefined cost ctx
  where
  opCost CostModel {} _ _ = mrgReturn 0

instance ExtractFeature Undefined FeatureSet where
  extractFeature _ = error "Unsupported"

instance ToFastSketch Undefined SketchUndefined where
  toFastSketch (UndefinedVector config) = SketchUndefinedVector config
  toFastSketch (UndefinedMask mul) = SketchUndefinedMask mul

instance ScaleLMul Undefined where
  scaleLMul ratio (UndefinedVector config) =
    UndefinedVector $ scaleLMul ratio config
  scaleLMul ratio (UndefinedMask mul) = UndefinedMask $ scaleLMul ratio mul

instance ScaleLMul SketchUndefined where
  scaleLMul ratio (SketchUndefinedVector config) =
    SketchUndefinedVector $ scaleLMul ratio config
  scaleLMul ratio (SketchUndefinedMask mul) =
    SketchUndefinedMask $ scaleLMul ratio mul
