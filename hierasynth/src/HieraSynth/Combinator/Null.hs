{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HieraSynth.Combinator.Null (Null) where

import GHC.Generics (Generic)
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    Mergeable,
    allClasses01,
    derive,
  )
import HieraSynth.Context (MonadContext)
import HieraSynth.Operator.OpParser (OpParser (opParser))
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.Program.Choice.Counting
  ( CountNumProgs (countNumChoices, countNumComponentChoices, countNumInsts, countNumProgs),
    SplitChoice (splitChoice),
  )
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
  )
import HieraSynth.Program.ComponentSketch
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import HieraSynth.Program.Concrete
  ( OpPPrint (describeArguments, pformatOp, prefixResults),
  )
import HieraSynth.Program.Concrete.Flatten
  ( OpFlatten (opForwardedSubProg),
  )
import HieraSynth.Program.Concrete.Program
  ( ProgPPrint (pformatProg),
    ProgToDot (toDotProg),
  )
import HieraSynth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import HieraSynth.Program.ProgParser (ProgParser (progParser))
import HieraSynth.Program.ProgSemantics (ProgSemantics (runProg))
import HieraSynth.Program.ProgTyping (ProgTyping (typeProg))
import HieraSynth.Program.ProgUtil
  ( ProgUtil
      ( ProgOpType,
        ProgStmtType,
        ProgTypeType,
        ProgVarIdType
      ),
    ProgUtilImpl
      ( getProgArgIds,
        getProgNumStmts,
        getProgResIds,
        getProgStmtAtIdx
      ),
    StmtUtil
      ( StmtOpType,
        StmtVarIdType
      ),
    StmtUtilImpl
      ( getStmtArgIds,
        getStmtDisabled,
        getStmtOp,
        getStmtResIds
      ),
  )
import HieraSynth.Program.SymbolTable
  ( ProgReachableSymbols (progReachableSymbols),
  )
import HieraSynth.Type.TypeParser (TypeParser (typeParser))

data Null ty deriving (Generic)

derive [''Null] allClasses01

instance (MonadContext ctx) => ProgSemantics semObj (Null ty) val ctx where
  runProg _ _ = error "Impossible"

instance StmtUtilImpl (Null ty) (Null ty) (Null ty) where
  getStmtArgIds = error "Impossible"
  getStmtResIds = error "Impossible"
  getStmtOp = error "Impossible"
  getStmtDisabled _ = error "Impossible"

instance StmtUtil (Null ty) where
  type StmtVarIdType (Null ty) = Null ty
  type StmtOpType (Null ty) = Null ty

instance ProgUtilImpl (Null ty) (Null ty) (Null ty) (Null ty) where
  getProgArgIds _ = error "Impossible"
  getProgResIds _ = error "Impossible"
  getProgNumStmts _ = error "Impossible"
  getProgStmtAtIdx _ _ = error "Impossible"

instance ProgUtil (Null ty) where
  type ProgOpType (Null ty) = Null ty
  type ProgTypeType (Null ty) = ty
  type ProgVarIdType (Null ty) = Null ty
  type ProgStmtType (Null ty) = Null ty

instance ProgTyping (Null ty) where
  typeProg _ = error "Impossible"

instance ProgPPrint (Null ty) where
  pformatProg _ = error "Impossible"

instance ProgToDot (Null ty) where
  toDotProg _ = error "Impossible"

instance LowestSeqNum (Null ty) where
  lowestSeqNum _ _ = error "Impossible"

instance PartitionSpec (Null ty) where
  partitionSpec _ _ = error "Impossible"

instance GenSymSimple (Null ty0) (Null ty1) where
  simpleFresh _ = error "Impossible"

instance (Mergeable a) => GenSym (Null ty0) a where
  fresh _ = error "Impossible"

instance (Mergeable a) => GenSymSimple (Null ty0) a where
  simpleFresh _ = error "Impossible"

instance (MonadContext ctx) => OpTyping (Null ty) ctx where
  type OpTypeType (Null ty) = ty
  typeOp _ = error "Impossible"

instance (MonadContext ctx) => OpSemantics semObj (Null ty) val ctx where
  applyOp _ _ _ = error "Impossible"

instance OpReachableSymbols (Null ty) where
  opReachableSymbols _ = error "Impossible"

instance OpSymmetryReduction (Null ty) where
  opUnreorderable _ _ = error "Impossible"
  opCommutativeArgPos _ = error "Impossible"

instance OpPPrint (Null ty) where
  describeArguments _ = error "Impossible"
  prefixResults _ = error "Impossible"
  pformatOp _ = error "Impossible"

instance OpFlatten (Null ty) (Null ty) where
  opForwardedSubProg _ = error "Impossible"

instance (MonadContext ctx) => OpCost costObj (Null ty) cost ctx where
  opCost _ _ _ = error "Impossible"

instance CountNumProgs (Null ty) where
  countNumProgs _ = error "Impossible"
  countNumChoices _ = error "Impossible"
  countNumComponentChoices _ = error "Impossible"
  countNumInsts _ = error "Impossible"

instance SplitChoice (Null ty) where
  splitChoice _ = error "Impossible"

instance ProgReachableSymbols (Null ty) where
  progReachableSymbols _ = error "Impossible"

instance OpParser (Null ty) where
  opParser = fail "Impossible"

instance TypeParser (Null ty) where
  typeParser = fail "Impossible"

instance ProgParser (Null ty) where
  progParser = fail "Impossible"
