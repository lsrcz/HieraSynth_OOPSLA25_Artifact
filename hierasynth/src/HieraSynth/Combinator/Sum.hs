{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HieraSynth.Combinator.Sum ((:|) (..), (:<:) (..)) where

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (second))
import Data.Functor.Classes
  ( Show1 (liftShowsPrec),
    Show2 (liftShowsPrec2),
    showsPrec1,
  )
import Data.List ((\\))
import GHC.Generics (Generic)
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    LogicalOp (true),
    PPrint (pformatList, pformatPrec),
    PPrint1 (liftPFormatPrec),
    PPrint2 (liftPFormatPrec2),
    ToCon (toCon),
    allClasses012,
    derive,
    mrgFmap,
    pformatPrec1,
    pprintClasses,
    showClasses,
  )
import HieraSynth.Combinator.Embed ((:<:) (inj, prj))
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
import HieraSynth.Program.Concrete (OpPPrint (describeArguments))
import HieraSynth.Program.Concrete.Flatten
  ( OpFlatten (opForwardedSubProg),
  )
import HieraSynth.Program.Concrete.OpPPrint
  ( OpPPrint (pformatOp, prefixResults),
  )
import HieraSynth.Program.Concrete.Program
  ( ProgPPrint (pformatProg),
    ProgToDot (toDotProg),
  )
import HieraSynth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import HieraSynth.Program.ProgCost (ProgCost (progCost))
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
import Text.Megaparsec (try)

data (:|) l r = InLeft l | InRight r deriving (Generic)

infixr 5 :|

derive [''(:|)] (allClasses012 \\ (showClasses ++ pprintClasses))

instance Show2 (:|) where
  liftShowsPrec2 sp1 _ _ _ n (InLeft l) = sp1 n l
  liftShowsPrec2 _ _ sp2 _ n (InRight r) = sp2 n r

instance {-# INCOHERENT #-} (ToCon l c, ToCon r c) => ToCon ((:|) l r) c where
  toCon (InLeft l) = toCon l
  toCon (InRight r) = toCon r

instance (Show l) => Show1 ((:|) l) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show l, Show r) => Show ((:|) l r) where
  showsPrec = showsPrec1

instance PPrint2 (:|) where
  liftPFormatPrec2 pp1 _ _ _ n (InLeft l) = pp1 n l
  liftPFormatPrec2 _ _ pp2 _ n (InRight r) = pp2 n r

instance (PPrint l) => PPrint1 ((:|) l) where
  liftPFormatPrec = liftPFormatPrec2 pformatPrec pformatList

instance (PPrint l, PPrint r) => PPrint ((:|) l r) where
  pformatPrec = pformatPrec1

instance {-# OVERLAPPING #-} a :<: (a :| b) where
  inj = InLeft
  prj (InLeft v) = Just v
  prj (InRight _) = Nothing

instance {-# OVERLAPPABLE #-} (b :<: c) => b :<: (a :| c) where
  inj = InRight . inj
  prj (InLeft _) = Nothing
  prj (InRight v) = prj v

instance
  ( ProgSemantics semObj l val ctx,
    ProgSemantics semObj r val ctx,
    ProgTypeType l ~ ProgTypeType r
  ) =>
  ProgSemantics semObj ((:|) l r) val ctx
  where
  runProg semObj table (InLeft l) = runProg semObj table l
  runProg semObj table (InRight r) = runProg semObj table r

instance
  ( StmtUtil l,
    StmtUtil r,
    lop ~ StmtOpType l,
    rop ~ StmtOpType r,
    lid ~ StmtVarIdType l,
    rid ~ StmtVarIdType r
  ) =>
  StmtUtilImpl ((:|) l r) ((:|) lop rop) ((:|) lid rid)
  where
  getStmtArgIds (InLeft l) = InLeft <$> getStmtArgIds l
  getStmtArgIds (InRight r) = InRight <$> getStmtArgIds r
  getStmtResIds (InLeft l) = InLeft <$> getStmtResIds l
  getStmtResIds (InRight r) = InRight <$> getStmtResIds r
  getStmtOp (InLeft l) = InLeft $ getStmtOp l
  getStmtOp (InRight r) = InRight $ getStmtOp r
  getStmtDisabled (InLeft l) = getStmtDisabled l
  getStmtDisabled (InRight r) = getStmtDisabled r

instance (StmtUtil l, StmtUtil r) => StmtUtil ((:|) l r) where
  type
    StmtVarIdType ((:|) l r) =
      (:|) (StmtVarIdType l) (StmtVarIdType r)
  type
    StmtOpType ((:|) l r) =
      (:|) (StmtOpType l) (StmtOpType r)

instance
  ( ProgUtil l,
    ProgUtil r,
    lop ~ ProgOpType l,
    rop ~ ProgOpType r,
    lid ~ ProgVarIdType l,
    rid ~ ProgVarIdType r,
    lstmt ~ ProgStmtType l,
    rstmt ~ ProgStmtType r,
    ProgTypeType l ~ ProgTypeType r
  ) =>
  ProgUtilImpl
    ((:|) l r)
    ((:|) lop rop)
    ((:|) lstmt rstmt)
    ((:|) lid rid)
  where
  getProgArgIds (InLeft l) = InLeft <$> getProgArgIds l
  getProgArgIds (InRight r) = InRight <$> getProgArgIds r
  getProgResIds (InLeft l) = InLeft <$> getProgResIds l
  getProgResIds (InRight r) = InRight <$> getProgResIds r
  getProgNumStmts (InLeft l) = getProgNumStmts l
  getProgNumStmts (InRight r) = getProgNumStmts r
  getProgStmtAtIdx (InLeft l) i = InLeft <$> getProgStmtAtIdx l i
  getProgStmtAtIdx (InRight r) i = InRight <$> getProgStmtAtIdx r i

instance
  (ProgUtil l, ProgUtil r, ProgTypeType l ~ ProgTypeType r) =>
  ProgUtil ((:|) l r)
  where
  type ProgTypeType ((:|) l r) = ProgTypeType l
  type
    ProgStmtType ((:|) l r) =
      (:|) (ProgStmtType l) (ProgStmtType r)
  type
    ProgVarIdType ((:|) l r) =
      (:|) (ProgVarIdType l) (ProgVarIdType r)
  type
    ProgOpType ((:|) l r) =
      (:|) (ProgOpType l) (ProgOpType r)

instance
  ( ProgTyping l,
    ProgTyping r,
    ProgTypeType l ~ ProgTypeType r
  ) =>
  ProgTyping ((:|) l r)
  where
  typeProg (InLeft l) = typeProg l
  typeProg (InRight r) = typeProg r

instance (ProgPPrint l, ProgPPrint r) => ProgPPrint ((:|) l r) where
  pformatProg key (InLeft l) = pformatProg key l
  pformatProg key (InRight r) = pformatProg key r

instance (ProgToDot l, ProgToDot r) => ProgToDot ((:|) l r) where
  toDotProg key (InLeft l) = toDotProg key l
  toDotProg key (InRight r) = toDotProg key r

instance
  (ProgCost costObj prog0 cost ctx, ProgCost costObj prog1 cost ctx) =>
  ProgCost costObj ((:|) prog0 prog1) cost ctx
  where
  progCost costObj table (InLeft l) = progCost costObj table l
  progCost costObj table (InRight r) = progCost costObj table r

instance (LowestSeqNum l, LowestSeqNum r) => LowestSeqNum ((:|) l r) where
  lowestSeqNum succeeded (InLeft l) = lowestSeqNum succeeded l
  lowestSeqNum succeeded (InRight r) = lowestSeqNum succeeded r

instance (PartitionSpec l, PartitionSpec r) => PartitionSpec ((:|) l r) where
  partitionSpec seqNum (InLeft l) =
    [InLeft l | l <- partitionSpec seqNum l]
  partitionSpec seqNum (InRight r) =
    [InRight r | r <- partitionSpec seqNum r]

instance (GenSym l0 l1, GenSym r0 r1) => GenSym ((:|) l0 r0) ((:|) l1 r1) where
  fresh (InLeft l) = mrgFmap InLeft <$> fresh l
  fresh (InRight r) = mrgFmap InRight <$> fresh r

instance
  (GenSymSimple l0 l1, GenSymSimple r0 r1) =>
  GenSymSimple ((:|) l0 r0) ((:|) l1 r1)
  where
  simpleFresh (InLeft l) = InLeft <$> simpleFresh l
  simpleFresh (InRight r) = InRight <$> simpleFresh r

instance
  ( OpTyping l ctx,
    OpTyping r ctx,
    OpTypeType l ~ OpTypeType r
  ) =>
  OpTyping ((:|) l r) ctx
  where
  type OpTypeType ((:|) l r) = OpTypeType l
  typeOp (InLeft l) = typeOp l
  typeOp (InRight r) = typeOp r

instance
  ( OpSemantics semObj l val ctx,
    OpSemantics semObj r val ctx,
    OpTypeType l ~ OpTypeType r
  ) =>
  OpSemantics semObj ((:|) l r) val ctx
  where
  applyOp sem op (InLeft l) = applyOp sem op l
  applyOp sem op (InRight r) = applyOp sem op r

instance
  (OpReachableSymbols l, OpReachableSymbols r) =>
  OpReachableSymbols ((:|) l r)
  where
  opReachableSymbols (InLeft l) = opReachableSymbols l
  opReachableSymbols (InRight r) = opReachableSymbols r

instance
  (OpSymmetryReduction l, OpSymmetryReduction r) =>
  OpSymmetryReduction ((:|) l r)
  where
  opUnreorderable (InLeft l) (InLeft r) = opUnreorderable l r
  opUnreorderable (InRight l) (InRight r) = opUnreorderable l r
  opUnreorderable _ _ = true
  opCommutativeArgPos (InLeft l) = opCommutativeArgPos l
  opCommutativeArgPos (InRight r) = opCommutativeArgPos r

instance (OpPPrint l, OpPPrint r) => OpPPrint ((:|) l r) where
  describeArguments (InLeft l) =
    case describeArguments l of
      Left e -> Left $ fmap InLeft e
      Right args -> Right args
  describeArguments (InRight r) =
    case describeArguments r of
      Left e -> Left $ fmap InRight e
      Right args -> Right args
  prefixResults (InLeft l) = case prefixResults l of
    Left e -> Left $ fmap InLeft e
    Right args -> Right args
  prefixResults (InRight r) = case prefixResults r of
    Left e -> Left $ fmap InRight e
    Right args -> Right args
  pformatOp (InLeft l) = pformatOp l
  pformatOp (InRight r) = pformatOp r

instance (OpFlatten l fl, OpFlatten r fr) => OpFlatten ((:|) l r) ((:|) fl fr) where
  opForwardedSubProg (InLeft l) = fmap InLeft <$> opForwardedSubProg l
  opForwardedSubProg (InRight r) = fmap InRight <$> opForwardedSubProg r

instance
  ( OpCost costObj l cost ctx,
    OpCost costObj r cost ctx
  ) =>
  OpCost costObj ((:|) l r) cost ctx
  where
  opCost costObj table (InLeft l) = opCost costObj table l
  opCost costObj table (InRight r) = opCost costObj table r

instance (CountNumProgs l, CountNumProgs r) => CountNumProgs ((:|) l r) where
  countNumChoices (InLeft l) = countNumChoices l
  countNumChoices (InRight r) = countNumChoices r
  countNumProgs (InLeft l) = countNumProgs l
  countNumProgs (InRight r) = countNumProgs r
  countNumComponentChoices (InLeft l) = countNumComponentChoices l
  countNumComponentChoices (InRight r) = countNumComponentChoices r
  countNumInsts (InLeft l) = countNumInsts l
  countNumInsts (InRight r) = countNumInsts r

instance (SplitChoice l, SplitChoice r) => SplitChoice ((:|) l r) where
  splitChoice (InLeft l) = [InLeft l | l <- splitChoice l]
  splitChoice (InRight r) = [InRight r | r <- splitChoice r]

instance
  (ProgReachableSymbols l, ProgReachableSymbols r) =>
  ProgReachableSymbols ((:|) l r)
  where
  progReachableSymbols (InLeft l) = progReachableSymbols l
  progReachableSymbols (InRight r) = progReachableSymbols r

instance
  (OpParser l, OpParser r) =>
  OpParser ((:|) l r)
  where
  opParser = try (InLeft <$> opParser) <|> try (InRight <$> opParser)

instance
  (TypeParser l, TypeParser r) =>
  TypeParser ((:|) l r)
  where
  typeParser = try (InLeft <$> typeParser) <|> try (InRight <$> typeParser)

instance
  (ProgParser l, ProgParser r) =>
  ProgParser ((:|) l r)
  where
  progParser =
    try (second InLeft <$> progParser) <|> try (second InRight <$> progParser)
