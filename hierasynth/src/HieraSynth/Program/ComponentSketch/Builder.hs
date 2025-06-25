{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HieraSynth.Program.ComponentSketch.Builder
  ( StmtExtraConstraint (..),
    simpleFreshStmt,
    simpleFreshStmt',
    simpleFreshStmts,
    simpleFreshStmts',
    freshStmt,
    freshStmt',
    freshStmts,
    freshStmts',
    MkProg (..),
    MkFreshProg (..),
    mkSimpleFreshProg,
    fromConcrete,
    mkSketch,
    mkSimpleSketch,
  )
where

import Control.Monad (replicateM)
import qualified Data.Text as T
import Grisette
  ( Fresh,
    GenSymSimple (simpleFresh),
    Identifier,
    LogicalOp ((.||)),
    Mergeable,
    Solvable (con),
    ToSym (toSym),
    runFresh,
  )
import HieraSynth.Context (SymbolicContext)
import HieraSynth.Operator.OpTyping
  ( OpTyping,
    symOpMaximumArgNum,
    symOpMaximumResNum,
  )
import HieraSynth.Program.ComponentSketch.Program
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt, stmtDisabled, stmtMustBeAfter, stmtResIds),
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Util.Show (showAsText)

newtype StmtExtraConstraint op symVarId = StmtExtraConstraint
  { stmtMustBeAfterStmts :: [Stmt op symVarId]
  }

simpleFreshStmt' ::
  ( OpTyping op SymbolicContext,
    GenSymSimple () symVarId
  ) =>
  op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
simpleFreshStmt' op (StmtExtraConstraint mustBeAfterStmts) = do
  let maxArgNum = symOpMaximumArgNum op
  argIds <- replicateM maxArgNum (simpleFresh ())
  argNum <- simpleFresh ()
  let maxResNum = symOpMaximumResNum op
  resIds <- replicateM maxResNum (simpleFresh ())
  resNum <- simpleFresh ()
  disabled <- simpleFresh ()
  return
    [ Stmt op argIds argNum resIds resNum disabled $
        concatMap stmtResIds mustBeAfterStmts
    ]

augmentMustBeAfterForStmts ::
  [Stmt op symVarId] -> [Stmt op symVarId]
augmentMustBeAfterForStmts = go []
  where
    go _ [] = []
    go resIds (stmt : stmts) =
      stmt {stmtMustBeAfter = stmtMustBeAfter stmt ++ resIds}
        : go (resIds ++ stmtResIds stmt) stmts

augmentDisabled ::
  [Stmt op symVarId] -> [Stmt op symVarId]
augmentDisabled = go (con False)
  where
    go _ [] = []
    go disabled (stmt : stmts) =
      let newDisabled = stmtDisabled stmt .|| disabled
       in stmt {stmtDisabled = newDisabled} : go newDisabled stmts

simpleFreshStmts' ::
  (OpTyping op SymbolicContext, GenSymSimple () symVarId) =>
  Int ->
  op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
simpleFreshStmts' num op extraConstraint = do
  stmts <- concat <$> replicateM num (simpleFreshStmt' op extraConstraint)
  return $ augmentDisabled $ augmentMustBeAfterForStmts stmts

simpleFreshStmt ::
  (OpTyping op SymbolicContext, GenSymSimple () symVarId) =>
  op ->
  Fresh [Stmt op symVarId]
simpleFreshStmt op = simpleFreshStmt' op (StmtExtraConstraint [])

simpleFreshStmts ::
  (OpTyping op SymbolicContext, GenSymSimple () symVarId) =>
  Int ->
  op ->
  Fresh [Stmt op symVarId]
simpleFreshStmts num op =
  simpleFreshStmts' num op (StmtExtraConstraint [])

freshStmt' ::
  ( OpTyping op SymbolicContext,
    Mergeable op,
    GenSymSimple () symVarId
  ) =>
  Fresh op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
freshStmt' freshOp extraConstraint = do
  op <- freshOp
  simpleFreshStmt' op extraConstraint

freshStmts' ::
  ( OpTyping op SymbolicContext,
    Mergeable op,
    GenSymSimple () symVarId
  ) =>
  Int ->
  Fresh op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
freshStmts' num freshOp extraConstraint = do
  stmts <- concat <$> replicateM num (freshStmt' freshOp extraConstraint)
  return $ augmentDisabled $ augmentMustBeAfterForStmts stmts

freshStmt ::
  ( OpTyping op SymbolicContext,
    Mergeable op,
    GenSymSimple () symVarId
  ) =>
  Fresh op ->
  Fresh [Stmt op symVarId]
freshStmt freshOp = freshStmt' freshOp (StmtExtraConstraint [])

freshStmts ::
  ( OpTyping op SymbolicContext,
    Mergeable op,
    GenSymSimple () symVarId
  ) =>
  Int ->
  Fresh op ->
  Fresh [Stmt op symVarId]
freshStmts num freshOp =
  freshStmts' num freshOp (StmtExtraConstraint [])

class MkProg prog stmts op symVarId ty | prog -> stmts op symVarId ty where
  mkProg ::
    [(T.Text, ty)] ->
    stmts ->
    [(symVarId, ty)] ->
    prog

instance MkProg (Prog op symVarId ty) [Stmt op symVarId] op symVarId ty where
  mkProg args stmts rets =
    Prog (uncurry ProgArg <$> args) stmts (uncurry ProgRes <$> rets)

instance
  MkProg
    (Fresh (Prog op symVarId ty))
    [Fresh [Stmt op symVarId]]
    (Fresh op)
    (Fresh symVarId)
    ty
  where
  mkProg args stmts rets =
    (Prog (uncurry ProgArg <$> args) . concat <$> sequence stmts)
      <*> traverse
        (\(freshVarId, ty) -> ProgRes <$> freshVarId <*> return ty)
        rets

class
  MkFreshProg prog stmt op symVarId ty
    | prog -> stmt op ty symVarId,
      stmt ty -> prog
  where
  mkFreshProg :: [ty] -> [Fresh [stmt]] -> [ty] -> Fresh prog

mkSimpleFreshProg ::
  forall op ty symVarId.
  ( OpTyping op SymbolicContext,
    GenSymSimple () symVarId
  ) =>
  [ty] ->
  [op] ->
  [ty] ->
  Fresh (Prog op symVarId ty)
mkSimpleFreshProg argTypes ops =
  mkFreshProg argTypes (simpleFreshStmt <$> ops :: [Fresh [Stmt op symVarId]])

instance
  (GenSymSimple () symVarId) =>
  MkFreshProg (Prog op symVarId ty) (Stmt op symVarId) op symVarId ty
  where
  mkFreshProg argTypes freshStmts retTypes =
    ( ( Prog
          [ProgArg ("arg" <> showAsText n) ty | (n, ty) <- zip [0 ..] argTypes]
      )
        . concat
        <$> sequence freshStmts
    )
      <*> sequence [ProgRes <$> simpleFresh () <*> return ty | ty <- retTypes]

fromConcrete ::
  forall symOp symVarId symTy conOp conVarId conTy.
  ( ToSym conTy symTy,
    ToSym conOp symOp,
    OpTyping symOp SymbolicContext,
    GenSymSimple () symVarId
  ) =>
  Concrete.Prog conOp conVarId conTy ->
  Fresh (Prog symOp symVarId symTy)
fromConcrete (Concrete.Prog argList stmtList resList) = do
  stmts <- concat <$> allFreshStmts
  Prog args stmts <$> freshResults
  where
    args =
      (\(Concrete.ProgArg name _ ty) -> ProgArg name (toSym ty)) <$> argList
    allFreshStmts =
      traverse
        (\(Concrete.Stmt op _ _) -> simpleFreshStmt (toSym op :: symOp))
        stmtList
    freshResults =
      traverse
        ( \(Concrete.ProgRes _ ty) -> do
            freshId <- simpleFresh ()
            return $ ProgRes freshId (toSym ty)
        )
        resList

mkSketch ::
  forall prog stmt op symVarId ty.
  (MkFreshProg prog stmt op symVarId ty) =>
  Identifier ->
  [ty] ->
  [Fresh [stmt]] ->
  [ty] ->
  prog
mkSketch ident argTypes stmts retTypes =
  flip runFresh ident $
    mkFreshProg argTypes stmts retTypes

mkSimpleSketch ::
  forall op ty symVarId.
  (OpTyping op SymbolicContext, GenSymSimple () symVarId) =>
  Identifier ->
  [ty] ->
  [op] ->
  [ty] ->
  Prog op symVarId ty
mkSimpleSketch ident argTypes ops retTypes =
  flip runFresh ident $
    mkSimpleFreshProg argTypes ops retTypes
