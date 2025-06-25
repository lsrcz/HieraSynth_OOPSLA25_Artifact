{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HieraSynth.Program.ProgUtil
  ( StmtUtil (..),
    ProgUtil (..),
    StmtUtilImpl (..),
    ProgUtilImpl (..),
  )
where

import Grisette (SymBool)
import HieraSynth.Context (MonadContext)

class StmtUtilImpl stmt op varId | stmt -> op varId where
  getStmtArgIds :: stmt -> [varId]
  getStmtResIds :: stmt -> [varId]
  getStmtOp :: stmt -> op
  getStmtDisabled :: stmt -> SymBool

class
  (StmtUtilImpl stmt (StmtOpType stmt) (StmtVarIdType stmt)) =>
  StmtUtil stmt
  where
  type StmtOpType stmt
  type StmtVarIdType stmt

class
  (StmtUtilImpl stmt op varId) =>
  ProgUtilImpl prog op stmt varId
    | prog -> stmt,
      prog -> varId
  where
  getProgArgIds :: prog -> [varId]
  getProgResIds :: prog -> [varId]
  getProgNumStmts :: prog -> Int
  getProgStmtAtIdx :: (MonadContext ctx) => prog -> Int -> ctx stmt

class
  ( ProgUtilImpl
      prog
      (StmtOpType (ProgStmtType prog))
      (ProgStmtType prog)
      (ProgVarIdType prog),
    StmtUtil (ProgStmtType prog),
    StmtVarIdType (ProgStmtType prog) ~ ProgVarIdType prog,
    ProgOpType prog ~ StmtOpType (ProgStmtType prog)
  ) =>
  ProgUtil prog
  where
  type ProgTypeType prog
  type ProgStmtType prog
  type ProgVarIdType prog
  type ProgOpType prog
