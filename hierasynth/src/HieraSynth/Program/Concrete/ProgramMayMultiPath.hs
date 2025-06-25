{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HieraSynth.Program.Concrete.ProgramMayMultiPath
  ( ProgMayMultiPath (..),
  )
where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State
  ( MonadState (get),
    MonadTrans (lift),
    StateT,
    evalStateT,
  )
import qualified Data.HashMap.Lazy as HM
import Grisette
  ( Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy, SortedStrategy),
    MonadUnion,
    SimpleMergeable (mrgIte),
    Union,
    liftToMonadUnion,
    merge,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Control.Monad.State.Class (mrgPut)
import Grisette.Lib.Data.Foldable (mrgTraverse_)
import Grisette.Lib.Data.Traversable (mrgTraverse)
import HieraSynth.Context (MonadContext)
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType))
import HieraSynth.Program.Concrete.Program
  ( Prog (Prog),
    ProgArg (progArgId),
    ProgRes (progResId),
    Stmt (Stmt),
  )
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
  )
import HieraSynth.Util.Show (showAsText)
import HieraSynth.VarId (ConcreteVarId)

newtype ProgMayMultiPath op varId ty = ProgMayMultiPath (Prog op varId ty)

newtype MayMultiPathEnv varId val
  = MayMultiPathEnv (HM.HashMap varId (Union val))

instance
  (ConcreteVarId conVarId, Mergeable val) =>
  Mergeable (MayMultiPathEnv conVarId val)
  where
  rootStrategy =
    SortedStrategy
      (\(MayMultiPathEnv m) -> HM.keysSet m)
      ( const $ SimpleStrategy $ \c (MayMultiPathEnv l) (MayMultiPathEnv r) ->
          MayMultiPathEnv $
            HM.mapWithKey
              (\k v -> mrgIte c v (r HM.! k))
              l
      )

addValMayMultiPath ::
  (ConcreteVarId varId, MonadContext ctx, MonadUnion ctx, Mergeable val) =>
  varId ->
  val ->
  StateT (MayMultiPathEnv varId val) ctx ()
addValMayMultiPath varId val = do
  MayMultiPathEnv env <- get
  when (HM.member varId env) . mrgThrowError $
    "Variable " <> showAsText varId <> " is already defined."
  mrgPut $ MayMultiPathEnv $ HM.insert varId (mrgReturn val) env

lookupValMayMultiPath ::
  (ConcreteVarId varId, MonadContext ctx, MonadUnion ctx, Mergeable val) =>
  varId ->
  StateT (MayMultiPathEnv varId val) ctx val
lookupValMayMultiPath varId = do
  MayMultiPathEnv env <- get
  case HM.lookup varId env of
    Nothing -> mrgThrowError $ "Variable " <> showAsText varId <> " is undefined."
    Just val -> liftToMonadUnion val

instance (Mergeable ty) => ProgTyping (ProgMayMultiPath op varId ty) where
  typeProg (ProgMayMultiPath prog) = typeProg prog

instance
  ( OpSemantics semObj op val ctx,
    ConcreteVarId varId,
    MonadUnion ctx,
    Mergeable val,
    Mergeable ty,
    ty ~ OpTypeType op
  ) =>
  ProgSemantics semObj (ProgMayMultiPath op varId ty) val ctx
  where
  runProg sem table (ProgMayMultiPath (Prog arg stmts ret)) inputs = merge $ do
    when (length inputs /= length arg) . mrgThrowError $
      "Expected "
        <> showAsText (length arg)
        <> " arguments, but got "
        <> showAsText (length inputs)
        <> " arguments."
    let initialEnv =
          MayMultiPathEnv . HM.fromList . zip (progArgId <$> arg) $
            mrgReturn <$> inputs
    let runStmt (Stmt op argIds resIds) = do
          args <- mrgTraverse lookupValMayMultiPath argIds
          res <- lift $ applyOp sem table op args
          when (length res /= length resIds) . throwError $
            "Incorrect number of results."
          mrgTraverse_ (uncurry addValMayMultiPath) $ zip resIds res
    flip evalStateT initialEnv $ do
      mrgTraverse_ runStmt stmts
      mrgTraverse (lookupValMayMultiPath . progResId) ret

instance ProgUtilImpl (ProgMayMultiPath op varId ty) op (Stmt op varId) varId where
  getProgArgIds (ProgMayMultiPath prog) = getProgArgIds prog
  getProgResIds (ProgMayMultiPath prog) = getProgResIds prog
  getProgNumStmts (ProgMayMultiPath prog) = getProgNumStmts prog
  getProgStmtAtIdx (ProgMayMultiPath prog) = getProgStmtAtIdx prog

instance ProgUtil (ProgMayMultiPath op varId ty) where
  type ProgTypeType (ProgMayMultiPath op varId ty) = ty
  type ProgStmtType (ProgMayMultiPath op varId ty) = Stmt op varId
  type ProgVarIdType (ProgMayMultiPath op varId ty) = varId
  type ProgOpType (ProgMayMultiPath op varId ty) = op
