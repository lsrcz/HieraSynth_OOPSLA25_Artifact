{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HieraSynth.Program.Concrete.Builder
  ( buildProg,
    noden,
    node1,
    node2,
    node3,
    node4,
    node5,
    node6,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (mplus, mzero))
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified HieraSynth.Program.Concrete.Program as Concrete
import HieraSynth.VarId (ConcreteVarId)

newtype NodeRef = NodeRef
  { nodeVarId :: Int
  }
  deriving (Generic, Show, Eq)

data SimpleStmt op = SimpleStmt
  { simpleStmtOp :: op,
    simpleStmtArgIds :: [Int],
    simpleStmtResIds :: [Int]
  }
  deriving (Generic)

data BuilderState op = BuilderState
  { nextVarId :: Int,
    stmtList :: [SimpleStmt op]
  }
  deriving (Generic)

newtype Builder op a = Builder
  { runBuilder :: BuilderState op -> Either String (a, BuilderState op)
  }

instance Functor (Builder op) where
  fmap f (Builder m) = Builder $ \s -> case m s of
    Left err -> Left err
    Right (a, s') -> Right (f a, s')

instance Applicative (Builder op) where
  pure a = Builder $ \s -> Right (a, s)
  Builder f <*> Builder m = Builder $ \s -> case f s of
    Left err -> Left err
    Right (fab, s') -> case m s' of
      Left err -> Left err
      Right (a, s'') -> Right (fab a, s'')

instance Monad (Builder op) where
  Builder m >>= f = Builder $ \s -> case m s of
    Left err -> Left err
    Right (a, s') -> runBuilder (f a) s'

instance MonadFail (Builder op) where
  fail msg = Builder $ \_ -> Left msg

instance Alternative (Builder op) where
  empty = Builder $ \_ -> Left "empty"
  Builder m1 <|> Builder m2 = Builder $ \s -> case m1 s of
    Left _ -> m2 s
    Right r -> Right r

instance MonadPlus (Builder op) where
  mzero = empty
  mplus = (<|>)

-- Get the current state
get :: Builder op (BuilderState op)
get = Builder $ \s -> Right (s, s)

-- Put a new state
put :: BuilderState op -> Builder op ()
put s = Builder $ \_ -> Right ((), s)

-- | Build a program using the monadic builder interface
buildProg ::
  forall varId op ty.
  (ConcreteVarId varId) =>
  [(T.Text, ty)] ->
  ([NodeRef] -> Builder op [(NodeRef, ty)]) ->
  Concrete.Prog op varId ty
buildProg argPairs builderAction =
  case runBuilder (builderAction argRefs) initialState of
    Left err -> error $ "Builder failed: " ++ err
    Right (resultNodes, finalState) ->
      Concrete.Prog
        ( fmap
            ( \(i, (name, ty)) ->
                Concrete.ProgArg
                  { Concrete.progArgName = name,
                    Concrete.progArgId = fromIntegral i,
                    Concrete.progArgType = ty
                  }
            )
            (zip [0 ..] argPairs)
        )
        ( fmap
            ( \simpleStmt ->
                Concrete.Stmt
                  { Concrete.stmtOp = simpleStmtOp simpleStmt,
                    Concrete.stmtArgIds = fromIntegral <$> simpleStmtArgIds simpleStmt,
                    Concrete.stmtResIds = fromIntegral <$> simpleStmtResIds simpleStmt
                  }
            )
            (stmtList finalState)
        )
        ( fmap
            ( \(NodeRef varId, ty) ->
                Concrete.ProgRes
                  { Concrete.progResId = fromIntegral varId,
                    Concrete.progResType = ty
                  }
            )
            resultNodes
        )
  where
    argRefs =
      [ NodeRef i
      | i <- [0 .. length argPairs - 1]
      ]

    initialState =
      BuilderState
        { nextVarId = length argPairs,
          stmtList = []
        }

-- | Create a node with the given operator, arguments.
noden :: op -> Int -> [NodeRef] -> Builder op [NodeRef]
noden op numResults argRefs = do
  state <- get
  let startVarId = nextVarId state
  let resultVarIds = [startVarId .. startVarId + numResults - 1]
  let stmt =
        SimpleStmt
          { simpleStmtOp = op,
            simpleStmtArgIds = nodeVarId <$> argRefs,
            simpleStmtResIds = resultVarIds
          }
  put $
    BuilderState
      { nextVarId = startVarId + numResults,
        stmtList = stmtList state ++ [stmt]
      }
  return [NodeRef varId | varId <- resultVarIds]

node1 :: op -> [NodeRef] -> Builder op NodeRef
node1 op argRefs = do
  [r] <- noden op 1 argRefs
  return r

node2 :: op -> [NodeRef] -> Builder op (NodeRef, NodeRef)
node2 op argRefs = do
  [r1, r2] <- noden op 2 argRefs
  return (r1, r2)

node3 :: op -> [NodeRef] -> Builder op (NodeRef, NodeRef, NodeRef)
node3 op argRefs = do
  [r1, r2, r3] <- noden op 3 argRefs
  return (r1, r2, r3)

node4 :: op -> [NodeRef] -> Builder op (NodeRef, NodeRef, NodeRef, NodeRef)
node4 op argRefs = do
  [r1, r2, r3, r4] <- noden op 4 argRefs
  return (r1, r2, r3, r4)

node5 :: op -> [NodeRef] -> Builder op (NodeRef, NodeRef, NodeRef, NodeRef, NodeRef)
node5 op argRefs = do
  [r1, r2, r3, r4, r5] <- noden op 5 argRefs
  return (r1, r2, r3, r4, r5)

node6 :: op -> [NodeRef] -> Builder op (NodeRef, NodeRef, NodeRef, NodeRef, NodeRef, NodeRef)
node6 op argRefs = do
  [r1, r2, r3, r4, r5, r6] <- noden op 6 argRefs
  return (r1, r2, r3, r4, r5, r6)
