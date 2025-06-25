{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module HieraSynth.Program.SymbolTable
  ( SymbolTable (..),
    lookupSymbol,
    ProgReachableSymbols (..),
    transitivelyReachableSymbols,
    filterByReachableSymbols,
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.HashSet as HS
import Data.List ((\\))
import Data.List.Extra (intersperse)
import qualified Data.Text as T
import Grisette
  ( GenSymSimple (simpleFresh),
    PPrint (pformat),
    allClasses01,
    derive,
    hardline,
    pprintClasses,
  )
import HieraSynth.Context (ConcreteContext)
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
    lowestSeqNumList,
  )
import HieraSynth.Program.ProgPPrint (ProgPPrint (pformatProg))

newtype SymbolTable prog = SymbolTable [(T.Text, prog)]

derive [''SymbolTable] (allClasses01 \\ pprintClasses)

instance (ProgPPrint prog) => PPrint (SymbolTable prog) where
  pformat (SymbolTable lst) =
    mconcat $ intersperse hardline $ go <$> lst
    where
      go (key, prog) =
        case pformatProg key prog of
          Left err -> err
          Right doc -> doc

lookupSymbol :: SymbolTable prog -> T.Text -> ConcreteContext prog
lookupSymbol (SymbolTable table) symbol = go table
  where
    go [] = throwError $ "Symbol " <> symbol <> " not found"
    go ((sym, p) : rest) = if sym == symbol then return p else go rest

class ProgReachableSymbols prog where
  progReachableSymbols :: prog -> HS.HashSet T.Text

transitivelyReachableSymbols ::
  (ProgReachableSymbols prog) =>
  HS.HashSet T.Text ->
  SymbolTable prog ->
  ConcreteContext (HS.HashSet T.Text)
transitivelyReachableSymbols symbols table = go symbols symbols
  where
    go confirmed queue
      | HS.null queue = return confirmed
      | otherwise = do
          allNew <- fmap mconcat $ traverse (go1 confirmed) $ HS.toList queue
          go (HS.union confirmed allNew) allNew
    go1 confirmed s = do
      prog <- lookupSymbol table s
      let symbols = progReachableSymbols prog
      return $ HS.difference symbols confirmed

filterByReachableSymbols ::
  (ProgReachableSymbols prog) =>
  HS.HashSet T.Text ->
  SymbolTable prog ->
  ConcreteContext (SymbolTable prog)
filterByReachableSymbols symbols table@(SymbolTable t) = do
  progReachable <- transitivelyReachableSymbols symbols table
  let filtered = filter (\(s, _) -> HS.member s progReachable) t
  return $ SymbolTable filtered

instance (LowestSeqNum prog) => LowestSeqNum (SymbolTable prog) where
  lowestSeqNum succeeded (SymbolTable lst) =
    lowestSeqNumList succeeded $ snd <$> lst

instance (PartitionSpec prog) => PartitionSpec (SymbolTable prog) where
  partitionSpec seqNum (SymbolTable lst) =
    SymbolTable <$> go lst
    where
      go [] = [[]]
      go ((sym, prog) : rest) = do
        prog' <- partitionSpec seqNum prog
        res <- go rest
        return $ (sym, prog') : res

instance
  (GenSymSimple prog0 prog) =>
  GenSymSimple (SymbolTable prog0) (SymbolTable prog)
  where
  simpleFresh (SymbolTable lst) = do
    SymbolTable <$> traverse (\(sym, prog) -> (sym,) <$> simpleFresh prog) lst
