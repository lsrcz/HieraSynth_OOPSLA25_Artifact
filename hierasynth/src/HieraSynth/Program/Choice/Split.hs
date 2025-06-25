module HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    lowestSeqNumList,
    combineLowestSeqNum,
    partitionSpecList,
    PartitionSpec (partitionSpec),
  )
where

import Data.Maybe (catMaybes)
import HieraSynth.Program.Choice.ChoiceTree
  ( ChoiceMeta (NoSplit, Split),
    ChoiceTree (Branch, Leaf),
  )

combineLowestSeqNum :: [Maybe Int] -> Maybe Int
combineLowestSeqNum xs = case catMaybes xs of
  [] -> Nothing
  l -> Just $ minimum l

class LowestSeqNum spec where
  lowestSeqNum :: Bool -> spec -> Maybe Int

instance LowestSeqNum (ChoiceTree op) where
  lowestSeqNum _ (Leaf _) = Nothing
  lowestSeqNum succeeded (Branch (Split seqNum onlyWhenSuccess) subLists) =
    let subSeqNums = map (lowestSeqNum succeeded) subLists
        currentSeqNum =
          if not onlyWhenSuccess || succeeded then Just seqNum else Nothing
     in combineLowestSeqNum $ currentSeqNum : subSeqNums
  lowestSeqNum succeeded (Branch NoSplit subLists) =
    lowestSeqNumList succeeded subLists

lowestSeqNumList :: (LowestSeqNum spec) => Bool -> [spec] -> Maybe Int
lowestSeqNumList succeeded = combineLowestSeqNum . map (lowestSeqNum succeeded)

class PartitionSpec spec where
  partitionSpec :: Int -> spec -> [spec]

partitionSpecList :: (PartitionSpec spec) => Int -> [spec] -> [[spec]]
partitionSpecList _ [] = [[]]
partitionSpecList seqNum (t : ts) = do
  t' <- partitionSpec seqNum t
  ts' <- partitionSpecList seqNum ts
  return (t' : ts')

instance PartitionSpec (ChoiceTree op) where
  partitionSpec _ (Leaf ops) = [Leaf ops]
  partitionSpec seqNum (Branch (Split expectedSeqNum _) subLists)
    | seqNum == expectedSeqNum = do
        r <- subLists
        r1 <- partitionSpec seqNum r
        return $ Branch NoSplit [r1]
  partitionSpec seqNum (Branch meta subLists) = do
    r <- partitionSpecList seqNum subLists
    return $ Branch meta r
