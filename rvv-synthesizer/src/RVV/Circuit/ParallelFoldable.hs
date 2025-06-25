{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Circuit.ParallelFoldable
  ( ParallelFoldMethod (..),
    parallelScanl1,
    parallelScanr1,
    parallelFoldl,
    parallelFoldr,
    parallelAndl,
    parallelOrl,
    parallelAndr,
    parallelOrr,
    parallelAnd,
    parallelOr,
    parallelPrefixAdder,
    parallelPrefixUnsignedCompare,
    parallelPrefixSignedCompare,
    parallelPrefixEq,
    parallelPrefixNeq,
    parallelPrefixUlt,
    parallelPrefixUgt,
    parallelPrefixUle,
    parallelPrefixUge,
    parallelPrefixSlt,
    parallelPrefixSle,
    parallelPrefixSgt,
    parallelPrefixSge,
  )
where

import Control.Monad.ST.Strict (ST, runST)
import Data.Foldable (forM_, traverse_)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Grisette (LogicalOp (false, symXor, true, (.||)), symNot, (.&&))
import Grisette.Internal.Unified.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified (GetSomeWordN, symBitBlast, symFromBits, (.==))
import RVV.EvalMode (EvalMode)

data ParallelFoldMethod
  = Sklansky
  | KoggeStone
  | BrentKung [Int]
  | HanCarlson Int
  deriving (Show)

sklanskyScanl1ImplST ::
  (a -> a -> a) ->
  MV.STVector s a ->
  ST s ()
sklanskyScanl1ImplST f v = go 1
  where
    go skip
      | skip >= MV.length v = return ()
      | otherwise = do
          let offsets = [0 .. skip - 1]
          let starts =
                takeWhile (< MV.length v) [skip * (i * 2 + 1) | i <- [0 ..]]
          traverse_ (update offsets) starts
          go (skip * 2)
    update offsets start = do
      let indices = filter (< MV.length v) [start + i | i <- offsets]
      traverse_ (update' start) indices
    update' start index = do
      v1 <- MV.read v index
      v2 <- MV.read v (start - 1)
      MV.write v index (f v2 v1)

koggeStoneScanl1ImplSTStep ::
  Int ->
  Int ->
  (a -> a -> a) ->
  MV.STVector s a ->
  Int ->
  ST s ()
koggeStoneScanl1ImplSTStep space offset f v step = do
  let len = MV.length v
  let targets = takeWhile (< len) [offset + step + space * i | i <- [0 ..]]
  forM_ (reverse targets) $ \j -> do
    prev <- MV.read v (j - step)
    current <- MV.read v j
    MV.write v j (f prev current)

koggeStoneScanl1ImplST ::
  (a -> a -> a) ->
  MV.STVector s a ->
  ST s ()
koggeStoneScanl1ImplST f v = do
  let len = MV.length v
  let steps = takeWhile (< len) [2 ^ i | i <- [0 ..]]
  forM_ steps $ koggeStoneScanl1ImplSTStep 1 0 f v

brentKungScanl1ImplSTReductionStep ::
  Int ->
  (a -> a -> a) ->
  MV.STVector s a ->
  Int ->
  ST s ()
brentKungScanl1ImplSTReductionStep radix f v step = do
  let len = MV.length v
  let target =
        reverse $
          takeWhile ((< len) . snd) $
            concat
              [ [(r, step * (radix * i + r + 1) - 1) | r <- [1 .. radix - 1]]
                | i <- [0 ..]
              ]
  forM_ target $ \(r, j) -> do
    values <- mapM (MV.read v) [j - step * c | c <- reverse [0 .. r]]
    MV.write v j $ foldl1 f values

brentKungScanl1ImplSTPropagationStep ::
  Int ->
  (a -> a -> a) ->
  MV.STVector s a ->
  Int ->
  ST s ()
brentKungScanl1ImplSTPropagationStep radix f v step = do
  let len = MV.length v
  let sources = takeWhile (< len) [radix * step * (i + 1) - 1 | i <- [0 ..]]
  forM_ (reverse sources) $ \source -> do
    let targets = takeWhile (< len) [source + step * i | i <- [1 .. radix - 1]]
    forM_ (reverse targets) $ \target -> do
      prev <- MV.read v source
      current <- MV.read v target
      MV.write v target (f prev current)

brentKungScanl1ImplST ::
  [Int] ->
  (a -> a -> a) ->
  MV.STVector s a ->
  ST s ()
brentKungScanl1ImplST radixes f v = do
  let augmentedRadixes = radixes ++ repeat 2
  let len = MV.length v
  let steps = takeWhile (< len) $ 1 : scanl1 (*) augmentedRadixes
  forM_ (zip steps augmentedRadixes) $ \(step, radix) ->
    brentKungScanl1ImplSTReductionStep radix f v step
  forM_ (reverse $ zip steps augmentedRadixes) $ \(step, radix) ->
    brentKungScanl1ImplSTPropagationStep radix f v step

hanCarlsonScanl1ImplST ::
  Int ->
  (a -> a -> a) ->
  MV.STVector s a ->
  ST s ()
hanCarlsonScanl1ImplST n f v | n <= 0 = koggeStoneScanl1ImplST f v
hanCarlsonScanl1ImplST n f v = do
  let len = MV.length v
  let brentKungRadixes = replicate n 2
  let brentKungSteps = 1 : scanl1 (*) brentKungRadixes
  if any (>= len) brentKungSteps
    then brentKungScanl1ImplST brentKungRadixes f v
    else do
      let koggeStoneInitialStep = last brentKungSteps
      let koggeStoneSteps =
            if null brentKungSteps
              then []
              else takeWhile (< len) $ iterate (* 2) koggeStoneInitialStep
      forM_ (zip brentKungSteps brentKungRadixes) $ \(step, radix) ->
        brentKungScanl1ImplSTReductionStep radix f v step
      forM_ koggeStoneSteps $
        koggeStoneScanl1ImplSTStep
          koggeStoneInitialStep
          (koggeStoneInitialStep - 1)
          f
          v
      forM_ (reverse $ zip brentKungSteps brentKungRadixes) $ \(step, radix) ->
        brentKungScanl1ImplSTPropagationStep radix f v step

parallelScanl1ImplST ::
  ParallelFoldMethod ->
  (a -> a -> a) ->
  MV.STVector s a ->
  ST s ()
parallelScanl1ImplST Sklansky = sklanskyScanl1ImplST
parallelScanl1ImplST KoggeStone = koggeStoneScanl1ImplST
parallelScanl1ImplST (BrentKung radixes) = brentKungScanl1ImplST radixes
parallelScanl1ImplST (HanCarlson radixes) = hanCarlsonScanl1ImplST radixes

parallelScanl1 ::
  ParallelFoldMethod ->
  (a -> a -> a) ->
  [a] ->
  [a]
parallelScanl1 method f l = runST $ do
  v <- V.thaw $ V.fromList l
  parallelScanl1ImplST method f v
  V.toList <$> V.freeze v

parallelFoldl ::
  ParallelFoldMethod ->
  (a -> a -> a) ->
  a ->
  [a] ->
  a
parallelFoldl method f empty l = last $ parallelScanl1 method f $ empty : l

parallelFoldr ::
  ParallelFoldMethod ->
  (a -> a -> a) ->
  a ->
  [a] ->
  a
parallelFoldr method f empty l = head $ parallelScanr1 method f $ l ++ [empty]

parallelAnd ::
  (EvalMode mode) =>
  ParallelFoldMethod ->
  [GetBool mode] ->
  GetBool mode
parallelAnd method = parallelFoldl method (.&&) true

parallelOr ::
  (EvalMode mode) =>
  ParallelFoldMethod ->
  [GetBool mode] ->
  GetBool mode
parallelOr method = parallelFoldl method (.||) false

parallelScanr1 ::
  ParallelFoldMethod ->
  (a -> a -> a) ->
  [a] ->
  [a]
parallelScanr1 method f l = reverse $ parallelScanl1 method (flip f) $ reverse l

parallelAndl ::
  (EvalMode mode) =>
  ParallelFoldMethod ->
  [GetBool mode] ->
  [GetBool mode]
parallelAndl method = parallelScanl1 method (.&&)

parallelOrl ::
  (EvalMode mode) =>
  ParallelFoldMethod ->
  [GetBool mode] ->
  [GetBool mode]
parallelOrl method = parallelScanl1 method (.||)

parallelAndr ::
  (EvalMode mode) =>
  ParallelFoldMethod ->
  [GetBool mode] ->
  [GetBool mode]
parallelAndr method = parallelScanr1 method (.&&)

parallelOrr ::
  (EvalMode mode) =>
  ParallelFoldMethod ->
  [GetBool mode] ->
  [GetBool mode]
parallelOrr method = parallelScanr1 method (.||)

parallelPrefixAdder ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetBool mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  (GetBool mode, GetSomeWordN mode)
parallelPrefixAdder method initialCarry l r = (cout, sumWord)
  where
    lb = symBitBlast @mode l
    rb = symBitBlast @mode r
    gp lbit rbit = (lbit .&& rbit, symXor lbit rbit)
    combine (g2, p2) (g1, p1) = (g1 .|| (p1 .&& g2), p1 .&& p2)
    initialGP = (initialCarry, true)
    restGPs = zipWith gp lb rb
    originalPs = map snd restGPs -- Capture original propagate bits
    combined = parallelScanl1 method combine (initialGP : restGPs)
    gBits = fmap fst combined
    -- Calculate sum using original propagate bits and carry chain
    sumBits = zipWith symXor originalPs (init gBits) -- gBits[0..n-1] are carry-ins
    cout = last gBits
    sumWord = symFromBits @mode sumBits

parallelPrefixUnsignedCompare ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  (GetBool mode, GetBool mode, GetBool mode)
parallelPrefixUnsignedCompare method l r = (equal, less, greater)
  where
    -- Process bits from MSB to LSB for correct unsigned comparison
    lb = symBitBlast @mode l
    rb = symBitBlast @mode r

    -- Fixed bit comparison: swapped less/greater logic
    bitCmp :: GetBool mode -> GetBool mode -> (GetBool mode, GetBool mode, GetBool mode)
    bitCmp lbit rbit = (lbit .== rbit, symNot lbit .&& rbit, lbit .&& symNot rbit)

    combine (e2, l2, g2) (e1, l1, g1) =
      ( e1 .&& e2,
        l1 .|| (e1 .&& l2), -- Propagate less condition
        g1 .|| (e1 .&& g2) -- Propagate greater condition
      )

    combined = parallelScanl1 method combine $ zipWith bitCmp lb rb

    (_, less, greater) = last combined
    equal = symNot less .&& symNot greater

parallelPrefixSignedCompare ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  (GetBool mode, GetBool mode, GetBool mode)
parallelPrefixSignedCompare method l r =
  ( equal,
    mustLess .|| (symNot mustGreater .&& less),
    mustGreater .|| (symNot mustLess .&& greater)
  )
  where
    lSign = last $ symBitBlast @mode l
    rSign = last $ symBitBlast @mode r
    mustLess = lSign .&& symNot rSign
    mustGreater = symNot lSign .&& rSign
    equal, less, greater :: GetBool mode
    (equal, less, greater) = parallelPrefixUnsignedCompare @mode method l r

parallelPrefixEq ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixEq method l r =
  let (equal, _, _) = parallelPrefixUnsignedCompare @mode method l r
   in equal

parallelPrefixNeq ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixNeq method l r = symNot $ parallelPrefixEq @mode method l r

parallelPrefixUlt ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixUlt method l r =
  let (_, less, _) = parallelPrefixUnsignedCompare @mode method l r
   in less

parallelPrefixUgt ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixUgt method l r =
  let (_, _, greater) = parallelPrefixUnsignedCompare @mode method l r
   in greater

parallelPrefixUle ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixUle method l r =
  let (_, _, greater) = parallelPrefixUnsignedCompare @mode method l r
   in symNot greater

parallelPrefixUge ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixUge method l r =
  let (_, less, _) = parallelPrefixUnsignedCompare @mode method l r
   in symNot less

parallelPrefixSlt ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixSlt method l r =
  let (_, less, _) = parallelPrefixSignedCompare @mode method l r
   in less

parallelPrefixSle ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixSle method l r =
  let (_, _, greater) = parallelPrefixSignedCompare @mode method l r
   in symNot greater

parallelPrefixSgt ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixSgt method l r =
  let (_, _, greater) = parallelPrefixSignedCompare @mode method l r
   in greater

parallelPrefixSge ::
  forall mode.
  (EvalMode mode) =>
  ParallelFoldMethod ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
parallelPrefixSge method l r =
  let (_, less, _) = parallelPrefixSignedCompare @mode method l r
   in symNot less
