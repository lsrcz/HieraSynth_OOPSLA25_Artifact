{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Circuit.ParallelFoldableTest (parallelFoldableTest) where

import Control.Monad (forM_)
import Data.Char (chr, ord)
import Grisette
  ( BV (bvSelect, bvZext),
    LogicalOp (false, true, (.&&), (.||)),
    SignConversion (toSigned),
    Solvable (isym),
    SomeSymWordN,
    SymBool,
    SymEq ((./=), (.==)),
    SymOrd ((.<), (.<=), (.>), (.>=)),
    symBV,
  )
import Grisette.Unified (EvalModeTag (S), symBitBlast, symFromBits)
import RVV.Circuit.ParallelFoldable
  ( ParallelFoldMethod (BrentKung, HanCarlson, KoggeStone, Sklansky),
    parallelAnd,
    parallelAndl,
    parallelAndr,
    parallelFoldl,
    parallelFoldr,
    parallelOr,
    parallelOrl,
    parallelOrr,
    parallelPrefixAdder,
    parallelPrefixEq,
    parallelPrefixNeq,
    parallelPrefixSge,
    parallelPrefixSgt,
    parallelPrefixSignedCompare,
    parallelPrefixSle,
    parallelPrefixSlt,
    parallelPrefixUge,
    parallelPrefixUgt,
    parallelPrefixUle,
    parallelPrefixUlt,
    parallelPrefixUnsignedCompare,
    parallelScanl1,
    parallelScanr1,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import TestUtil.SymbolicAssertion ((.@?=))

boolList :: Int -> [SymBool]
boolList n = [isym "a" n | n <- [0 .. n - 1]]

stringList :: Int -> [String]
stringList n = [[chr (n + ord 'a')] | n <- [0 .. n - 1]]

parallelFoldableTest :: Test
parallelFoldableTest =
  testGroup "ParallelFoldableTest" $ do
    method <-
      [ Sklansky,
        KoggeStone,
        BrentKung [],
        BrentKung [2, 3, 4],
        HanCarlson 0,
        HanCarlson 1,
        HanCarlson 2,
        HanCarlson 3
        ]
    return . testGroup (show method) $
      [ testCase "parallelAnd" $ do
          let test l = parallelAnd @'S method l .@?= foldl (.&&) true l
          test []
          test (boolList 1)
          test (boolList 17),
        testCase "parallelOr" $ do
          let test l = parallelOr @'S method l .@?= foldl (.||) false l
          test []
          test (boolList 1)
          test (boolList 17),
        testCase "parallelAndl" $ do
          let test l = parallelAndl @'S method l .@?= scanl1 (.&&) l
          test []
          test (boolList 1)
          test (boolList 17),
        testCase "parallelOrl" $ do
          let test l = parallelOrl @'S method l .@?= scanl1 (.||) l
          test []
          test (boolList 1)
          test (boolList 17),
        testCase "parallelAndr" $ do
          let test l = parallelAndr @'S method l .@?= scanr1 (.&&) l
          test []
          test (boolList 1)
          test (boolList 17),
        testCase "parallelOrr" $ do
          let test l = parallelOrr @'S method l .@?= scanr1 (.||) l
          test []
          test (boolList 1)
          test (boolList 17),
        testCase "parallelFoldl" $ do
          let test l = parallelFoldl method (<>) "x" (l :: [String]) @?= ("x" <> concat l)
          forM_ [0 .. 64] $ \n -> do
            test (stringList n),
        testCase "parallelFoldr" $ do
          let test l = parallelFoldr method (<>) "x" (l :: [String]) @?= foldr (<>) "x" l
          forM_ [0 .. 64] $ \n -> do
            test (stringList n),
        testCase "parallelScanl1" $ do
          let test l = parallelScanl1 method (<>) (l :: [String]) @?= scanl1 (<>) l
          forM_ [0 .. 64] $ \n -> do
            test (stringList n),
        testCase "parallelScanr1" $ do
          let test l = parallelScanr1 method (<>) (l :: [String]) @?= scanr1 (<>) l
          forM_ [0 .. 64] $ \n -> do
            test (stringList n),
        testCase "parallelPrefixAdder_simple" $ do
          let a = symBV 32 "b" :: SomeSymWordN
          let b = symBV 32 "c" :: SomeSymWordN
          let (_, sumWord) = parallelPrefixAdder method false a b
          sumWord .@?= (a + b),
        testCase "parallelPrefixAdder" $ do
          let initialCarry = "a" :: SymBool
          let a = symBV 32 "b" :: SomeSymWordN
          let b = symBV 32 "c" :: SomeSymWordN
          let (cout, sumWord) = parallelPrefixAdder method initialCarry a b
          let a33 = bvZext 33 a
          let b33 = bvZext 33 b
          let initialCarry33 = bvZext 33 $ symFromBits @'S [initialCarry]
          let r = a33 + b33 + initialCarry33
          let expectedSum = bvSelect 0 32 r
          let expectedCarry = head $ symBitBlast @'S $ bvSelect 32 1 r
          cout .@?= expectedCarry
          sumWord .@?= expectedSum,
        testCase "parallelPrefixUnsignedCompare" $ do
          let a = symBV 64 "a" :: SomeSymWordN
          let b = symBV 64 "b" :: SomeSymWordN
          let (equal, less, greater) = parallelPrefixUnsignedCompare method a b
          equal .@?= (a .== b)
          less .@?= (a .< b)
          greater .@?= (a .> b)
          parallelPrefixEq method a b .@?= (a .== b)
          parallelPrefixNeq method a b .@?= (a ./= b)
          parallelPrefixUlt method a b .@?= (a .< b)
          parallelPrefixUgt method a b .@?= (a .> b)
          parallelPrefixUle method a b .@?= (a .<= b)
          parallelPrefixUge method a b .@?= (a .>= b),
        testCase "parallelPrefixSignedCompare" $ do
          let a = symBV 64 "a" :: SomeSymWordN
          let b = symBV 64 "b" :: SomeSymWordN
          let (equal, less, greater) = parallelPrefixSignedCompare method a b
          equal .@?= (toSigned a .== toSigned b)
          less .@?= (toSigned a .< toSigned b)
          greater .@?= (toSigned a .> toSigned b)
          parallelPrefixEq method a b .@?= (toSigned a .== toSigned b)
          parallelPrefixNeq method a b .@?= (toSigned a ./= toSigned b)
          parallelPrefixSlt method a b .@?= (toSigned a .< toSigned b)
          parallelPrefixSgt method a b .@?= (toSigned a .> toSigned b)
          parallelPrefixSle method a b .@?= (toSigned a .<= toSigned b)
          parallelPrefixSge method a b .@?= (toSigned a .>= toSigned b)
      ]
