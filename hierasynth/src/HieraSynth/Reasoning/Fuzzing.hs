{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HieraSynth.Reasoning.Fuzzing
  ( fuzzingTestProg,
    fuzzingTestSymProgWithModel,
    QuickCheckFuzzer (..),
    defaultQuickCheckFuzzer,
    defaultSemQuickCheckFuzzer,
  )
where

import Control.Exception.Safe (Exception (fromException), throw)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Grisette
  ( EvalSym,
    Mergeable,
    Model,
    ToCon,
    ToSym,
    VerifierResult (CEGISVerifierFoundCex, CEGISVerifierNoCex),
    evalSymToCon,
  )
import HieraSynth.Context (AngelicContext, ConcreteContext)
import HieraSynth.Operator.OpSemantics (DefaultSem (DefaultSem))
import HieraSynth.Program.ProgSemantics
  ( EvaledSymbolTable,
    ProgSemantics,
    evalSymbolTable,
    runEvaledSymbol,
  )
import HieraSynth.Program.ProgUtil (ProgUtil)
import HieraSynth.Program.SymbolTable (SymbolTable)
import HieraSynth.Reasoning.IOPair (IOPair (IOPair))
import HieraSynth.Reasoning.Matcher
  ( EqMatcher (EqMatcher),
    Matcher (match),
  )
import HieraSynth.Reasoning.Synthesis
  ( ConExampleConstraint,
    Example (Example),
    IsVerifier (toVerifierFuns),
    SomeExample (SomeExample),
    SomeVerifier (SomeVerifier),
    SymExampleConstraint,
  )
import HieraSynth.Util.Exception (CancellingException)
import Test.QuickCheck.Counterexamples
  ( Args (chatty),
    Gen,
    PropertyOf,
    Result (Failure, theException),
    Testable (Counterexample),
    discard,
    forAll,
    quickCheckWithResult,
    stdArgs,
    withMaxSuccess,
    type (:&:) ((:&:)),
  )

fuzzingTestProg' ::
  ( Matcher matcher Bool conVal,
    Show conVal,
    Testable prop,
    Counterexample prop ~ ([conVal] :&: ())
  ) =>
  prop ->
  ([conVal] -> ConcreteContext ([conVal], matcher)) ->
  IO (Maybe (IOPair conVal, matcher))
fuzzingTestProg' prop spec = do
  (maybeCex, result) <- quickCheckWithResult stdArgs {chatty = False} prop
  case maybeCex of
    Nothing -> return Nothing
    Just (cex :&: _) -> do
      case result of
        Failure {theException = Just e} ->
          case fromException e of
            Just (se :: CancellingException) -> throw se
            _ -> return ()
        _ -> return ()
      let Right (outputs, matcher) = spec cex
      return $ Just (IOPair cex outputs, matcher)

propertyWithMatcher ::
  ( Matcher matcher Bool conVal,
    Show conVal,
    Mergeable conVal
  ) =>
  Gen [conVal] ->
  ([conVal] -> ConcreteContext ([conVal], matcher)) ->
  Int ->
  EvaledSymbolTable conVal ConcreteContext ->
  T.Text ->
  PropertyOf ([conVal] :&: ())
propertyWithMatcher gen spec maxTests table symbol = forAll gen $ \inputs ->
  withMaxSuccess maxTests $
    case (spec inputs, runEvaledSymbol table symbol inputs) of
      (Left _, _) -> discard
      (_, Left _) -> False
      (Right (expectedOutputs, matcher), Right actualOutputs) ->
        match matcher actualOutputs expectedOutputs

fuzzingTestProg ::
  ( Matcher matcher Bool conVal,
    Show conVal,
    Mergeable conVal
  ) =>
  Gen [conVal] ->
  ([conVal] -> ConcreteContext ([conVal], matcher)) ->
  Int ->
  EvaledSymbolTable conVal ConcreteContext ->
  T.Text ->
  IO (Maybe (IOPair conVal, matcher))
fuzzingTestProg gen spec maxTests table symbol =
  fuzzingTestProg' (propertyWithMatcher gen spec maxTests table symbol) spec

fuzzingTestSymProgWithModel ::
  forall conVal conProg symProg matcher conSemObj p.
  ( Matcher matcher Bool conVal,
    Show conVal,
    ProgSemantics conSemObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ProgUtil conProg,
    Mergeable conVal,
    EvalSym symProg
  ) =>
  Gen [conVal] ->
  ([conVal] -> ConcreteContext ([conVal], matcher)) ->
  Int ->
  p conProg ->
  conSemObj ->
  SymbolTable symProg ->
  T.Text ->
  Model ->
  IO (Maybe (IOPair conVal, matcher))
fuzzingTestSymProgWithModel gen spec maxTests _ sem table symbol model = do
  fuzzingTestProg
    gen
    spec
    maxTests
    (evalSymbolTable sem (evalSymToCon model table :: SymbolTable conProg))
    symbol

data QuickCheckFuzzer symVal conVal symProg conProg where
  QuickCheckFuzzer ::
    ( SymExampleConstraint symSemObj symProg symVal matcher,
      ConExampleConstraint conSemObj conProg conVal matcher
    ) =>
    { quickCheckFuzzerSymSemantics :: symSemObj,
      quickCheckFuzzerConSemantics :: conSemObj,
      quickCheckFuzzerMaxTests :: Int,
      quickCheckFuzzerGenerators :: [Gen [conVal]],
      quickCheckFuzzerSpec :: [conVal] -> ConcreteContext ([conVal], matcher)
    } ->
    QuickCheckFuzzer symVal conVal symProg conProg

instance
  (ToCon symProg conProg, EvalSym symProg, ToSym conVal symVal) =>
  IsVerifier
    (QuickCheckFuzzer symVal conVal symProg conProg)
    symProg
    conProg
  where
  toVerifierFuns (QuickCheckFuzzer symSem conSem maxTests gens spec) table sym =
    flip fmap gens $ \gen model -> do
      fuzzingResult <-
        fuzzingTestSymProgWithModel
          gen
          spec
          maxTests
          (Proxy :: Proxy conProg)
          conSem
          table
          sym
          model
      case fuzzingResult of
        Just (ioPair, matcher) ->
          return
            ( CEGISVerifierFoundCex $
                SomeExample $
                  Example
                    conSem
                    symSem
                    (Proxy :: Proxy symVal)
                    (ioPair :: IOPair conVal)
                    matcher
            )
        Nothing -> return $ CEGISVerifierNoCex False

defaultQuickCheckFuzzer ::
  forall symVal conVal symProg conProg semObj.
  ( SymExampleConstraint semObj symProg symVal EqMatcher,
    ConExampleConstraint semObj conProg conVal EqMatcher,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvalSym symProg
  ) =>
  semObj ->
  Gen [conVal] ->
  ([conVal] -> ConcreteContext [conVal]) ->
  SomeVerifier symProg conProg
defaultQuickCheckFuzzer semObj gen spec =
  SomeVerifier
    ( QuickCheckFuzzer
        { quickCheckFuzzerSymSemantics = semObj,
          quickCheckFuzzerConSemantics = semObj,
          quickCheckFuzzerMaxTests = 100,
          quickCheckFuzzerGenerators = [gen],
          quickCheckFuzzerSpec = fmap (,EqMatcher) . spec
        } ::
        QuickCheckFuzzer symVal conVal symProg conProg
    )

defaultSemQuickCheckFuzzer ::
  forall symVal conVal symProg conProg.
  ( ProgSemantics DefaultSem symProg symVal AngelicContext,
    SymExampleConstraint DefaultSem symProg symVal EqMatcher,
    ProgSemantics DefaultSem conProg conVal ConcreteContext,
    ConExampleConstraint DefaultSem conProg conVal EqMatcher,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvalSym symProg
  ) =>
  Gen [conVal] ->
  ([conVal] -> ConcreteContext [conVal]) ->
  SomeVerifier symProg conProg
defaultSemQuickCheckFuzzer = defaultQuickCheckFuzzer @symVal DefaultSem
