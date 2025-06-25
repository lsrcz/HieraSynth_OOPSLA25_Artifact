{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HieraSynth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (..),
    TestPrettyExtOp (..),
    TestPrettyType (..),
  )
where

import Data.List ((\\))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( PPrint (pformat),
    allClasses0,
    derive,
    pprintClasses,
  )
import HieraSynth.Context (ConcreteContext)
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.Program.Concrete
  ( OpPPrint (describeArguments, prefixResults),
    PrefixByType (prefixByType),
    allPrefixesByTypes,
  )
import HieraSynth.Program.Concrete.Flatten
  ( OpFlatten (opForwardedSubProg),
  )
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes),
  )

data TestPrettyExtOp = TestPrettyExtOp deriving (Generic)

data TestPrettyOp
  = PrettyOp0
  | PrettyOp1
  | PrettyOp2
  | PrettyOp2NoDescNoPrefix
  | PrettyInvokeOp (TypeSignature TestPrettyType) T.Text
  | PrettyInvokeExtOp (TypeSignature TestPrettyType) T.Text
  deriving (Generic)

data TestPrettyType = PrettyType1 | PrettyType2 deriving (Generic)

derive [''TestPrettyType] allClasses0

derive
  [''TestPrettyExtOp, ''TestPrettyOp]
  (allClasses0 \\ pprintClasses)

instance PPrint TestPrettyExtOp where
  pformat TestPrettyExtOp = "ext"

instance OpPPrint TestPrettyExtOp where
  describeArguments TestPrettyExtOp = Right [Nothing]

instance OpTyping TestPrettyExtOp ConcreteContext where
  type OpTypeType TestPrettyExtOp = TestPrettyType
  typeOp TestPrettyExtOp =
    return $ TypeSignature [PrettyType1] [PrettyType1, PrettyType2]

instance OpFlatten TestPrettyOp TestPrettyOp where
  opForwardedSubProg (PrettyInvokeOp _ prog) = return $ Left prog
  opForwardedSubProg (PrettyInvokeExtOp _ prog) = return $ Left prog
  opForwardedSubProg r = return $ Right r

instance PPrint TestPrettyOp where
  pformat PrettyOp0 = "op0"
  pformat PrettyOp1 = "op1"
  pformat PrettyOp2 = "op2"
  pformat PrettyOp2NoDescNoPrefix = "op2NoDescNoPrefix"
  pformat (PrettyInvokeOp _ prog) = "invoke(" <> pformat prog <> ")"
  pformat (PrettyInvokeExtOp _ prog) =
    "invoke_ext(" <> pformat prog <> ")"

instance OpPPrint TestPrettyOp where
  describeArguments PrettyOp0 = Right []
  describeArguments PrettyOp1 = Right [Just "op1"]
  describeArguments PrettyOp2 = Right [Just "op2'2'0'arg", Nothing]
  describeArguments PrettyOp2NoDescNoPrefix = Right []
  describeArguments (PrettyInvokeOp sig _) = return $ Nothing <$ argTypes sig
  describeArguments (PrettyInvokeExtOp sig _) =
    return $ Nothing <$ argTypes sig
  prefixResults PrettyOp2 = return ["op2_", "op2'_"]
  prefixResults PrettyOp2NoDescNoPrefix = return []
  prefixResults op = allPrefixesByTypes op

instance PrefixByType TestPrettyType where
  prefixByType PrettyType1 = "t1_"
  prefixByType PrettyType2 = "t2_"

instance OpTyping TestPrettyOp ConcreteContext where
  type OpTypeType TestPrettyOp = TestPrettyType
  typeOp PrettyOp0 = return $ TypeSignature [] [PrettyType1]
  typeOp PrettyOp1 = return $ TypeSignature [PrettyType1] [PrettyType1]
  typeOp PrettyOp2 =
    return $ TypeSignature [PrettyType1, PrettyType2] [PrettyType2, PrettyType1]
  typeOp PrettyOp2NoDescNoPrefix =
    return $ TypeSignature [PrettyType1, PrettyType2] [PrettyType2, PrettyType1]
  typeOp (PrettyInvokeOp sig _) = return sig
  typeOp (PrettyInvokeExtOp sig _) = return sig
