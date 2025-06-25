{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Type
  ( ValueType (..),
    isVLType,
    isMemType,
    isVectorType,
    isMaskType,
    isScalarType,
  )
where

import Control.Applicative (asum)
import Control.Lens (makeFieldsNoPrefix, (%~), (&))
import Grisette
  ( GenSym,
    GenSymSimple (simpleFresh),
    PPrint (pformat),
  )
import HieraSynth.Program.Choice.Counting
  ( ValidArgument (validArgument),
  )
import HieraSynth.Program.Concrete (PrefixByType (prefixByType))
import HieraSynth.Type.TypeParser (TypeParser (typeParser))
import HieraSynth.Util.Parser
  ( CharParser,
    betweenAngles,
    named,
    symbol,
  )
import RVV.Parser.ArgParser
  ( maskMulParser,
    vectorConfigParser,
    widthMulParser,
  )
import RVV.Semantics.Memory (BlockId)
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    maskMulFeature,
    vectorConfigFeature,
    widthMulFeature,
  )
import RVV.Synthesizer.Lens
  ( HasBlockId (blockId),
    HasMaskMul (maskMul),
    HasVectorConfig (vectorConfig),
    HasWidthMul (widthMul),
  )
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data ValueType
  = VLType {_maskMul :: MaskMul}
  | MaskType {_maskMul :: MaskMul}
  | VectorType {_vectorConfig :: VectorConfig}
  | ScalarType {_widthMul :: WidthMul}
  | PtrType {_widthMul :: WidthMul, _blockId :: BlockId}
  | MemType

makeFieldsNoPrefix ''ValueType

deriveFull noModeDeriveConfig [''ValueType]

instance ValidArgument ValueType where
  validArgument (VLType mmul1) (VLType mmul2) = mmul1 == mmul2
  validArgument (VectorType vtype1) (VectorType vtype2) = vtype1 == vtype2
  validArgument (MaskType mmul1) (MaskType mmul2) = mmul1 == mmul2
  validArgument (ScalarType xmul1) (ScalarType xmul2) = xmul1 == xmul2
  validArgument (PtrType xmul1 blockId1) (PtrType xmul2 blockId2) =
    xmul1 == xmul2 && blockId1 == blockId2
  validArgument MemType MemType = True
  validArgument _ _ = False

instance GenSym ValueType ValueType

instance GenSymSimple ValueType ValueType where
  simpleFresh = return

isVectorType :: ValueType -> Bool
isVectorType (VectorType _) = True
isVectorType _ = False

isMaskType :: ValueType -> Bool
isMaskType (MaskType _) = True
isMaskType _ = False

isScalarType :: ValueType -> Bool
isScalarType (ScalarType _) = True
isScalarType _ = False

isVLType :: ValueType -> Bool
isVLType (VLType _) = True
isVLType _ = False

isMemType :: ValueType -> Bool
isMemType MemType = True
isMemType _ = False

instance PPrint ValueType where
  pformat (VLType maskMul) = "vl<mmul=" <> pformat maskMul <> ">"
  pformat (MaskType maskMul) =
    "mask<mmul=" <> pformat maskMul <> ">"
  pformat (VectorType vtype) =
    "vec<" <> pformat vtype <> ">"
  pformat (ScalarType xmul) = "scalar<xmul=" <> pformat xmul <> ">"
  pformat (PtrType xmul blockId) =
    "ptr<xmul=" <> pformat xmul <> ", block=" <> pformat blockId <> ">"
  pformat MemType = "mem"

instance PrefixByType ValueType where
  prefixByType (VLType _) = "vl"
  prefixByType (MaskType _) = "m"
  prefixByType (VectorType _) = "v"
  prefixByType (ScalarType _) = "x"
  prefixByType (PtrType _ _) = "ptr"
  prefixByType MemType = "mem"

instance ExtractFeature ValueType FeatureSet where
  extractFeature (VectorType vtype) = vectorConfigFeature vtype
  extractFeature (MaskType maskMul) = maskMulFeature maskMul
  extractFeature (ScalarType xmul) = widthMulFeature xmul
  extractFeature (VLType maskMul) = maskMulFeature maskMul
  extractFeature _ = error "Unimplemented"

vecTypeParser :: (CharParser e s m) => m ValueType
vecTypeParser = do
  symbol "vec"
  vtype <- betweenAngles vectorConfigParser
  return $ VectorType vtype

maskTypeParser :: (CharParser e s m) => m ValueType
maskTypeParser = do
  symbol "mask"
  maskMul <- betweenAngles $ named "mmul" maskMulParser
  return $ MaskType maskMul

scalarTypeParser :: (CharParser e s m) => m ValueType
scalarTypeParser = do
  symbol "scalar"
  scalarWidthMul <- betweenAngles $ named "xmul" widthMulParser
  return $ ScalarType scalarWidthMul

instance TypeParser ValueType where
  typeParser = asum [vecTypeParser, maskTypeParser, scalarTypeParser]

instance ScaleLMul ValueType where
  scaleLMul _ MemType = error "Not supported"
  scaleLMul _ PtrType {} = error "Not supported"
  scaleLMul ratio ty =
    ty
      & (maskMul %~ scaleLMul ratio)
      & (vectorConfig %~ scaleLMul ratio)
