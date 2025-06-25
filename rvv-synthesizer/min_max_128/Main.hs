{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.HashSet as HS
import Data.Ratio ((%))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    PPrint,
    WordN,
  )
import HieraSynth.Combinator.Invoke (Invoke (Invoke))
import HieraSynth.Combinator.Sum (inj)
import HieraSynth.Program.Choice.ChoiceTree (ChoiceMeta (Split), ChoiceTree (Branch, Leaf))
import HieraSynth.Program.Choice.ComponentBag (ComponentBag (ComponentBag))
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Unified (EvalModeTag (C))
import Options.Applicative
  ( Parser,
    ReadM,
    auto,
    help,
    long,
    option,
    showDefaultWith,
    str,
    switch,
    value,
  )
import RVV.App
  ( MainConfig
      ( MainConfig,
        defaultFuzzerMaxTests,
        defaultIRFile,
        defaultIRFuncName,
        defaultIRSpecScalingMethod,
        defaultImmScaleConfig,
        defaultLMulDownscaleRatio,
        defaultMachineConfigSpecs,
        defaultTemplateArgType,
        extraSketchArgsParser,
        maybeDefaultSpecification,
        overrideGenerators,
        sketchTable
      ),
    mainFunc,
  )
import RVV.App.TemplateArgType (TemplateArgType, toValueType)
import RVV.Semantics.MachineConfig (AllowPartialVL (DisallowPartialVL))
import RVV.Semantics.Multiplier (WidthMul (WidthMul))
import RVV.Semantics.Policy (muPolicy, nonePolicy)
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    vectorMaskMul,
  )
import RVV.Synthesizer.Feature.ExtractFeature (extractProgTableFeature)
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (Compare),
    FeatureSet (opFeatures),
    featureSetDifference,
  )
import RVV.Synthesizer.Feature.ToSketchOp (allowPartialVLFromFeatures, inferSketch, sketchFromFeatures)
import RVV.Synthesizer.Op
  ( ConSymbolTable,
    SketchSpec (CompSpec, ConProg, ConSpec),
    SketchSpecTable,
  )
import RVV.Synthesizer.Operator.Common.ImmSpec
  ( ImmSpec (ArbitraryImmSpec, BoundedImmSpec, ConstImmSpec, ReplicatingImmSpec),
  )
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (VectorRHS),
    SketchRHSSpec (SketchImmRHS, SketchScalarRHS, SketchVectorRHS),
  )
import RVV.Synthesizer.Operator.DelegatedVectorBinaryOnMask (sketchDelegatedVectorBinaryOnMask)
import RVV.Synthesizer.Operator.Merge (sketchMerge)
import RVV.Synthesizer.Operator.Move (sketchMoveToMask)
import RVV.Synthesizer.Operator.Scalar (sketchScalarLongImm)
import RVV.Synthesizer.Operator.SetVectorLength
  ( setMaxVectorLength,
    sketchSetMaxVectorLength,
  )
import RVV.Synthesizer.Operator.SingleWidthIntBinary (sketchSingleWidthIntBinary)
import RVV.Synthesizer.Operator.Slide (sketchSlide, sketchSlide1)
import RVV.Synthesizer.Operator.VectorCompare (sketchVectorCompare, vectorCompare)
import RVV.Synthesizer.Operator.VectorIndex (sketchVectorId)
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest, UseUndefinedDest),
    ud,
  )
import RVV.Synthesizer.Parameter.IntCompareOpCode
  ( IntCompareOpCode (MSEq, MSLeu, MSLtu),
    mkMSEq,
    mkMSLtu,
  )
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask, UseProvidedMask), fm)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (Add, And, Max, Maxu, Min, Minu, Or, Srl),
  )
import RVV.Synthesizer.Parameter.SlideDirection
  ( SlideDirection (SlideDown, SlideUp),
  )
import RVV.Synthesizer.Type (ValueType (MaskType, VLType, VectorType))

min128SubProgNew ::
  Bool ->
  Int ->
  T.Text ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  [TemplateArgType] ->
  (T.Text, TypeSignature ValueType, SketchSpec (WordN 8))
min128SubProgNew useArbitraryImm maskElementWiseBits key num numVecArg numMaskArg numVecRes numMaskRes templateArgTypes =
  let [(VectorType vtype) :: ValueType] =
        toValueType <$> templateArgTypes
      replicatingImm = if useArbitraryImm then ArbitraryImmSpec else ReplicatingImmSpec 2
      maskShiftImm = if useArbitraryImm then ArbitraryImmSpec else ConstImmSpec 1
      smallImm = if useArbitraryImm then ArbitraryImmSpec else BoundedImmSpec False 0 3
      maskElementWiseXMul = WidthMul $ maskElementWiseBits % 64
   in ( key,
        TypeSignature
          ( replicate numVecArg (VectorType vtype)
              ++ replicate numMaskArg (MaskType 1)
          )
          ( replicate numVecRes (VectorType vtype)
              ++ replicate numMaskRes (MaskType 1)
          ),
        CompSpec $
          ComponentBag
            ( replicate numVecArg (VectorType vtype)
                ++ replicate numMaskArg (MaskType 1)
            )
            [ (Leaf [sketchSetMaxVectorLength 1 [nonePolicy]], 1),
              ( Branch
                  (Split 0 False)
                  [ Branch
                      (Split 1 False)
                      [ Branch
                          (Split 2 False)
                          [ Leaf
                              [ sketchVectorCompare
                                  vtype
                                  [MSEq, MSLtu]
                                  [UseUndefinedDest]
                                  [UseFullMask]
                                  SketchVectorRHS
                              ],
                            Leaf
                              [ sketchDelegatedVectorBinaryOnMask
                                  maskElementWiseXMul
                                  (vectorMaskMul vtype)
                                  [And, Add, Or]
                                  (SketchImmRHS replicatingImm),
                                sketchDelegatedVectorBinaryOnMask
                                  maskElementWiseXMul
                                  (vectorMaskMul vtype)
                                  [Srl]
                                  (SketchImmRHS maskShiftImm),
                                sketchDelegatedVectorBinaryOnMask
                                  maskElementWiseXMul
                                  (vectorMaskMul vtype)
                                  [ And,
                                    Add,
                                    Or
                                  ]
                                  SketchVectorRHS
                              ]
                          ],
                        Branch
                          (Split 3 False)
                          [ Branch
                              (Split 4 False)
                              [ Branch
                                  (Split 6 False)
                                  [ Leaf
                                      [ sketchSingleWidthIntBinary
                                          vtype
                                          [And, Add, Or, Srl]
                                          [UseUndefinedDest, UseProvidedDest]
                                          [UseFullMask, UseProvidedMask]
                                          (SketchImmRHS smallImm),
                                        sketchSingleWidthIntBinary
                                          vtype
                                          [And, Add, Or, Srl]
                                          [UseUndefinedDest, UseProvidedDest]
                                          [UseFullMask, UseProvidedMask]
                                          (SketchImmRHS replicatingImm),
                                        sketchSingleWidthIntBinary
                                          vtype
                                          [And, Add, Or, Srl]
                                          [UseUndefinedDest, UseProvidedDest]
                                          [UseFullMask, UseProvidedMask]
                                          SketchVectorRHS
                                      ],
                                    Leaf
                                      [ sketchSingleWidthIntBinary
                                          vtype
                                          [Min, Minu, Max, Maxu]
                                          [UseUndefinedDest, UseProvidedDest]
                                          [UseFullMask, UseProvidedMask]
                                          (SketchImmRHS smallImm),
                                        sketchSingleWidthIntBinary
                                          vtype
                                          [Min, Minu, Max, Maxu]
                                          [UseUndefinedDest, UseProvidedDest]
                                          [UseFullMask, UseProvidedMask]
                                          SketchVectorRHS
                                      ]
                                  ],
                                Branch
                                  (Split 5 False)
                                  [ Leaf
                                      [ sketchMerge
                                          vtype
                                          [UseUndefinedDest, UseProvidedDest]
                                          (SketchImmRHS smallImm),
                                        sketchMerge
                                          vtype
                                          [UseUndefinedDest, UseProvidedDest]
                                          (SketchImmRHS replicatingImm),
                                        sketchMerge
                                          vtype
                                          [UseUndefinedDest, UseProvidedDest]
                                          SketchVectorRHS
                                      ],
                                    Leaf
                                      [ sketchSlide1
                                          vtype
                                          [SlideUp, SlideDown]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                          SketchScalarRHS,
                                        sketchSlide
                                          vtype
                                          [SlideUp, SlideDown]
                                          [UseProvidedDest]
                                          [UseFullMask]
                                          SketchScalarRHS
                                      ]
                                  ]
                              ]
                          ]
                      ],
                    Leaf
                      [ sketchScalarLongImm
                          (elementWidthMul vtype)
                          smallImm,
                        sketchScalarLongImm
                          (elementWidthMul vtype)
                          replicatingImm,
                        sketchVectorId
                          vtype
                          [UseUndefinedDest, UseProvidedDest]
                          [UseFullMask, UseProvidedMask]
                      ]
                  ],
                num
              )
            ]
            ( replicate numVecRes (VectorType vtype)
                ++ replicate numMaskRes (MaskType (vectorMaskMul vtype))
            )
      )

min128SubProg ::
  Bool ->
  Int ->
  T.Text ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  [TemplateArgType] ->
  (T.Text, TypeSignature ValueType, SketchSpec (WordN 8))
min128SubProg useArbitraryImm maskElementWiseBits key num numVecArg numMaskArg numVecRes numMaskRes templateArgTypes =
  let [(VectorType vtype) :: ValueType] =
        toValueType <$> templateArgTypes
      replicatingImm = if useArbitraryImm then ArbitraryImmSpec else ReplicatingImmSpec 2
      maskShiftImm = if useArbitraryImm then ArbitraryImmSpec else ConstImmSpec 1
      smallImm = if useArbitraryImm then ArbitraryImmSpec else BoundedImmSpec False 0 3
      maskElementWiseXMul = WidthMul $ maskElementWiseBits % 64
   in ( key,
        TypeSignature
          ( replicate numVecArg (VectorType vtype)
              ++ replicate numMaskArg (MaskType 1)
          )
          ( replicate numVecRes (VectorType vtype)
              ++ replicate numMaskRes (MaskType 1)
          ),
        CompSpec $
          ComponentBag
            ( replicate numVecArg (VectorType vtype)
                ++ replicate numMaskArg (MaskType 1)
            )
            [ (Leaf [sketchSetMaxVectorLength 1 [nonePolicy]], 1),
              ( Branch
                  (Split 1 False)
                  [ Leaf
                      [ sketchVectorCompare
                          vtype
                          [MSEq, MSLtu]
                          [UseUndefinedDest]
                          [UseFullMask]
                          SketchVectorRHS
                      ],
                    Branch
                      (Split 2 False)
                      [ Branch
                          (Split 4 False)
                          [ Leaf
                              [ sketchDelegatedVectorBinaryOnMask
                                  maskElementWiseXMul
                                  (vectorMaskMul vtype)
                                  [And, Add, Or]
                                  (SketchImmRHS replicatingImm),
                                sketchDelegatedVectorBinaryOnMask
                                  maskElementWiseXMul
                                  (vectorMaskMul vtype)
                                  [Srl]
                                  (SketchImmRHS maskShiftImm),
                                sketchDelegatedVectorBinaryOnMask
                                  maskElementWiseXMul
                                  (vectorMaskMul vtype)
                                  [ And,
                                    Add,
                                    Or
                                  ]
                                  SketchVectorRHS
                              ],
                            Leaf
                              [ sketchSingleWidthIntBinary
                                  vtype
                                  [And, Add, Or, Srl]
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask]
                                  (SketchImmRHS smallImm),
                                sketchSingleWidthIntBinary
                                  vtype
                                  [And, Add, Or, Srl]
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask]
                                  (SketchImmRHS replicatingImm),
                                sketchSingleWidthIntBinary
                                  vtype
                                  [And, Add, Or, Srl]
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask]
                                  SketchVectorRHS,
                                sketchSingleWidthIntBinary
                                  vtype
                                  [Min, Minu, Max, Maxu]
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask]
                                  (SketchImmRHS smallImm),
                                sketchSingleWidthIntBinary
                                  vtype
                                  [Min, Minu, Max, Maxu]
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask]
                                  SketchVectorRHS
                              ]
                          ],
                        Branch
                          (Split 3 False)
                          [ Branch
                              (Split 5 False)
                              [ Leaf
                                  [ sketchMerge
                                      vtype
                                      [UseUndefinedDest, UseProvidedDest]
                                      (SketchImmRHS smallImm),
                                    sketchMerge
                                      vtype
                                      [UseUndefinedDest, UseProvidedDest]
                                      (SketchImmRHS replicatingImm),
                                    sketchMerge
                                      vtype
                                      [UseUndefinedDest, UseProvidedDest]
                                      SketchVectorRHS
                                  ],
                                Leaf
                                  [ sketchSlide1
                                      vtype
                                      [SlideUp, SlideDown]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                      SketchScalarRHS,
                                    sketchSlide
                                      vtype
                                      [SlideUp, SlideDown]
                                      [UseProvidedDest]
                                      [UseFullMask]
                                      SketchScalarRHS
                                  ]
                              ],
                            Leaf
                              [ sketchScalarLongImm
                                  (elementWidthMul vtype)
                                  smallImm,
                                sketchScalarLongImm
                                  (elementWidthMul vtype)
                                  replicatingImm,
                                sketchVectorId
                                  vtype
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask]
                              ]
                          ]
                      ]
                  ],
                num
              )
            ]
            ( replicate numVecRes (VectorType vtype)
                ++ replicate numMaskRes (MaskType (vectorMaskMul vtype))
            )
      )

data SketchSelector
  = SubProg
      { _numSubProg1Insts :: Int,
        _numSubProg2Insts :: Int,
        _numSubProg3Insts :: Int,
        _numSubProg2VecArg :: Int,
        _numSubProg2MaskArg :: Int,
        _numSubProg3VecArg :: Int,
        _numSubProg3MaskArg :: Int
      }
  | SubProgFixedCmp {_num :: Int}
  | SubProgInferred {_num :: Int}
  | FixedCmpInferred {_num :: Int}
  | Optimal
  | FixedCmpOptimal
  deriving (Generic)
  deriving (PPrint) via (Default SketchSelector)

data Args = Args
  { useArbitraryImm :: Bool,
    maskElementWiseBits :: Int,
    newSubProg :: Bool,
    sketchSelector :: SketchSelector
  }
  deriving (Generic)
  deriving (PPrint) via (Default Args)

sketchSelectorReader :: ReadM SketchSelector
sketchSelectorReader = do
  s <- str
  if "sub_prog_inferred_" `T.isPrefixOf` s
    then do
      let num = read $ T.unpack $ T.drop 18 s
      return $ SubProgInferred num
    else
      if "sub_prog" `T.isPrefixOf` s
        then do
          let [num1, num2, num3, numVecArg, numMaskArg, numVecArg2, numMaskArg2] =
                read . T.unpack <$> T.splitOn "_" (T.drop 9 s)
          return $ SubProg num1 num2 num3 numVecArg numMaskArg numVecArg2 numMaskArg2
        else
          if "fixed_cmp_inferred_" `T.isPrefixOf` s
            then do
              let num = read $ T.unpack $ T.drop 19 s
              return $ FixedCmpInferred num
            else
              if "fixed_cmp_optimal" `T.isPrefixOf` s
                then return FixedCmpOptimal
                else
                  if "optimal" `T.isPrefixOf` s
                    then return Optimal
                    else
                      if "fixed_cmp" `T.isPrefixOf` s
                        then do
                          let num = read $ T.unpack $ T.drop 10 s
                          return $ SubProgFixedCmp num
                        else error $ "Invalid sketch selector: " <> T.unpack s

argsParser :: Parser Args
argsParser =
  Args
    <$> switch
      ( long "arbitrary-imm"
          <> help
            ( "Use arbitrary immediate values in the sketch. "
                <> "Default is bounded immediate values."
            )
      )
    <*> option
      auto
      ( long "mask-element-wise-bits"
          <> help
            ( "The number of bits in the mask element-wise operations. "
                <> "Default is 8."
            )
          <> showDefaultWith show
          <> value 8
      )
    <*> switch
      ( long "new-sub-prog"
          <> help
            ( "Use the new sub-prog sketch. "
                <> "Default is the old sub-prog sketch."
            )
      )
    <*> option
      sketchSelectorReader
      ( long "sketch-selector"
          <> help
            ( "Select the sketch to use. "
                <> "The format is 'sub_prog_<num1>_<num2>_<num3>_<numVecArg>_<numMaskArg>_<numVecArg2>_<numMaskArg2>' "
                <> "or 'fixed_cmp_<num>'."
            )
          <> showDefaultWith (const "fixed_cmp_inferred_7")
          <> value (FixedCmpInferred 7)
      )

min128WithSubProg ::
  Bool ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Bool ->
  Int ->
  T.Text ->
  [TemplateArgType] ->
  SketchSpecTable (WordN 8)
min128WithSubProg
  newSubProg
  num1
  num2
  num3
  numVecArg
  numMaskArg
  numVecArg2
  numMaskArg2
  useArbitraryImm
  maskElementWiseBits
  symbol
  templateArgTypes =
    SymbolTable $ do
      let subProg = if newSubProg then min128SubProgNew else min128SubProg
      let [ty :: ValueType] = toValueType <$> templateArgTypes
      let (name1, type1, spec1) =
            subProg useArbitraryImm maskElementWiseBits "prog1" num1 2 0 numVecArg numMaskArg templateArgTypes
      let (name2, type2, spec2) =
            subProg useArbitraryImm maskElementWiseBits "prog2" num2 numVecArg numMaskArg numVecArg2 numMaskArg2 templateArgTypes
      let (name3, type3, spec3) =
            subProg useArbitraryImm maskElementWiseBits "prog3" num3 numVecArg2 numMaskArg2 1 0 templateArgTypes
      [ (name1, spec1),
        (name2, spec2),
        (name3, spec3),
        ( symbol,
          ConSpec $
            Concrete.buildProg [("a", ty), ("b", ty)] $
              \[a, b] -> do
                l <-
                  Concrete.noden
                    (Leaf [inj $ Invoke type1 name1])
                    (numVecArg + numMaskArg)
                    [a, b]
                r <-
                  Concrete.noden
                    (Leaf [inj $ Invoke type2 name2])
                    (numVecArg2 + numMaskArg2)
                    l
                p <-
                  Concrete.node1
                    (Leaf [inj $ Invoke type3 name3])
                    r
                return [(p, ty)]
        )
        ]

min128WithSubProgFixedCmp ::
  Bool ->
  Int ->
  Bool ->
  Int ->
  T.Text ->
  [TemplateArgType] ->
  SketchSpecTable (WordN 8)
min128WithSubProgFixedCmp newSubProg num1 useArbitraryImm maskElementWiseBits symbol templateArgTypes =
  SymbolTable $ do
    let subProg = if newSubProg then min128SubProgNew else min128SubProg
    let [ty@(VectorType vtype) :: ValueType] = toValueType <$> templateArgTypes
    let (name1, type1, spec1) =
          subProg useArbitraryImm maskElementWiseBits "prog1" num1 2 2 1 0 templateArgTypes
    let mmul = vectorMaskMul vtype
    [ (name1, spec1),
      ( symbol,
        ConProg $
          Concrete.buildProg [("a", ty), ("b", ty)] $
            \[a, b] -> do
              vlmax <- Concrete.node1 (setMaxVectorLength @'C mmul nonePolicy) []
              sltu <-
                Concrete.node1
                  (vectorCompare @'C vtype mkMSLtu ud fm VectorRHS)
                  [vlmax, a, b]
              eq <-
                Concrete.node1
                  (vectorCompare @'C vtype mkMSEq ud fm VectorRHS)
                  [vlmax, a, b]
              p <-
                Concrete.node1
                  (inj $ Invoke type1 name1)
                  [a, b, sltu, eq]
              return [(p, ty)]
      )
      ]

min128WithSubProgFixedCmpInferred ::
  Int ->
  Int ->
  ConSymbolTable (WordN 8) ->
  T.Text ->
  [TemplateArgType] ->
  (AllowPartialVL, SketchSpecTable (WordN 8))
min128WithSubProgFixedCmpInferred
  num1
  maskElementWiseBits
  conTable
  symbol
  templateArgTypes = do
    let [ty@(VectorType vtype) :: ValueType] = toValueType <$> templateArgTypes
    let mmul = vectorMaskMul vtype
    let argTypes =
          [VLType mmul, VectorType vtype, VectorType vtype, MaskType mmul, MaskType mmul]
    let resTypes =
          [VectorType vtype]
    let features =
          featureSetDifference
            (extractProgTableFeature conTable)
            mempty {opFeatures = HS.fromList [Compare]}
    let spec1 =
          sketchFromFeatures
            (WidthMul $ maskElementWiseBits % 64)
            num1
            features
            argTypes
            resTypes
    let type1 = TypeSignature argTypes resTypes
    -- let subProg = if newSubProg then min128SubProgNew else min128SubProg
    -- let (name1, type1, spec1) =
    --       subProg useArbitraryImm maskElementWiseBits "prog1" num1 2 2 1 0 templateArgTypes
    ( allowPartialVLFromFeatures features,
      SymbolTable $ do
        [ ("prog", CompSpec spec1),
          ( symbol,
            ConProg $
              Concrete.buildProg [("a", ty), ("b", ty)] $
                \[a, b] -> do
                  vlmax <- Concrete.node1 (setMaxVectorLength @'C mmul nonePolicy) []
                  sltu <-
                    Concrete.node1
                      (vectorCompare @'C vtype mkMSLtu ud fm VectorRHS)
                      [vlmax, a, b]
                  eq <-
                    Concrete.node1
                      (vectorCompare @'C vtype mkMSEq ud fm VectorRHS)
                      [vlmax, a, b]
                  p <-
                    Concrete.node1
                      (inj $ Invoke type1 "prog")
                      [vlmax, a, b, sltu, eq]
                  return [(p, ty)]
          )
          ]
      )

min128OptimalSketch :: Int -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
min128OptimalSketch maskElementWiseBits _ min128SketchSymbol templateArgTypes =
  let [VectorType vtype :: ValueType] =
        toValueType <$> templateArgTypes
      mmul = vectorMaskMul vtype
      delegatedXMul = WidthMul $ maskElementWiseBits % 64
   in SymbolTable
        [ ( min128SketchSymbol,
            CompSpec $
              ComponentBag
                [VectorType vtype, VectorType vtype]
                [ (Leaf [sketchSetMaxVectorLength mmul [muPolicy]], 1),
                  -- Vector comparison for less than (m3 = vmsltu.vv)
                  (Leaf [sketchVectorCompare vtype [MSLtu] [UseUndefinedDest] [UseFullMask] SketchVectorRHS], 1),
                  -- Delegated vector operations (m4 = vadd.delegated, m6 = vand.delegated, m7 = vsrl.delegated, m8 = vadd.delegated)
                  (Leaf [sketchDelegatedVectorBinaryOnMask delegatedXMul mmul [Add] SketchVectorRHS], 2),
                  (Leaf [sketchVectorCompare vtype [MSLeu] [UseProvidedDest] [UseProvidedMask] SketchVectorRHS], 1),
                  (Leaf [sketchDelegatedVectorBinaryOnMask delegatedXMul mmul [And] (SketchImmRHS ArbitraryImmSpec)], 1),
                  (Leaf [sketchDelegatedVectorBinaryOnMask delegatedXMul mmul [Srl] (SketchImmRHS (BoundedImmSpec False 0 3))], 1),
                  -- Vector max unsigned (v9 = vmaxu.vv)
                  (Leaf [sketchSingleWidthIntBinary vtype [Maxu] [UseProvidedDest] [UseProvidedMask] SketchVectorRHS], 1)
                ]
                [VectorType vtype]
          )
        ]

fixedCmpOptimalSketch :: Int -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
fixedCmpOptimalSketch maskElementWiseBits _ min128SketchSymbol templateArgTypes =
  let [VectorType vtype :: ValueType] =
        toValueType <$> templateArgTypes
      mmul = vectorMaskMul vtype
      delegatedXMul = WidthMul $ maskElementWiseBits % 64
      progArgTypes = [VLType mmul, VectorType vtype, VectorType vtype, MaskType mmul, MaskType mmul]
      progResTypes = [VectorType vtype]
   in SymbolTable
        [ ( "prog",
            CompSpec $
              ComponentBag
                progArgTypes
                [ (Leaf [sketchSetMaxVectorLength mmul [muPolicy]], 1),
                  (Leaf [sketchMerge vtype [UseUndefinedDest] SketchVectorRHS], 1),
                  (Leaf [sketchSlide1 vtype [SlideUp] [UseProvidedDest] [UseFullMask] (SketchImmRHS ArbitraryImmSpec)], 1),
                  (Leaf [sketchSlide1 vtype [SlideUp] [UseProvidedDest] [UseProvidedMask] (SketchImmRHS ArbitraryImmSpec)], 2),
                  (Leaf [sketchSlide1 vtype [SlideDown] [UseProvidedDest] [UseProvidedMask] (SketchImmRHS ArbitraryImmSpec)], 1),
                  -- Scalar long immediate (m9 = imm_to_mask)
                  (Leaf [sketchMoveToMask delegatedXMul mmul (SketchImmRHS ArbitraryImmSpec)], 1)
                ]
                progResTypes
          ),
          ( min128SketchSymbol,
            ConProg $
              Concrete.buildProg [("a", VectorType vtype), ("b", VectorType vtype)] $
                \[ar, br] -> do
                  vlmax <- Concrete.node1 (setMaxVectorLength @'C mmul nonePolicy) []
                  sltu <-
                    Concrete.node1
                      (vectorCompare @'C vtype mkMSLtu ud fm VectorRHS)
                      [vlmax, ar, br]
                  eq <-
                    Concrete.node1
                      (vectorCompare @'C vtype mkMSEq ud fm VectorRHS)
                      [vlmax, ar, br]
                  r <-
                    Concrete.node1
                      ( inj @(Invoke ValueType) $
                          Invoke (TypeSignature progArgTypes progResTypes) "prog"
                      )
                      [vlmax, ar, br, sltu, eq]
                  return [(r, VectorType vtype)]
          )
        ]

min128Sketch :: Args -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> (AllowPartialVL, SketchSpecTable (WordN 8))
min128Sketch (Args useArbitraryImm maskElementWiseBits newSubProg sketchSelector) conTable symbol templateArgTypes =
  case sketchSelector of
    SubProg num1 num2 num3 numVecArg numMaskArg numVecArg2 numMaskArg2 ->
      ( DisallowPartialVL,
        min128WithSubProg
          newSubProg
          num1
          num2
          num3
          numVecArg
          numMaskArg
          numVecArg2
          numMaskArg2
          useArbitraryImm
          maskElementWiseBits
          symbol
          templateArgTypes
      )
    SubProgInferred n ->
      inferSketch
        (WidthMul $ maskElementWiseBits % 64)
        n
        conTable
        symbol
        id
    SubProgFixedCmp num ->
      ( DisallowPartialVL,
        min128WithSubProgFixedCmp
          newSubProg
          num
          useArbitraryImm
          maskElementWiseBits
          symbol
          templateArgTypes
      )
    FixedCmpInferred num ->
      min128WithSubProgFixedCmpInferred
        num
        maskElementWiseBits
        conTable
        symbol
        templateArgTypes
    Optimal ->
      (DisallowPartialVL, min128OptimalSketch maskElementWiseBits conTable symbol templateArgTypes)
    FixedCmpOptimal ->
      (DisallowPartialVL, fixedCmpOptimalSketch maskElementWiseBits conTable symbol templateArgTypes)

main :: IO ()
main =
  mainFunc $
    MainConfig
      { defaultIRFile = "min_max_128/min_max_128.sir",
        defaultIRFuncName = "Min128",
        defaultTemplateArgType = Just "vuint64m1_t",
        defaultIRSpecScalingMethod = "zext",
        defaultMachineConfigSpecs =
          "vlen32f4,vlen64f4,vlen128f4;vlen128,vlen256,vlen512",
        defaultImmScaleConfig = Nothing,
        defaultLMulDownscaleRatio = 1,
        maybeDefaultSpecification = Nothing,
        overrideGenerators = Nothing,
        defaultFuzzerMaxTests = 50,
        sketchTable = min128Sketch,
        extraSketchArgsParser = argsParser
      }
