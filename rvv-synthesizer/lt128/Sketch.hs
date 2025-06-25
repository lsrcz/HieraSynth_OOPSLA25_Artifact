{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sketch
  ( lt128Sketch,
    argsParser,
  )
where

import Concrete (compareVVV)
import qualified Data.HashSet as HS
import Data.Ratio ((%))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    GenSymSimple (simpleFresh),
    PPrint,
    SymWordN,
    ToSym (toSym),
    Union,
    WordN,
    chooseFresh,
    mrgReturn,
    runFresh,
  )
import HieraSynth.Combinator.Invoke (Invoke (Invoke))
import HieraSynth.Combinator.Sum ((:|) (InLeft, InRight), type (:<:) (inj))
import HieraSynth.Program.Choice.ChoiceTree (ChoiceMeta (NoSplit, Split), ChoiceTree (Branch, Leaf))
import HieraSynth.Program.Choice.ComponentBag (ComponentBag (ComponentBag))
import HieraSynth.Program.ComponentSketch
  ( MkFreshProg (mkFreshProg),
    simpleFreshStmt,
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.ProgTyping (ProgTyping (typeProg))
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Unified (EvalModeTag (C, S))
import Options.Applicative (Parser, ReadM, auto, help, long, option, showDefaultWith, str, switch, value)
import RVV.App.TemplateArgType (TemplateArgType, toValueType)
import RVV.Semantics.Imm (Imm)
import RVV.Semantics.MachineConfig (AllowPartialVL (DisallowPartialVL))
import RVV.Semantics.Multiplier (WidthMul (WidthMul))
import RVV.Semantics.Policy (muPolicy, nonePolicy)
import RVV.Semantics.VectorConfig (VectorConfig (elementWidthMul), vectorMaskMul)
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
    SymProg,
  )
import RVV.Synthesizer.Operator.Common.ImmSpec
  ( ImmSpec (ArbitraryImmSpec, BoundedImmSpec, ConstImmSpec, ReplicatingImmSpec),
  )
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ScalarRHS, VectorRHS), SketchRHSSpec (SketchImmRHS, SketchScalarRHS, SketchVectorRHS))
import RVV.Synthesizer.Operator.DelegatedVectorBinaryOnMask (sketchDelegatedVectorBinaryOnMask)
import RVV.Synthesizer.Operator.MaskLogical (sketchMaskLogical)
import RVV.Synthesizer.Operator.Merge (sketchMerge)
import RVV.Synthesizer.Operator.Reinterpret (vectorToMask)
import RVV.Synthesizer.Operator.Scalar (scalarLongImm, sketchScalarLongImm)
import RVV.Synthesizer.Operator.SetVectorLength
  ( setMaxVectorLength,
    sketchSetMaxVectorLength,
  )
import RVV.Synthesizer.Operator.SingleWidthIntBinary (SketchSingleWidthIntBinary (SketchSingleWidthIntBinary), singleWidthIntBinary)
import RVV.Synthesizer.Operator.Slide (sketchSlide1)
import RVV.Synthesizer.Operator.VectorCompare (sketchVectorCompare, vectorCompare)
import RVV.Synthesizer.Operator.VectorIndex (sketchVectorId)
import RVV.Synthesizer.Parameter.Destination (Destination (UseProvidedDest, UseUndefinedDest), ud)
import RVV.Synthesizer.Parameter.IntCompareOpCode
  ( IntCompareOpCode (MSEq, MSLeu, MSLtu, MSNe),
    mkMSEq,
    mkMSLtu,
  )
import RVV.Synthesizer.Parameter.MaskLogicalOpCode (MaskLogicalOpCode (MNand, MXnor, MXor))
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask, UseProvidedMask), fm)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (Add, And, Mul, Or, Sll, Srl, Xor),
  )
import RVV.Synthesizer.Parameter.SlideDirection (SlideDirection (SlideDown, SlideUp))
import RVV.Synthesizer.Type (ValueType (MaskType, VLType, VectorType))
import Types (vtypeE1M1)

symSingleWidthVLongScalar ::
  T.Text ->
  VectorConfig ->
  Union SingleWidthIntBinaryOpCode ->
  Union Destination ->
  Union Masking ->
  Imm 'S ->
  (T.Text, SymProg (WordN 8) (SymWordN 8))
symSingleWidthVLongScalar
  key
  config
  opUnion
  destination
  masking
  imm =
    ( key,
      InLeft
        $ Concrete.buildProg
          [ ("lhs", VectorType config),
            ("vl", VLType (vectorMaskMul config))
          ]
        $ \[lhs, vl] -> do
          immVec <-
            Concrete.node1
              (scalarLongImm @'S 1 imm)
              []
          res <-
            Concrete.node1
              ( singleWidthIntBinary
                  config
                  opUnion
                  destination
                  masking
                  ScalarRHS
              )
              [vl, lhs, immVec]
          return [(res, VectorType config)]
    )

lt128Sketch0 :: T.Text -> SymbolTable (SymProg (WordN 8) (SymWordN 8))
lt128Sketch0 lt128SketchSymbol = flip runFresh "sketch" $ do
  let (mseqrSymbol, mseqr) =
        compareVVV "mseqr" vtypeE1M1 mkMSEq ud fm
  let (msltrSymbol, msltr) =
        compareVVV "msltr" vtypeE1M1 mkMSLtu ud fm
  (long1Symbol, long1) <- do
    imm <- simpleFresh ()
    op <- chooseFresh [And, Mul, Add, Or, Srl]
    return $
      symSingleWidthVLongScalar
        "long1"
        vtypeE1M1
        op
        ud
        fm
        imm
  (long2Symbol, long2) <- do
    imm <- simpleFresh ()
    op <- chooseFresh [And, Add, Or, Srl]
    return $
      symSingleWidthVLongScalar
        "long2"
        vtypeE1M1
        op
        ud
        fm
        imm
  (long3Symbol, long3) <- do
    imm <- simpleFresh ()
    op <- chooseFresh [And, Add, Or, Srl]
    return $
      symSingleWidthVLongScalar
        "long3"
        vtypeE1M1
        op
        ud
        fm
        imm
  (long4Symbol, long4) <- do
    imm <- simpleFresh ()
    op <- chooseFresh [And, Add, Or, Srl]
    return $
      symSingleWidthVLongScalar
        "long4"
        vtypeE1M1
        op
        ud
        fm
        imm
  prog <-
    mkFreshProg
      [VectorType vtypeE1M1, VectorType vtypeE1M1]
      [ simpleFreshStmt $
          setMaxVectorLength @'S 1 nonePolicy,
        simpleFreshStmt
          (mrgReturn $ inj @(Invoke ValueType) $ Invoke (typeProg mseqr) mseqrSymbol),
        simpleFreshStmt
          (mrgReturn $ inj @(Invoke ValueType) $ Invoke (typeProg msltr) msltrSymbol),
        simpleFreshStmt
          ( singleWidthIntBinary @'S
              vtypeE1M1
              (mrgReturn And)
              ud
              fm
              VectorRHS
          ),
        simpleFreshStmt
          ( singleWidthIntBinary @'S
              vtypeE1M1
              (mrgReturn Or)
              ud
              fm
              VectorRHS
          ),
        simpleFreshStmt
          (mrgReturn $ inj @(Invoke ValueType) $ Invoke (typeProg long1) long1Symbol),
        simpleFreshStmt
          (mrgReturn $ inj @(Invoke ValueType) $ Invoke (typeProg long2) long2Symbol),
        simpleFreshStmt
          (mrgReturn $ inj @(Invoke ValueType) $ Invoke (typeProg long3) long3Symbol),
        simpleFreshStmt
          (mrgReturn $ inj @(Invoke ValueType) $ Invoke (typeProg long4) long4Symbol),
        simpleFreshStmt $ vectorToMask vtypeE1M1 1
      ]
      [MaskType 1]
  return $
    SymbolTable
      [ (mseqrSymbol, InLeft $ toSym mseqr),
        (msltrSymbol, InLeft $ toSym msltr),
        (long1Symbol, long1),
        (long2Symbol, long2),
        (long3Symbol, long3),
        (long4Symbol, long4),
        (lt128SketchSymbol, InRight prog)
      ]

lt128Sketch191 :: Int -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
lt128Sketch191 maskElementWiseBits _ lt128SketchSymbol templateArgTypes =
  let [ maskType@(MaskType mmul),
        vectorType@(VectorType vtype) :: ValueType
        ] = toValueType <$> templateArgTypes
      delegatedXMul = WidthMul $ maskElementWiseBits % 64

      -- Create sketch spec for the main program
      mainSpec =
        CompSpec $
          ComponentBag
            [vectorType, vectorType] -- Two input vectors
            [ -- First set VLEN
              (Leaf [sketchSetMaxVectorLength mmul [muPolicy]], 1),
              -- Allow comparison operations
              ( Leaf
                  [ sketchVectorCompare
                      vtype
                      [MSLeu, MSEq, MSLtu, MSNe] -- Allow equality, less than, not equal
                      [UseProvidedDest, UseUndefinedDest]
                      [UseProvidedMask, UseFullMask]
                      SketchVectorRHS
                  ],
                2
              ),
              -- Allow various mask/vector operations
              ( Branch
                  NoSplit -- (Split 1 False)
                  [ -- First group: Shift and multiply operations with imms
                    Leaf
                      [ sketchDelegatedVectorBinaryOnMask
                          delegatedXMul -- Delegated xmul
                          mmul
                          [Sll, Srl] -- Shift right logical
                          (SketchImmRHS (BoundedImmSpec False 0 3)), -- Bounded imm 0-3
                        sketchDelegatedVectorBinaryOnMask
                          delegatedXMul
                          mmul
                          [Mul] -- Multiply
                          (SketchImmRHS (ConstImmSpec 3)) -- Constant imm 3
                      ],
                    -- Second group: Add, shift, and bitwise operations
                    Leaf
                      [ sketchDelegatedVectorBinaryOnMask
                          delegatedXMul
                          mmul
                          [Add]
                          SketchVectorRHS, -- Add
                        sketchDelegatedVectorBinaryOnMask
                          delegatedXMul
                          mmul
                          [Sll, Srl]
                          SketchScalarRHS, -- Bitwise operations
                        sketchDelegatedVectorBinaryOnMask
                          delegatedXMul
                          mmul
                          [Xor, Or, And] -- Bitwise operations
                          (SketchImmRHS ArbitraryImmSpec)
                      ]
                  ],
                4 -- Allow 4 operations from these groups
              )
            ]
            [maskType] -- Output mask type
   in SymbolTable [(lt128SketchSymbol, mainSpec)]

lt128OptimalSketch :: Int -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
lt128OptimalSketch maskElementWiseBits _ lt128SketchSymbol templateArgTypes =
  let [MaskType mmul, VectorType vtype :: ValueType] =
        toValueType <$> templateArgTypes
      delegatedXMul = WidthMul $ maskElementWiseBits % 64
   in SymbolTable
        [ ( lt128SketchSymbol,
            CompSpec $
              ComponentBag
                [VectorType vtype, VectorType vtype]
                [ (Leaf [sketchSetMaxVectorLength mmul [muPolicy]], 1),
                  (Leaf [sketchVectorCompare vtype [MSLtu] [UseUndefinedDest] [UseFullMask] SketchVectorRHS], 1),
                  (Leaf [sketchDelegatedVectorBinaryOnMask delegatedXMul mmul [Add] SketchVectorRHS], 2),
                  (Leaf [sketchVectorCompare vtype [MSLeu] [UseProvidedDest] [UseProvidedMask] SketchVectorRHS], 1),
                  (Leaf [sketchDelegatedVectorBinaryOnMask delegatedXMul mmul [And] (SketchImmRHS ArbitraryImmSpec)], 1),
                  (Leaf [sketchDelegatedVectorBinaryOnMask delegatedXMul mmul [Srl] (SketchImmRHS (BoundedImmSpec False 0 3))], 1)
                ]
                [MaskType mmul]
          )
        ]

fixedCmpOptimalSketch :: Int -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
fixedCmpOptimalSketch maskElementWiseBits _ lt128SketchSymbol templateArgTypes =
  let [MaskType mmul, VectorType vtype :: ValueType] =
        toValueType <$> templateArgTypes
      delegatedXMul = WidthMul $ maskElementWiseBits % 64
      progArgTypes = [VLType mmul, MaskType mmul, MaskType mmul]
      progResTypes = [MaskType mmul]
   in SymbolTable
        [ ( "prog",
            CompSpec $
              ComponentBag
                progArgTypes
                [ -- Add delegated operation (for m3 = vadd.delegated)
                  (Leaf [sketchDelegatedVectorBinaryOnMask delegatedXMul mmul [Add] SketchVectorRHS], 1),
                  -- Mask logical operations (for m4 = vmnand.mm, m5 = vmxor.mm, m8 = vmxnor.mm)
                  (Leaf [sketchMaskLogical mmul [MNand]], 1),
                  (Leaf [sketchMaskLogical mmul [MXor]], 1),
                  (Leaf [sketchMaskLogical mmul [MXnor]], 1),
                  -- Delegated vector binary operations with arbitrary immediates
                  -- (for m6 = vand.delegated with imm_rhs, m7 = vsrl.delegated with imm_rhs)
                  (Leaf [sketchDelegatedVectorBinaryOnMask delegatedXMul mmul [And] (SketchImmRHS ArbitraryImmSpec)], 1),
                  (Leaf [sketchDelegatedVectorBinaryOnMask delegatedXMul mmul [Srl] (SketchImmRHS (BoundedImmSpec False 0 3))], 1)
                ]
                progResTypes
          ),
          ( lt128SketchSymbol,
            ConProg $
              Concrete.buildProg [("a", VectorType vtype), ("b", VectorType vtype)] $
                \[ar, br] -> do
                  vlmax <- Concrete.node1 (setMaxVectorLength @'C mmul muPolicy) []
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
                      [vlmax, sltu, eq]
                  return [(r, MaskType mmul)]
          )
        ]

lt128SimpleSketch :: Int -> Int -> Bool -> Int -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
lt128SimpleSketch numCmp num useArbitraryImm maskElementWiseBits lt128SketchSymbol templateArgTypes =
  let [MaskType mmul, VectorType vtype :: ValueType] =
        toValueType <$> templateArgTypes
      replicatingImm = if useArbitraryImm then ArbitraryImmSpec else ReplicatingImmSpec 2
      boundedImm = if useArbitraryImm then ArbitraryImmSpec else ConstImmSpec 1
      maskElementWiseXMul = WidthMul $ maskElementWiseBits % 64
   in SymbolTable
        [ ( lt128SketchSymbol,
            CompSpec $
              ComponentBag
                [ VectorType vtype,
                  VectorType vtype
                ]
                [ (Leaf [sketchSetMaxVectorLength mmul [nonePolicy]], 1),
                  ( Leaf
                      [ sketchVectorCompare
                          vtype
                          [MSEq, MSLtu, MSNe]
                          [UseUndefinedDest]
                          [UseFullMask]
                          SketchVectorRHS
                      ],
                    numCmp
                  ),
                  ( Branch
                      (Split 1 False)
                      [ Leaf
                          [ sketchDelegatedVectorBinaryOnMask
                              maskElementWiseXMul
                              mmul
                              [And, Add, Or]
                              (SketchImmRHS replicatingImm)
                          ],
                        Leaf
                          [ sketchDelegatedVectorBinaryOnMask
                              maskElementWiseXMul
                              mmul
                              [Srl]
                              (SketchImmRHS boundedImm)
                          ],
                        Leaf
                          [ sketchDelegatedVectorBinaryOnMask
                              maskElementWiseXMul
                              mmul
                              [And, Add, Or]
                              SketchVectorRHS
                          ]
                      ],
                    num
                  )
                ]
                [MaskType mmul]
          )
        ]

{-
lt128Sketch2ShrinkMMCombinedSpecPart1 ::
  Int ->
  Int ->
  Int ->
  [TemplateArgType] ->
  (T.Text, TypeSignature ValueType, SketchSpec (WordN 8))
lt128Sketch2ShrinkMMCombinedSpecPart1 num numVecRes numMaskRes templateArgTypes =
  let [MaskType _ mmul, VectorType _ vtype :: ValueType] =
        toValueType <$> templateArgTypes
      -- xmul = vtypeEewXlenMultiplier vtype
   in ( "lt128Sketch2ShrinkMMCombinedPart1" <> "<" <> showText num <> ">",
        TypeSignature
          [ VectorType (con False) vtype,
            VectorType (con False) vtype
          ]
          ( replicate numVecRes (VectorType (con False) vtype)
              ++ replicate numMaskRes (MaskType (con False) mmul)
      ),
        ComponentSpec $
          ComponentSketchSpec
            [ VectorType (con False) vtype,
              VectorType (con False) vtype
            ]
            [ (Leaf [SketchVSetVLMax mmul [nonePolicy]], 1),
              ( Leaf
                  [ {-SketchElementWiseMI
                      1
                      1
                      [And, Add, Or, Srl, Mul]
                      Nothing,
                    SketchElementWiseMM
                      1
                      1
                      [And, Add, Or],-}
                    SketchCompareVV
                      vtype
                      [MSEq, MSLtu]
                      [UseUndefinedDest]
                      [UseFullMask] -- ,
                      -- SketchCompareVI
                      --   vtype
                      --   [MSEq, MSLtu]
                      --   [UseUndefinedDest]
                      --   [UseFullMask]
                      --   Nothing
                      {-,
                      SketchSingleWidthVI
                        vtype
                        [And, Add, Or, Srl, Mul]
                        [UseUndefinedDest, UseProvidedDest]
                        [UseFullMask, UseProvidedMask]
                        Nothing,
                      SketchSingleWidthVV
                        vtype
                        [And, Add, Or, Srl, Mul]
                        [UseUndefinedDest, UseProvidedDest]
                        [UseFullMask, UseProvidedMask],
                      SketchVMergeVIM vtype [UseUndefinedDest, UseProvidedDest] Nothing,
                      SketchVMergeVVM vtype [UseUndefinedDest, UseProvidedDest],
                      SketchScalarLong xmul Nothing,
                      SketchVSlide1VX
                        vtype
                        [SlideUp, SlideDown]
                        [UseUndefinedDest]
                        [UseFullMask],
                      SketchVSlideVX
                        vtype
                        [SlideUp, SlideDown]
                        [UseProvidedDest]
                        [UseFullMask],
                      SketchVId
                        vtype
                        [UseUndefinedDest, UseProvidedDest]
                        [UseFullMask, UseProvidedMask]-}
                  ],
                num
              )
            ]
            ( replicate numVecRes (VectorType (con False) vtype)
                ++ replicate numMaskRes (MaskType (con False) mmul)
      )

lt128Sketch2ShrinkMMCombinedSpecPart2 ::
  Int -> Int -> Int ->
  [TemplateArgType] ->
      (T.Text, TypeSignature ValueType, SketchSpec (WordN 8))
lt128Sketch2ShrinkMMCombinedSpecPart2 num numVecArg numMaskArg templateArgTypes =
  ( "lt128Sketch2ShrinkMMCombinedPart2" <> "<" <> showText num <> ">",
    TypeSignature
      ( replicate numVecArg (VectorType (con False) vtypeE1M1)
          ++ replicate numMaskArg (MaskType (con False) 1)
      )
      [MaskType (con False) 1],
    ComponentSpec $
      ComponentSketchSpec
        -- [ VectorType (con False) vtypeE1M1,
        --   VectorType (con False) vtypeE1M1
        -- ]
        ( replicate numVecArg (VectorType (con False) vtypeE1M1)
            ++ replicate numMaskArg (MaskType (con False) 1)
        )
        [ -- ([SketchVSetVLMax 1 [nonePolicy]], 1),
          ( Leaf
              [ SketchElementWiseMI
                  1
                  1
                  [And, Add, Or, Srl, Mul]
                  Nothing,
                SketchElementWiseMM
                  1
                  1
                  [ And,
                    Add,
                    Or {-,
                       SketchCompareVV
                         vtypeE1M1
                         [MSEq, MSLtu]
                         [UseUndefinedDest]
                         [UseFullMask],
                       SketchCompareVI
                         vtypeE1M1
                         [MSEq, MSLtu]
                         [UseUndefinedDest, UseProvidedDest]
                         [UseFullMask, UseProvidedMask]
                         Nothing,
                       SketchSingleWidthVI
                         vtypeE1M1
                         [And, Add, Or, Srl, Mul]
                         [UseUndefinedDest, UseProvidedDest]
                         [UseFullMask, UseProvidedMask]
                         Nothing,
                       SketchSingleWidthVV
                         vtypeE1M1
                         [And, Add, Or, Srl, Mul]
                         [UseUndefinedDest, UseProvidedDest]
                         [UseFullMask, UseProvidedMask],
                       SketchVMergeVIM vtypeE1M1 [UseUndefinedDest, UseProvidedDest] Nothing,
                       SketchVMergeVVM vtypeE1M1 [UseUndefinedDest, UseProvidedDest],
                       SketchScalarLong 1 Nothing,
                       SketchVSlide1VX
                         vtypeE1M1
                         [SlideUp, SlideDown]
                         [UseUndefinedDest]
                         [UseFullMask],
                       SketchVSlideVX
                         vtypeE1M1
                         [SlideUp, SlideDown]
                         [UseProvidedDest]
                         [UseFullMask],
                       SketchVId
                         vtypeE1M1
                         [UseUndefinedDest, UseProvidedDest]
                         [UseFullMask, UseProvidedMask]-}
                  ]
              ],
            num
          )
        ]
        [MaskType (con False) 1]
  )

lt128Sketch2ShrinkMMCombinedSpecPart2'0 ::
  Int -> Int -> Int -> Int -> Int -> (T.Text, TypeSignature ValueType, SketchSpec (WordN 8))
lt128Sketch2ShrinkMMCombinedSpecPart2'0 num numVecArg numMaskArg numVecRes numMaskRes =
  ( "lt128Sketch2ShrinkMMCombinedPart2'0" <> "<" <> showText num <> ">",
    TypeSignature
      ( replicate numVecArg (VectorType (con False) vtypeE1M1)
          ++ replicate numMaskArg (MaskType (con False) 1)
      )
      ( replicate numVecRes (VectorType (con False) vtypeE1M1)
          ++ replicate numMaskRes (MaskType (con False) 1)
      ),
    ComponentSpec $
      ComponentSketchSpec
        -- [ VectorType (con False) vtypeE1M1,
        --   VectorType (con False) vtypeE1M1
        -- ]
        ( replicate numVecArg (VectorType (con False) vtypeE1M1)
            ++ replicate numMaskArg (MaskType (con False) 1)
        )
        [ -- ([SketchVSetVLMax 1 [nonePolicy]], 1),
          ( Leaf
              [ SketchElementWiseMI
                  1
                  1
                  [And, Add, Or, Srl, Mul]
                  Nothing,
                SketchElementWiseMM
                  1
                  1
                  [ And,
                    Add,
                    Or
                  ] -- ,
                  {-,
                    SketchCompareVV
                      vtypeE1M1
                      [MSEq, MSLtu]
                      [UseUndefinedDest]
                      [UseFullMask],
                    SketchCompareVI
                      vtypeE1M1
                      [MSEq, MSLtu]
                      [UseUndefinedDest, UseProvidedDest]
                      [UseFullMask, UseProvidedMask]
                      Nothing,-}
                  -- SketchSingleWidthVI
                  --   vtypeE1M1
                  --   [And, Add, Or, Srl, Mul]
                  --   [UseUndefinedDest, UseProvidedDest]
                  --   [UseFullMask, UseProvidedMask]
                  --   Nothing,
                  -- SketchSingleWidthVV
                  --   vtypeE1M1
                  --   [And, Add, Or, Srl, Mul]
                  --   [UseUndefinedDest, UseProvidedDest]
                  --   [UseFullMask, UseProvidedMask]--,
                  -- SketchVMergeVIM vtypeE1M1 [UseUndefinedDest, UseProvidedDest] Nothing,
                  -- SketchVMergeVVM vtypeE1M1 [UseUndefinedDest, UseProvidedDest],
                  -- SketchScalarLong 1 Nothing,
                  -- SketchVSlide1VX
                  --   vtypeE1M1
                  --   [SlideUp, SlideDown]
                  --   [UseUndefinedDest]
                  --   [UseFullMask],
                  -- SketchVSlideVX
                  --   vtypeE1M1
                  --   [SlideUp, SlideDown]
                  --   [UseProvidedDest]
                  --   [UseFullMask],
                  -- SketchVId
                  --   vtypeE1M1
                  --   [UseUndefinedDest, UseProvidedDest]
                  --   [UseFullMask, UseProvidedMask]
              ],
            num
          )
        ]
        ( replicate numVecRes (VectorType (con False) vtypeE1M1)
            ++ replicate numMaskRes (MaskType (con False) 1)
        )
  )

lt128Sketch2ShrinkMMCombinedSpecAll ::
  Int -> Int -> Int -> Int -> Int -> Int -> Int -> SketchSpecTable (WordN 8)
lt128Sketch2ShrinkMMCombinedSpecAll num1 num2 num3 numVecArg numMaskArg numVecArg2 numMaskArg2 =
  SketchSpecTable $ do
    let (name1, type1, spec1) = lt128Sketch2ShrinkMMCombinedSpecPart1 num1 numVecArg numMaskArg
    let (name2', type2', spec2') = lt128Sketch2ShrinkMMCombinedSpecPart2'0 num2 numVecArg numMaskArg numVecArg2 numMaskArg2
    let (name2, type2, spec2) = lt128Sketch2ShrinkMMCombinedSpecPart2 num3 numVecArg2 numMaskArg2
    [ (name1, spec1),
      (name2', spec2'),
      (name2, spec2),
      ( lt128SketchSymbol,
        ConcreteSpec
          $ Concrete.buildProg
            [ ("a", VectorType (con False) vtypeE1M1),
              ("b", VectorType (con False) vtypeE1M1)
            ]
          $ \[a, b] ->
            let l = Concrete.node (Leaf [SketchNestedProg type1 name1]) (numVecArg + numMaskArg) [a, b]
                r = Concrete.node (Leaf [SketchNestedProg type2' name2']) (numVecArg2 + numMaskArg2) l
                [p] = Concrete.node (Leaf [SketchNestedProg type2 name2]) 1 r
             in [(p, MaskType (con False) 1)]
      )
      ]

lt128Sketch2ShrinkMMCombinedSpec3 :: Int -> SketchSpecTable (WordN 8)
lt128Sketch2ShrinkMMCombinedSpec3 num =
  SketchSpecTable
    [ ( lt128SketchSymbol,
        ComponentSpec $
          ComponentSketchSpec
            [ VectorType (con False) vtypeE1M1,
              VectorType (con False) vtypeE1M1
            ]
            [ (Leaf [SketchVSetVLMax 1 [nonePolicy]], 1),
              ( ToSplitList
                  [ ToSplitList
                      [ Leaf
                          [ SketchCompareVV
                              vtypeE1M1
                              [MSEq, MSLtu]
                              [UseUndefinedDest]
                              [UseFullMask],
                            SketchCompareVI
                              vtypeE1M1
                              [MSEq, MSLtu]
                              [UseUndefinedDest, UseProvidedDest]
                              [UseFullMask, UseProvidedMask]
                              Nothing
                          ],
                        ToSplitList
                          [ Leaf
                              [ SketchElementWiseMI
                                  1
                                  1
                                  [And, Add, Or, Srl, Mul]
                                  Nothing,
                                SketchElementWiseMM
                                  1
                                  1
                                  [And, Add, Or]
                              ],
                            Leaf
                              [ SketchSingleWidthVI
                                  vtypeE1M1
                                  [And, Add, Or, Srl, Mul]
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask]
                                  Nothing,
                                SketchSingleWidthVV
                                  vtypeE1M1
                                  [And, Add, Or, Srl, Mul]
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask]
                              ]
                          ]
                          4
                      ]
                      2,
                    ToSplitList
                      [ Leaf
                          [ SketchVMergeVIM vtypeE1M1 [UseUndefinedDest, UseProvidedDest] Nothing,
                            SketchVMergeVVM vtypeE1M1 [UseUndefinedDest, UseProvidedDest]
                          ],
                        ToSplitList
                          [ Leaf
                              [ SketchVSlide1VX
                                  vtypeE1M1
                                  [SlideUp, SlideDown]
                                  [UseUndefinedDest]
                                  [UseFullMask],
                                SketchVSlideVX
                                  vtypeE1M1
                                  [SlideUp, SlideDown]
                                  [UseProvidedDest]
                                  [UseFullMask]
                              ],
                            Leaf
                              [ SketchScalarLong 1 Nothing,
                                SketchVId
                                  vtypeE1M1
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask]
                              ]
                          ]
                          5
                      ]
                      3
                  ]
                  1,
                num
              )
            ]
            [MaskType (con False) 1]
      )
    ]
    -}

lt128SubProg ::
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
lt128SubProg
  useArbitraryImm
  maskElementWiseBits
  key
  num
  numVecArg
  numMaskArg
  numVecRes
  numMaskRes
  templateArgTypes =
    let [ maskType@(MaskType mmul),
          vectorType@(VectorType vtype) :: ValueType
          ] =
            toValueType <$> templateArgTypes
        argTypes = replicate numVecArg vectorType ++ replicate numMaskArg maskType
        resTypes = replicate numVecRes vectorType ++ replicate numMaskRes maskType
        xmul = elementWidthMul vtype
        replicatingImm = if useArbitraryImm then ArbitraryImmSpec else ReplicatingImmSpec 2
        maskShiftImm = if useArbitraryImm then ArbitraryImmSpec else BoundedImmSpec False 0 1
        smallImm = if useArbitraryImm then ArbitraryImmSpec else BoundedImmSpec False 0 3
        -- smallOrReplicatingImm = replicatingImm ++ smallImm
        maskElementWiseXMul = WidthMul $ maskElementWiseBits % 64
     in ( key,
          TypeSignature argTypes resTypes,
          CompSpec $
            ComponentBag
              (toSym argTypes)
              [ (Leaf [sketchSetMaxVectorLength mmul [nonePolicy]], 1),
                ( Branch
                    (Split 1 False)
                    [ Leaf
                        [ sketchVectorCompare
                            vtype
                            [MSEq, MSLtu, MSNe]
                            [UseUndefinedDest]
                            [UseFullMask]
                            SketchVectorRHS
                        ],
                      Branch
                        (Split 2 False)
                        [ Branch
                            (Split 3 False)
                            [ Leaf
                                [ sketchDelegatedVectorBinaryOnMask
                                    maskElementWiseXMul
                                    mmul
                                    [And, Add, Or]
                                    (SketchImmRHS replicatingImm),
                                  sketchDelegatedVectorBinaryOnMask
                                    maskElementWiseXMul
                                    mmul
                                    [Srl]
                                    (SketchImmRHS maskShiftImm),
                                  sketchDelegatedVectorBinaryOnMask
                                    maskElementWiseXMul
                                    mmul
                                    [Mul]
                                    (SketchImmRHS smallImm),
                                  sketchDelegatedVectorBinaryOnMask
                                    maskElementWiseXMul
                                    mmul
                                    [And, Add, Or]
                                    SketchVectorRHS
                                ],
                              Leaf
                                [ inj $
                                    SketchSingleWidthIntBinary
                                      vtype
                                      [And, Add, Or, Srl, Mul]
                                      [UseUndefinedDest, UseProvidedDest]
                                      [UseFullMask, UseProvidedMask]
                                      (SketchImmRHS smallImm),
                                  inj $
                                    SketchSingleWidthIntBinary
                                      vtype
                                      [And, Add, Or, Srl, Mul]
                                      [UseUndefinedDest, UseProvidedDest]
                                      [UseFullMask, UseProvidedMask]
                                      (SketchImmRHS replicatingImm),
                                  inj $
                                    SketchSingleWidthIntBinary
                                      vtype
                                      [And, Add, Or, Srl, Mul]
                                      [UseUndefinedDest, UseProvidedDest]
                                      [UseFullMask, UseProvidedMask]
                                      SketchVectorRHS
                                ]
                            ],
                          Branch
                            (Split 4 False)
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
                                    SketchVectorRHS,
                                  sketchSlide1
                                    vtype
                                    [SlideUp, SlideDown]
                                    [UseUndefinedDest]
                                    [UseFullMask]
                                    SketchScalarRHS,
                                  sketchSlide1
                                    vtype
                                    [SlideUp, SlideDown]
                                    [UseProvidedDest]
                                    [UseFullMask]
                                    SketchScalarRHS
                                ],
                              Leaf
                                [ sketchScalarLongImm xmul smallImm,
                                  sketchScalarLongImm xmul replicatingImm,
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
              (toSym resTypes)
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
  | SubProgInferred
  | SubProgFixedCmp {_num :: Int}
  | Simple {_numCmp :: Int, _num :: Int}
  | Inferred {_num :: Int}
  | FixedCmpInferred {_num :: Int}
  | Sketch191
  | Optimal
  | FixedCmpOptimal
  deriving (Generic)
  deriving (PPrint) via (Default SketchSelector)

data Args = Args
  { useArbitraryImm :: Bool,
    maskElementWiseBits :: Int,
    sketchSelector :: SketchSelector
  }
  deriving (Generic)
  deriving (PPrint) via (Default Args)

sketchSelectorReader :: ReadM SketchSelector
sketchSelectorReader = do
  s <- str
  if
    | "191" `T.isPrefixOf` s ->
        return Sketch191
    | "sub_prog_inferred" `T.isPrefixOf` s ->
        return SubProgInferred
    | "sub_prog" `T.isPrefixOf` s -> do
        let [num1, num2, num3, numVecArg, numMaskArg, numVecArg2, numMaskArg2] =
              read . T.unpack <$> T.splitOn "_" (T.drop 9 s)
        return $ SubProg num1 num2 num3 numVecArg numMaskArg numVecArg2 numMaskArg2
    | "fixed_cmp_inferred_" `T.isPrefixOf` s -> do
        let num = read $ T.unpack $ T.drop 19 s
        return $ FixedCmpInferred num
    | "fixed_cmp_optimal" `T.isPrefixOf` s ->
        return FixedCmpOptimal
    | "fixed_cmp_" `T.isPrefixOf` s -> do
        let num = read $ T.unpack $ T.drop 10 s
        return $ SubProgFixedCmp num
    | "simple_" `T.isPrefixOf` s -> do
        let [numCmp, num] = read . T.unpack <$> T.splitOn "_" (T.drop 7 s)
        return $ Simple numCmp num
    | "inferred_" `T.isPrefixOf` s -> do
        let num = read $ T.unpack $ T.drop 9 s
        return $ Inferred num
    | "optimal" `T.isPrefixOf` s ->
        return Optimal
    | otherwise ->
        error $ "Invalid sketch selector: " <> T.unpack s

lt128WithSubProgInferred ::
  Int ->
  ConSymbolTable (WordN 8) ->
  T.Text ->
  [TemplateArgType] ->
  (AllowPartialVL, SketchSpecTable (WordN 8))
lt128WithSubProgInferred
  maskElementWiseBits
  conProg
  lt128SketchSymbol
  templateArgTypes =
    let [_maskType, _vectorType :: ValueType] = toValueType <$> templateArgTypes
     in inferSketch
          (WidthMul $ maskElementWiseBits % 64)
          7
          conProg
          lt128SketchSymbol
          id

-- [id, id]

lt128WithSubProg ::
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
lt128WithSubProg
  num1
  num2
  num3
  numVecArg
  numMaskArg
  numVecArg2
  numMaskArg2
  useArbitraryImm
  maskElementWiseBits
  lt128SketchSymbol
  templateArgTypes =
    SymbolTable $ do
      let (name1, type1, spec1) = lt128SubProg useArbitraryImm maskElementWiseBits "prog1" num1 2 0 numVecArg numMaskArg templateArgTypes
      let (name2, type2, spec2) = lt128SubProg useArbitraryImm maskElementWiseBits "prog2" num2 numVecArg numMaskArg numVecArg2 numMaskArg2 templateArgTypes
      let (name3, type3, spec3) =
            lt128SubProg useArbitraryImm maskElementWiseBits "prog3" num3 numVecArg2 numMaskArg2 0 1 templateArgTypes
      let [maskType, vectorType :: ValueType] = toValueType <$> templateArgTypes
      [ (name1, spec1),
        (name2, spec2),
        (name3, spec3),
        ( lt128SketchSymbol,
          ConSpec $
            Concrete.buildProg [("a", vectorType), ("b", vectorType)] $
              \[a, b] -> do
                l <-
                  Concrete.noden
                    (Leaf [inj @(Invoke ValueType) $ Invoke (toSym type1) name1])
                    (numVecArg + numMaskArg)
                    [a, b]
                r <-
                  Concrete.noden
                    (Leaf [inj @(Invoke ValueType) $ Invoke (toSym type2) name2])
                    (numVecArg2 + numMaskArg2)
                    l
                p <-
                  Concrete.node1
                    (Leaf [inj @(Invoke ValueType) $ Invoke (toSym type3) name3])
                    r
                return [(p, maskType)]
        )
        ]

lt128WithSubProgFixedCmp ::
  Int ->
  Bool ->
  Int ->
  T.Text ->
  [TemplateArgType] ->
  SketchSpecTable (WordN 8)
lt128WithSubProgFixedCmp
  num
  useArbitraryImm
  maskElementWiseBits
  lt128SketchSymbol
  templateArgTypes =
    SymbolTable $ do
      let (name1, type1, spec1) =
            lt128SubProg useArbitraryImm maskElementWiseBits "prog1" num 0 2 0 1 templateArgTypes
      let [ maskType@(MaskType mmul),
            vectorType@(VectorType vtype) :: ValueType
            ] = toValueType <$> templateArgTypes
      [ (name1, spec1),
        ( lt128SketchSymbol,
          ConProg $
            Concrete.buildProg [("a", vectorType), ("b", vectorType)] $
              \[ar, br] -> do
                vlmax <- Concrete.node1 (setMaxVectorLength @'C mmul muPolicy) []
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
                    (inj @(Invoke ValueType) $ Invoke type1 name1)
                    [sltu, eq]
                return [(r, maskType)]
        )
        ]

lt128WithSubProgFixedCmpInferred ::
  Int ->
  Int ->
  ConSymbolTable (WordN 8) ->
  T.Text ->
  [TemplateArgType] ->
  (AllowPartialVL, SketchSpecTable (WordN 8))
lt128WithSubProgFixedCmpInferred
  num
  maskElementWiseBits
  conTable
  lt128SketchSymbol
  templateArgTypes = do
    let [ maskType@(MaskType mmul),
          vectorType@(VectorType vtype) :: ValueType
          ] = toValueType <$> templateArgTypes
    let features =
          featureSetDifference
            (extractProgTableFeature conTable)
            mempty {opFeatures = HS.singleton Compare}
    let allowPartialVL = allowPartialVLFromFeatures features
    let spec1 =
          sketchFromFeatures
            (WidthMul $ maskElementWiseBits % 64)
            num
            features
            [VLType mmul, MaskType mmul, MaskType mmul]
            [MaskType mmul]
    let type1 =
          TypeSignature
            [VLType mmul, MaskType mmul, MaskType mmul]
            [MaskType mmul]
    ( allowPartialVL,
      SymbolTable $ do
        -- let (name1, type1, spec1) =
        --       lt128SubProg useArbitraryImm maskElementWiseBits "prog1" num 0 2 0 1 templateArgTypes
        [ ("prog", CompSpec spec1),
          ( lt128SketchSymbol,
            ConProg $
              Concrete.buildProg [("a", vectorType), ("b", vectorType)] $
                \[ar, br] -> do
                  vlmax <- Concrete.node1 (setMaxVectorLength @'C mmul muPolicy) []
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
                      (inj @(Invoke ValueType) $ Invoke type1 "prog")
                      [vlmax, sltu, eq]
                  return [(r, maskType)]
          )
          ]
      )

lt128Sketch :: Args -> ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> (AllowPartialVL, SketchSpecTable (WordN 8))
lt128Sketch (Args useArbitraryImm maskElementWiseBits sketchSelector) conTable symbol templateArgTypes =
  case sketchSelector of
    SubProg num1 num2 num3 numVecArg numMaskArg numVecArg2 numMaskArg2 ->
      ( DisallowPartialVL,
        lt128WithSubProg
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
    SubProgInferred ->
      lt128WithSubProgInferred maskElementWiseBits conTable symbol templateArgTypes
    SubProgFixedCmp num ->
      (DisallowPartialVL, lt128WithSubProgFixedCmp num useArbitraryImm maskElementWiseBits symbol templateArgTypes)
    Simple numCmp num ->
      (DisallowPartialVL, lt128SimpleSketch numCmp num useArbitraryImm maskElementWiseBits symbol templateArgTypes)
    Inferred num ->
      inferSketch (WidthMul $ maskElementWiseBits % 64) num conTable symbol id
    FixedCmpInferred num ->
      lt128WithSubProgFixedCmpInferred num maskElementWiseBits conTable symbol templateArgTypes
    Sketch191 ->
      (DisallowPartialVL, lt128Sketch191 maskElementWiseBits conTable symbol templateArgTypes)
    Optimal ->
      (DisallowPartialVL, lt128OptimalSketch maskElementWiseBits conTable symbol templateArgTypes)
    FixedCmpOptimal ->
      (DisallowPartialVL, fixedCmpOptimalSketch maskElementWiseBits conTable symbol templateArgTypes)

argsParser :: Parser Args
argsParser =
  Args
    <$> switch
      ( long "arbitrary-imm"
          <> help
            ( "Use arbitrary imm values in the sketch. "
                <> "Default is bounded imm values."
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
    <*> option
      sketchSelectorReader
      ( long "sketch-selector"
          <> help
            ( "Select the sketch to use. "
                <> "The format is 'sub_prog_<num1>_<num2>_<num3>_<numVecArg>_<numMaskArg>_<numVecArg2>_<numMaskArg2>' "
                <> "or 'fixed_cmp_<num>' or 'simple_<numCmp>_<num>' or 'inferred_<num>'."
            )
          <> showDefaultWith (const "fixed_cmp_inferred_6")
          <> value (FixedCmpInferred 6)
      )
