{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.PrettyPrintingTest (prettyPrintingTest) where

import Control.Monad.Identity (Identity (Identity))
import qualified Data.Text as T
import Grisette (PPrint (pformat), WordN)
import HieraSynth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.Util.Pretty (renderDoc)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Policy (muPolicy, tuPolicy)
import RVV.Semantics.VectorConfigConstants
  ( vectorConfigE1M4,
    vectorConfigEF2M1,
    vectorConfigEF2M2,
    vectorConfigEF2M4,
  )
import RVV.Synthesizer.Op (ConOp)
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (ImmRHS, ScalarRHS, VectorRHS),
  )
import RVV.Synthesizer.Operator.Extract (Extract (ExtractMask, ExtractVector, VectorTruncate))
import RVV.Synthesizer.Operator.FixedPointClip (FixedPointClip (FixedPointClip))
import RVV.Synthesizer.Operator.Insert (Insert (InsertMask, InsertVector), VectorExtend (VectorExtend))
import RVV.Synthesizer.Operator.Load (Load (Load))
import RVV.Synthesizer.Operator.MaskLogical (MaskLogical (MaskLogical))
import RVV.Synthesizer.Operator.Merge
  ( Merge (Merge),
  )
import RVV.Synthesizer.Operator.MiscMask
  ( MiscMask (MaskFirstOne, MaskPopCount, MaskSetBit),
  )
import RVV.Synthesizer.Operator.Move
  ( Move (MoveScalarToFirstElement, MoveToMask, MoveToVector),
    MoveFirstElementToScalar (MoveFirstElementToScalar),
  )
import RVV.Synthesizer.Operator.NarrowingRightShift (NarrowingRightShift (NarrowingRightShift))
import RVV.Synthesizer.Operator.Reinterpret
  ( Reinterpret (MaskToMask, MaskToVector, PtrToScalar, ScalarToPtr, VLToScalar, VectorToMask, VectorToVector),
  )
import RVV.Synthesizer.Operator.Scalar
  ( Scalar (ScalarLongImm),
  )
import RVV.Synthesizer.Operator.ScalarOperator (ScalarOperator (ScalarBin, ScalarBinVectorLength))
import RVV.Synthesizer.Operator.SetVectorLength
  ( setMaxVectorLength,
    setRelayedVectorLength,
    setVectorLength,
  )
import RVV.Synthesizer.Operator.SingleWidthIntBinary
  ( SingleWidthIntBinary (SingleWidthIntBinary),
    singleWidthIntBinary,
  )
import RVV.Synthesizer.Operator.SingleWidthMulAdd (SingleWidthMulAdd (SingleWidthMulAdd))
import RVV.Synthesizer.Operator.Slide
  ( Slide (Slide, Slide1),
  )
import RVV.Synthesizer.Operator.Store (Store (Store))
import RVV.Synthesizer.Operator.Undefined (Undefined (UndefinedMask, UndefinedVector))
import RVV.Synthesizer.Operator.VectorCompare
  ( VectorCompare (VectorCompare),
  )
import RVV.Synthesizer.Operator.VectorIndex
  ( VectorIndex (VectorId, VectorIota),
  )
import RVV.Synthesizer.Operator.Vlenb (Vlenb (Vlenb))
import RVV.Synthesizer.Operator.WideningIntBinary (WideningIntBinary (WideningIntBinary))
import RVV.Synthesizer.Operator.WideningMulAdd (WideningMulAdd (WideningMulAdd))
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Parameter.FixedPointRoundingMode
  ( mkFixedRDN,
    mkFixedRNE,
    mkFixedRNU,
  )
import RVV.Synthesizer.Parameter.IntCompareOpCode (mkMSEq)
import RVV.Synthesizer.Parameter.MaskLogicalOpCode (mkMAnd)
import RVV.Synthesizer.Parameter.Masking (fm, pm)
import RVV.Synthesizer.Parameter.NarrowingRightShiftOpCode (mkNSra, mkNSrl)
import RVV.Synthesizer.Parameter.SetMaskMethod
  ( SetMaskMethod (BeforeFirst),
  )
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode (mkAdd)
import RVV.Synthesizer.Parameter.SingleWidthMulAddOpCode (mkMAcc, mkMAdd)
import RVV.Synthesizer.Parameter.SlideDirection (mkSlideDown, mkSlideUp)
import RVV.Synthesizer.Parameter.WideningIntBinaryOpCode (mkWAdd)
import RVV.Synthesizer.Parameter.WideningMulAddOpCode (mkWMAcc, mkWMAccu)
import RVV.Synthesizer.Type (ValueType (ScalarType, VectorType))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

prettyPrintingTest :: Test
prettyPrintingTest =
  testGroup
    "PrettyPrinting"
    [ testGroup
        "Op"
        [ testCase "VSetVL" $ do
            let actual =
                  renderDoc 80 (pformat (setVectorLength @'C 2 tuPolicy :: ConOp))
            actual @?= "vsetvl[mmul=2, tuma]",
          testCase "VSetVLMax" $ do
            let actual =
                  renderDoc
                    80
                    (pformat (setMaxVectorLength @'C (1 / 2) muPolicy :: ConOp))
            actual @?= "vsetvlmax[mmul=f2, tamu]",
          testCase "VSetVLRelay" $ do
            let actual =
                  renderDoc 80 (pformat (setRelayedVectorLength @'C 2 (1 / 4) tuPolicy :: ConOp))
            actual @?= "vsetvlrelay[mmul=2, src_mmul=f4, tuma]",
          testCase "UndefinedVector" $ do
            let actual =
                  renderDoc 80 $
                    pformat (UndefinedVector vectorConfigEF2M2)
            actual @?= "undefined[ef2m2]",
          testCase "UndefinedVMask" $ do
            let actual = renderDoc 80 $ pformat (UndefinedMask 2)
            actual @?= "undefined_mask[mmul=2]",
          testCase "SingleWidthIntBinary" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( SingleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd fm (ImmRHS (ConstImm 2))
                      )
            actual @?= "vadd.vi[ef2m2, pd, fm, rhs=0x0000000000000002]",
          testCase "WideningVV" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( WideningIntBinary vectorConfigEF2M2 mkWAdd pd pm False VectorRHS
                      )
            actual @?= "vwadd.vv[wide=ef2m2, pd, pm]",
          testCase "WideningVX" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( WideningIntBinary vectorConfigEF2M2 mkWAdd ud fm False ScalarRHS
                      )
            actual @?= "vwadd.vx[wide=ef2m2, ud, fm]",
          testCase "WideningVI" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( WideningIntBinary vectorConfigEF2M2 mkWAdd ud fm False (ImmRHS (ConstImm 1))
                      )
            actual @?= "vwadd.vi[wide=ef2m2, ud, fm, rhs=0x0000000000000001]",
          testCase "WideningWV" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( WideningIntBinary vectorConfigEF2M2 mkWAdd ud pm True VectorRHS
                      )
            actual @?= "vwadd.wv[wide=ef2m2, ud, pm]",
          testCase "WideningWX" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( WideningIntBinary vectorConfigEF2M2 mkWAdd pd fm True ScalarRHS
                      )
            actual @?= "vwadd.wx[wide=ef2m2, pd, fm]",
          testCase "WideningWI" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( WideningIntBinary vectorConfigEF2M2 mkWAdd pd fm True (ImmRHS (ConstImm 1))
                      )
            actual @?= "vwadd.wi[wide=ef2m2, pd, fm, rhs=0x0000000000000001]",
          testCase "FixedPointClip[vector_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat (FixedPointClip vectorConfigEF2M2 True mkFixedRDN ud fm VectorRHS)
            actual @?= "vnclip.rdn.wv[narrow=ef2m2, ud, fm]",
          testCase "FixedPointClip[scalar_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat (FixedPointClip vectorConfigEF2M2 False mkFixedRNE ud pm ScalarRHS)
            actual @?= "vnclipu.rne.wx[narrow=ef2m2, ud, pm]",
          testCase "FixedPointClip[imm_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat (FixedPointClip vectorConfigEF2M2 True mkFixedRNU ud pm (ImmRHS (ConstImm 1)))
            actual @?= "vnclip.rnu.wi[narrow=ef2m2, ud, pm, rhs=0x0000000000000001]",
          testCase "NarrowingRightShift[vector_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat (NarrowingRightShift @'C vectorConfigEF2M2 mkNSra ud fm VectorRHS)
            actual @?= "vnsra.wv[narrow=ef2m2, ud, fm]",
          testCase "NarrowingRightShift[scalar_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat (NarrowingRightShift @'C vectorConfigEF2M2 mkNSrl ud pm ScalarRHS)
            actual @?= "vnsrl.wx[narrow=ef2m2, ud, pm]",
          testCase "NarrowingRightShift[imm_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat (NarrowingRightShift @'C vectorConfigEF2M2 mkNSrl ud pm (ImmRHS (ConstImm 1)))
            actual @?= "vnsrl.wi[narrow=ef2m2, ud, pm, rhs=0x0000000000000001]",
          testCase "SingleWidthMulAdd[vector_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( SingleWidthMulAdd @'C vectorConfigEF2M2 mkMAcc fm VectorRHS
                      )
            actual @?= "vmacc.vv[ef2m2, fm]",
          testCase "SingleWidthMulAdd[scalar_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( SingleWidthMulAdd @'C vectorConfigEF2M2 mkMAdd pm ScalarRHS
                      )
            actual @?= "vmadd.vx[ef2m2, pm]",
          testCase "WideningMulAdd[vector_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( WideningMulAdd @'C vectorConfigEF2M2 mkWMAcc fm VectorRHS
                      )
            actual @?= "vwmacc.vv[wide=ef2m2, fm]",
          testCase "WideningMulAdd[scalar_rhs]" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( WideningMulAdd @'C vectorConfigEF2M2 mkWMAccu pm ScalarRHS
                      )
            actual @?= "vwmaccu.vx[wide=ef2m2, pm]",
          testCase "Load" $ do
            let actual =
                  renderDoc 80 $
                    pformat (Load @'C vectorConfigEF2M2 1 ud pm)
            actual @?= "vle[ef2m2, block=1, ud, pm]",
          testCase "Store" $ do
            let actual =
                  renderDoc 80 $
                    pformat (Store @'C vectorConfigEF2M2 0 fm)
            actual @?= "vse[ef2m2, block=0, fm]",
          testCase "ScalarBin" $ do
            let actual = renderDoc 80 $ pformat (ScalarBin @'C mkAdd (1 / 4) ScalarRHS)
            actual @?= "add.xx[width=f4]",
          testCase "ScalarBinImm" $ do
            let actual =
                  renderDoc 80 $
                    pformat (ScalarBin @'C mkAdd (1 / 4) (ImmRHS @'C $ ConstImm 4))
            actual @?= "add.xi[width=f4, rhs=0x0000000000000004]",
          testCase "ScalarBinVectorLength" $ do
            let actual = renderDoc 80 $ pformat (ScalarBinVectorLength @'C mkAdd 2 ScalarRHS)
            actual @?= "add.vl.x[mmul=2]",
          testCase "VSlideVX (up)" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      (Slide @'C vectorConfigEF2M2 mkSlideUp ud fm ScalarRHS)
            actual @?= "vslideup.vx[ef2m2, ud, fm]",
          testCase "VSlideVX (down)" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      ( Slide @'C vectorConfigEF2M2 mkSlideDown pd pm ScalarRHS
                      )
            actual @?= "vslidedown.vx[ef2m2, pd, pm]",
          testCase "VSlideVI (up)" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      (Slide @'C vectorConfigEF2M2 mkSlideUp pd fm (ImmRHS (ConstImm 2)))
            actual @?= "vslideup.vi[ef2m2, pd, fm, offset=0x0000000000000002]",
          testCase "VSlideVI (down)" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      (Slide @'C vectorConfigEF2M2 mkSlideDown ud pm (ImmRHS (ConstImm 2)))
            actual @?= "vslidedown.vi[ef2m2, ud, pm, offset=0x0000000000000002]",
          testCase "VSlide1VX (up)" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      (Slide1 @'C vectorConfigEF2M2 mkSlideUp pd fm ScalarRHS)
            actual @?= "vslide1up.vx[ef2m2, pd, fm]",
          testCase "VSlide1VX (down)" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      (Slide1 @'C vectorConfigEF2M2 mkSlideDown ud pm ScalarRHS)
            actual @?= "vslide1down.vx[ef2m2, ud, pm]",
          testCase "VSet" $ do
            let actual =
                  renderDoc 80 $
                    pformat (InsertVector @'C vectorConfigEF2M2 vectorConfigEF2M4 ud 2)
            actual @?= "vset[part=ef2m2, dest=ef2m4, ud, idx=2]",
          testCase "VLMulExtend" $ do
            let actual =
                  renderDoc 80 $
                    pformat (VectorExtend vectorConfigEF2M2 vectorConfigEF2M4)
            actual @?= "vlmul_extend[part=ef2m2, dest=ef2m4]",
          testCase "VGet" $ do
            let actual =
                  renderDoc 80 $
                    pformat (ExtractVector vectorConfigEF2M2 vectorConfigEF2M4 2)
            actual @?= "vget[part=ef2m2, src=ef2m4, idx=2]",
          testCase "VLMulTruncate" $ do
            let actual =
                  renderDoc 80 $
                    pformat (VectorTruncate vectorConfigEF2M2 vectorConfigEF2M4)
            actual @?= "vlmul_truncate[part=ef2m2, src=ef2m4]",
          testCase "InsertMask" $ do
            let actual =
                  renderDoc 80 $
                    pformat (InsertMask @'C 2 4 pd 0)
            actual @?= "insert_mask[part=2, dest=4, pd, idx=0]",
          testCase "ExtractMask" $ do
            let actual =
                  renderDoc 80 $
                    pformat (ExtractMask 2 4 1)
            actual
              @?= "extract_mask[part=2, src=4, idx=1]",
          testCase "PtrToScalar" $ do
            let actual = renderDoc 80 $ pformat (PtrToScalar (1 / 4) 0 :: Reinterpret)
            actual @?= "ptr_to_scalar[xmul=f4, block=0]",
          testCase "VLToScalar" $ do
            let actual = renderDoc 80 $ pformat (VLToScalar 4 :: Reinterpret)
            actual @?= "vl_to_scalar[mmul=4]",
          testCase "ScalarToPtr" $ do
            let actual = renderDoc 80 $ pformat (ScalarToPtr (1 / 4) 0 :: Reinterpret)
            actual @?= "scalar_to_ptr[xmul=f4, block=0]",
          testCase "CompareVV" $ do
            let actual =
                  renderDoc 80 $
                    pformat (VectorCompare @'C vectorConfigEF2M2 mkMSEq ud fm VectorRHS)
            actual @?= "vmseq.vv[ef2m2, ud, fm]",
          testCase "CompareVX" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      (VectorCompare @'C vectorConfigEF2M2 mkMSEq pd pm ScalarRHS)
            actual @?= "vmseq.vx[ef2m2, pd, pm]",
          testCase "CompareVI" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      (VectorCompare @'C vectorConfigEF2M2 mkMSEq ud pm (ImmRHS (ConstImm 3)))
            actual @?= "vmseq.vi[ef2m2, ud, pm, rhs=0x0000000000000003]",
          testCase "MaskLogical" $ do
            let actual =
                  renderDoc 80 $
                    pformat (MaskLogical @'C 2 mkMAnd)
            actual @?= "vmand.mm[mmul=2]",
          testCase "VectorToVector" $ do
            let actual =
                  renderDoc 80 $
                    pformat (VectorToVector vectorConfigEF2M4 vectorConfigE1M4)
            actual @?= "vec_to_vec[src=ef2m4, dest=e1m4]",
          testCase "MaskToVector" $ do
            let actual =
                  renderDoc 80 $
                    pformat (MaskToVector 4 vectorConfigEF2M1)
            actual @?= "mask_to_vec[src=4, dest=ef2m1]",
          testCase "VectorToMask" $ do
            let actual =
                  renderDoc 80 $
                    pformat (VectorToMask vectorConfigEF2M1 4)
            actual @?= "vec_to_mask[src=ef2m1, dest=4]",
          testCase "MaskToMask" $ do
            let actual =
                  renderDoc 80 $
                    pformat (MaskToMask 1 2)
            actual @?= "mask_to_mask[src=1, dest=2]",
          testCase "VMergeVVM" $ do
            let actual =
                  renderDoc 80 $
                    pformat (Merge @'C vectorConfigEF2M2 pd VectorRHS)
            actual @?= "vmerge.vvm[ef2m2, pd]",
          testCase "VMergeVXM" $ do
            let actual =
                  renderDoc 80 $
                    pformat (Merge @'C vectorConfigEF2M2 ud ScalarRHS)
            actual @?= "vmerge.vxm[ef2m2, ud]",
          testCase "VMergeVIM" $ do
            let actual =
                  renderDoc 80 $
                    pformat (Merge @'C vectorConfigEF2M2 ud (ImmRHS (ConstImm 4)))
            actual @?= "vmerge.vim[ef2m2, ud, rhs=0x0000000000000004]",
          testCase "VIota" $ do
            let actual =
                  renderDoc 80 $
                    pformat (VectorIota @'C vectorConfigEF2M2 ud pm)
            actual @?= "viota.m[ef2m2, ud, pm]",
          testCase "VId" $ do
            let actual =
                  renderDoc 80 $
                    pformat (VectorId @'C vectorConfigEF2M2 pd fm)
            actual @?= "vid.v[ef2m2, pd, fm]",
          testCase "VCPop" $ do
            let actual = renderDoc 80 $ pformat (MaskPopCount 16 fm :: MiscMask 'C)
            actual @?= "vcpop.m[mmul=16, fm]",
          testCase "VFirst" $ do
            let actual = renderDoc 80 $ pformat (MaskFirstOne 16 pm :: MiscMask 'C)
            actual @?= "vfirst.m[mmul=16, pm]",
          testCase "VSetMask" $ do
            let actual =
                  renderDoc 80 $
                    pformat
                      (MaskSetBit (Identity BeforeFirst) 16 ud pm :: MiscMask 'C)
            actual @?= "vmsbf.m[mmul=16, ud, pm]",
          testCase "ScalarLong" $
            renderDoc 80 (pformat (ScalarLongImm (1 / 4) (ConstImm 0x5) :: Scalar 'C))
              @?= "scalar[xmul=f4, imm=0x0000000000000005]",
          testCase "VMVVV" $
            renderDoc 80 (pformat (MoveToVector vectorConfigEF2M2 ud VectorRHS :: Move 'C))
              @?= "vec_to_vec[ef2m2, ud]",
          testCase "VMVVX" $
            renderDoc 80 (pformat (MoveToVector vectorConfigEF2M2 ud ScalarRHS :: Move 'C))
              @?= "scalar_to_vec[ef2m2, ud]",
          testCase "VMVSX" $
            renderDoc 80 (pformat (MoveScalarToFirstElement vectorConfigEF2M2 ud :: Move 'C))
              @?= "scalar_to_first_element[ef2m2, ud]",
          testCase "VMVXS" $
            renderDoc 80 (pformat (MoveFirstElementToScalar vectorConfigEF2M2))
              @?= "first_element_to_scalar[ef2m2]",
          testCase "VMVVI" $ do
            let actual =
                  renderDoc
                    80
                    (pformat (MoveToVector vectorConfigEF2M2 ud (ImmRHS (ConstImm 0x2)) :: Move 'C))
            actual @?= "imm_to_vec[ef2m2, ud, imm=0x0000000000000002]",
          testCase "VMVMX" $
            renderDoc 80 (pformat (MoveToMask (1 / 2) 4 ScalarRHS :: Move 'C))
              @?= "scalar_to_mask[xmul=f2, mmul=4]",
          testCase "VMVMI" $ do
            let actual =
                  renderDoc
                    80
                    (pformat (MoveToMask (1 / 2) 4 (ImmRHS (ConstImm 0x2)) :: Move 'C))
            actual @?= "imm_to_mask[xmul=f2, mmul=4, imm=0x0000000000000002]",
          testCase "Vlenb" $
            renderDoc 80 (pformat Vlenb)
              @?= "vlenb"
        ],
      testCase "Prog" $ do
        let prog =
              Prog
                [ ProgArg "v1" (0 :: WordN 8) (VectorType vectorConfigEF2M2),
                  ProgArg "v2" 1 (VectorType vectorConfigEF2M2),
                  ProgArg "avl" 2 (ScalarType 1)
                ]
                [ Stmt (setVectorLength @'C 4 tuPolicy :: ConOp) [2] [3],
                  Stmt
                    (singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd ud fm VectorRHS)
                    [0, 1, 3]
                    [4]
                ]
                [ProgRes 4 (VectorType vectorConfigEF2M2)]
        renderDoc 80 (pformat prog)
          @?= T.intercalate
            "\n"
            [ "def anonymous(",
              "  v1: vec<ef2m2>,",
              "  v2: vec<ef2m2>,",
              "  avl: scalar<xmul=1>",
              ") -> vec<ef2m2>:",
              "  vl3 = vsetvl[mmul=4, tuma](avl)",
              "  v4 = vadd.vv[ef2m2, ud, fm](v1, v2, vl3)",
              "  return v4"
            ]
    ]
