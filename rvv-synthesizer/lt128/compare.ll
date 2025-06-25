; ModuleID = 'compare.cpp'
source_filename = "compare.cpp"
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64-unknown-linux-gnu"

$_Z5Lt128Iu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_ = comdat any

$_Z10Lt128UpperIu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_ = comdat any

$_Z5Eq128Iu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_ = comdat any

$_Z10Eq128UpperIu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_ = comdat any

$_Z5Ne128Iu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_ = comdat any

$_Z10Ne128UpperIu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_ = comdat any

$_Z5Lt128Iu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_ = comdat any

$_Z10Lt128UpperIu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_ = comdat any

$_Z5Eq128Iu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_ = comdat any

$_Z10Eq128UpperIu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_ = comdat any

$_Z5Ne128Iu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_ = comdat any

$_Z10Ne128UpperIu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_ = comdat any

$_Z5Lt128Iu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_ = comdat any

$_Z10Lt128UpperIu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_ = comdat any

$_Z5Eq128Iu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_ = comdat any

$_Z10Eq128UpperIu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_ = comdat any

$_Z5Ne128Iu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_ = comdat any

$_Z10Ne128UpperIu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_ = comdat any

$_Z5Lt128Iu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_ = comdat any

$_Z10Lt128UpperIu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_ = comdat any

$_Z5Eq128Iu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_ = comdat any

$_Z10Eq128UpperIu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_ = comdat any

$_Z5Ne128Iu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_ = comdat any

$_Z10Ne128UpperIu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_ = comdat any

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i1> @_Z5Lt128Iu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 -1, <vscale x 1 x i1> %4, i64 %3)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 -1, <vscale x 1 x i1> %7, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1up.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %8, i64 0, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, <vscale x 1 x i64> %9, i64 %3)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vor.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %8, <vscale x 1 x i64> %10, i64 %3)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %11, i64 0, i64 %3)
  %13 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %14 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %13, i64 1, i64 %3)
  %15 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %14, i64 0, i64 %3)
  %16 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %11, <vscale x 1 x i64> %12, <vscale x 1 x i1> %15, i64 %3)
  %17 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.i64.i64(<vscale x 1 x i64> %16, i64 0, i64 %3)
  ret <vscale x 1 x i1> %17
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i1> @_Z10Lt128UpperIu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 -1, <vscale x 1 x i1> %4, i64 %3)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 0, i64 %3)
  %8 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv1i64(<vscale x 1 x i64> %7)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, <vscale x 1 x i64> %7, <vscale x 1 x i1> %11, i64 %3)
  %13 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.i64.i64(<vscale x 1 x i64> %12, i64 0, i64 %3)
  ret <vscale x 1 x i1> %13
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i1> @_Z5Eq128Iu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 -1, <vscale x 1 x i1> %4, i64 %3)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1up.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 0, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %7, <vscale x 1 x i64> %8, <vscale x 1 x i1> %11, i64 %3)
  %13 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, <vscale x 1 x i64> %12, i64 %3)
  %14 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv1i64(<vscale x 1 x i64> %13)
  %15 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.i64.i64(<vscale x 1 x i64> %13, i64 0, i64 %3)
  ret <vscale x 1 x i1> %15
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i1> @_Z10Eq128UpperIu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 -1, <vscale x 1 x i1> %4, i64 %3)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %8, i64 1, i64 %3)
  %10 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %9, i64 0, i64 %3)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, <vscale x 1 x i64> %7, <vscale x 1 x i1> %10, i64 %3)
  %12 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.i64.i64(<vscale x 1 x i64> %11, i64 0, i64 %3)
  ret <vscale x 1 x i1> %12
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i1> @_Z5Ne128Iu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 -1, <vscale x 1 x i1> %4, i64 %3)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1up.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 0, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %7, <vscale x 1 x i64> %8, <vscale x 1 x i1> %11, i64 %3)
  %13 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv1i64(<vscale x 1 x i64> %12)
  %14 = tail call <vscale x 1 x i64> @llvm.riscv.vor.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, <vscale x 1 x i64> %12, i64 %3)
  %15 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.i64.i64(<vscale x 1 x i64> %14, i64 0, i64 %3)
  ret <vscale x 1 x i1> %15
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i1> @_Z10Ne128UpperIu14__rvv_bool64_tu16__rvv_uint64m1_tET_T0_S1_(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 -1, <vscale x 1 x i1> %4, i64 %3)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 0, i64 %3)
  %8 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv1i64(<vscale x 1 x i64> %7)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, <vscale x 1 x i64> %7, <vscale x 1 x i1> %11, i64 %3)
  %13 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.i64.i64(<vscale x 1 x i64> %12, i64 0, i64 %3)
  ret <vscale x 1 x i1> %13
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i1> @_Z5Lt128Iu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %4 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1, i64 %3)
  %5 = tail call <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %5, i64 -1, <vscale x 2 x i1> %4, i64 %3)
  %7 = tail call <vscale x 2 x i1> @llvm.riscv.vmsltu.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1, i64 %3)
  %8 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %5, i64 -1, <vscale x 2 x i1> %7, i64 %3)
  %9 = tail call <vscale x 2 x i64> @llvm.riscv.vslide1up.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %8, i64 0, i64 %3)
  %10 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, <vscale x 2 x i64> %9, i64 %3)
  %11 = tail call <vscale x 2 x i64> @llvm.riscv.vor.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %8, <vscale x 2 x i64> %10, i64 %3)
  %12 = tail call <vscale x 2 x i64> @llvm.riscv.vslide1down.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %11, i64 0, i64 %3)
  %13 = tail call <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64> poison, i64 %3)
  %14 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %13, i64 1, i64 %3)
  %15 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64> %14, i64 0, i64 %3)
  %16 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %11, <vscale x 2 x i64> %12, <vscale x 2 x i1> %15, i64 %3)
  %17 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.i64.i64(<vscale x 2 x i64> %16, i64 0, i64 %3)
  ret <vscale x 2 x i1> %17
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i1> @_Z10Lt128UpperIu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %4 = tail call <vscale x 2 x i1> @llvm.riscv.vmsltu.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1, i64 %3)
  %5 = tail call <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %5, i64 -1, <vscale x 2 x i1> %4, i64 %3)
  %7 = tail call <vscale x 2 x i64> @llvm.riscv.vslide1down.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, i64 0, i64 %3)
  %8 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv2i64(<vscale x 2 x i64> %7)
  %9 = tail call <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64> poison, i64 %3)
  %10 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, <vscale x 2 x i64> %7, <vscale x 2 x i1> %11, i64 %3)
  %13 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.i64.i64(<vscale x 2 x i64> %12, i64 0, i64 %3)
  ret <vscale x 2 x i1> %13
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i1> @_Z5Eq128Iu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %4 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1, i64 %3)
  %5 = tail call <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %5, i64 -1, <vscale x 2 x i1> %4, i64 %3)
  %7 = tail call <vscale x 2 x i64> @llvm.riscv.vslide1up.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 2 x i64> @llvm.riscv.vslide1down.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, i64 0, i64 %3)
  %9 = tail call <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64> poison, i64 %3)
  %10 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %7, <vscale x 2 x i64> %8, <vscale x 2 x i1> %11, i64 %3)
  %13 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, <vscale x 2 x i64> %12, i64 %3)
  %14 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv2i64(<vscale x 2 x i64> %13)
  %15 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.i64.i64(<vscale x 2 x i64> %13, i64 0, i64 %3)
  ret <vscale x 2 x i1> %15
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i1> @_Z10Eq128UpperIu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %4 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1, i64 %3)
  %5 = tail call <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %5, i64 -1, <vscale x 2 x i1> %4, i64 %3)
  %7 = tail call <vscale x 2 x i64> @llvm.riscv.vslide1down.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64> poison, i64 %3)
  %9 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %8, i64 1, i64 %3)
  %10 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64> %9, i64 0, i64 %3)
  %11 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, <vscale x 2 x i64> %7, <vscale x 2 x i1> %10, i64 %3)
  %12 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.i64.i64(<vscale x 2 x i64> %11, i64 0, i64 %3)
  ret <vscale x 2 x i1> %12
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i1> @_Z5Ne128Iu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %4 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1, i64 %3)
  %5 = tail call <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %5, i64 -1, <vscale x 2 x i1> %4, i64 %3)
  %7 = tail call <vscale x 2 x i64> @llvm.riscv.vslide1up.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 2 x i64> @llvm.riscv.vslide1down.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, i64 0, i64 %3)
  %9 = tail call <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64> poison, i64 %3)
  %10 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %7, <vscale x 2 x i64> %8, <vscale x 2 x i1> %11, i64 %3)
  %13 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv2i64(<vscale x 2 x i64> %12)
  %14 = tail call <vscale x 2 x i64> @llvm.riscv.vor.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, <vscale x 2 x i64> %12, i64 %3)
  %15 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.i64.i64(<vscale x 2 x i64> %14, i64 0, i64 %3)
  ret <vscale x 2 x i1> %15
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i1> @_Z10Ne128UpperIu14__rvv_bool32_tu16__rvv_uint64m2_tET_T0_S1_(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %4 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1, i64 %3)
  %5 = tail call <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %5, i64 -1, <vscale x 2 x i1> %4, i64 %3)
  %7 = tail call <vscale x 2 x i64> @llvm.riscv.vslide1down.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, i64 0, i64 %3)
  %8 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv2i64(<vscale x 2 x i64> %7)
  %9 = tail call <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64> poison, i64 %3)
  %10 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %6, <vscale x 2 x i64> %7, <vscale x 2 x i1> %11, i64 %3)
  %13 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.i64.i64(<vscale x 2 x i64> %12, i64 0, i64 %3)
  ret <vscale x 2 x i1> %13
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i1> @_Z5Lt128Iu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %4 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1, i64 %3)
  %5 = tail call <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %5, i64 -1, <vscale x 4 x i1> %4, i64 %3)
  %7 = tail call <vscale x 4 x i1> @llvm.riscv.vmsltu.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1, i64 %3)
  %8 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %5, i64 -1, <vscale x 4 x i1> %7, i64 %3)
  %9 = tail call <vscale x 4 x i64> @llvm.riscv.vslide1up.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %8, i64 0, i64 %3)
  %10 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, <vscale x 4 x i64> %9, i64 %3)
  %11 = tail call <vscale x 4 x i64> @llvm.riscv.vor.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %8, <vscale x 4 x i64> %10, i64 %3)
  %12 = tail call <vscale x 4 x i64> @llvm.riscv.vslide1down.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %11, i64 0, i64 %3)
  %13 = tail call <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64> poison, i64 %3)
  %14 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %13, i64 1, i64 %3)
  %15 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64> %14, i64 0, i64 %3)
  %16 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %11, <vscale x 4 x i64> %12, <vscale x 4 x i1> %15, i64 %3)
  %17 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.i64.i64(<vscale x 4 x i64> %16, i64 0, i64 %3)
  ret <vscale x 4 x i1> %17
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i1> @_Z10Lt128UpperIu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %4 = tail call <vscale x 4 x i1> @llvm.riscv.vmsltu.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1, i64 %3)
  %5 = tail call <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %5, i64 -1, <vscale x 4 x i1> %4, i64 %3)
  %7 = tail call <vscale x 4 x i64> @llvm.riscv.vslide1down.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, i64 0, i64 %3)
  %8 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv4i64(<vscale x 4 x i64> %7)
  %9 = tail call <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64> poison, i64 %3)
  %10 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, <vscale x 4 x i64> %7, <vscale x 4 x i1> %11, i64 %3)
  %13 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.i64.i64(<vscale x 4 x i64> %12, i64 0, i64 %3)
  ret <vscale x 4 x i1> %13
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i1> @_Z5Eq128Iu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %4 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1, i64 %3)
  %5 = tail call <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %5, i64 -1, <vscale x 4 x i1> %4, i64 %3)
  %7 = tail call <vscale x 4 x i64> @llvm.riscv.vslide1up.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 4 x i64> @llvm.riscv.vslide1down.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, i64 0, i64 %3)
  %9 = tail call <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64> poison, i64 %3)
  %10 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %7, <vscale x 4 x i64> %8, <vscale x 4 x i1> %11, i64 %3)
  %13 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, <vscale x 4 x i64> %12, i64 %3)
  %14 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv4i64(<vscale x 4 x i64> %13)
  %15 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.i64.i64(<vscale x 4 x i64> %13, i64 0, i64 %3)
  ret <vscale x 4 x i1> %15
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i1> @_Z10Eq128UpperIu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %4 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1, i64 %3)
  %5 = tail call <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %5, i64 -1, <vscale x 4 x i1> %4, i64 %3)
  %7 = tail call <vscale x 4 x i64> @llvm.riscv.vslide1down.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64> poison, i64 %3)
  %9 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %8, i64 1, i64 %3)
  %10 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64> %9, i64 0, i64 %3)
  %11 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, <vscale x 4 x i64> %7, <vscale x 4 x i1> %10, i64 %3)
  %12 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.i64.i64(<vscale x 4 x i64> %11, i64 0, i64 %3)
  ret <vscale x 4 x i1> %12
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i1> @_Z5Ne128Iu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %4 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1, i64 %3)
  %5 = tail call <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %5, i64 -1, <vscale x 4 x i1> %4, i64 %3)
  %7 = tail call <vscale x 4 x i64> @llvm.riscv.vslide1up.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 4 x i64> @llvm.riscv.vslide1down.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, i64 0, i64 %3)
  %9 = tail call <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64> poison, i64 %3)
  %10 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %7, <vscale x 4 x i64> %8, <vscale x 4 x i1> %11, i64 %3)
  %13 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv4i64(<vscale x 4 x i64> %12)
  %14 = tail call <vscale x 4 x i64> @llvm.riscv.vor.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, <vscale x 4 x i64> %12, i64 %3)
  %15 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.i64.i64(<vscale x 4 x i64> %14, i64 0, i64 %3)
  ret <vscale x 4 x i1> %15
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i1> @_Z10Ne128UpperIu14__rvv_bool16_tu16__rvv_uint64m4_tET_T0_S1_(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %4 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1, i64 %3)
  %5 = tail call <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %5, i64 -1, <vscale x 4 x i1> %4, i64 %3)
  %7 = tail call <vscale x 4 x i64> @llvm.riscv.vslide1down.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, i64 0, i64 %3)
  %8 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv4i64(<vscale x 4 x i64> %7)
  %9 = tail call <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64> poison, i64 %3)
  %10 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %6, <vscale x 4 x i64> %7, <vscale x 4 x i1> %11, i64 %3)
  %13 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.i64.i64(<vscale x 4 x i64> %12, i64 0, i64 %3)
  ret <vscale x 4 x i1> %13
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i1> @_Z5Lt128Iu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %4 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1, i64 %3)
  %5 = tail call <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %5, i64 -1, <vscale x 8 x i1> %4, i64 %3)
  %7 = tail call <vscale x 8 x i1> @llvm.riscv.vmsltu.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1, i64 %3)
  %8 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %5, i64 -1, <vscale x 8 x i1> %7, i64 %3)
  %9 = tail call <vscale x 8 x i64> @llvm.riscv.vslide1up.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %8, i64 0, i64 %3)
  %10 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, <vscale x 8 x i64> %9, i64 %3)
  %11 = tail call <vscale x 8 x i64> @llvm.riscv.vor.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %8, <vscale x 8 x i64> %10, i64 %3)
  %12 = tail call <vscale x 8 x i64> @llvm.riscv.vslide1down.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %11, i64 0, i64 %3)
  %13 = tail call <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64> poison, i64 %3)
  %14 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %13, i64 1, i64 %3)
  %15 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64> %14, i64 0, i64 %3)
  %16 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %11, <vscale x 8 x i64> %12, <vscale x 8 x i1> %15, i64 %3)
  %17 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.i64.i64(<vscale x 8 x i64> %16, i64 0, i64 %3)
  ret <vscale x 8 x i1> %17
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i1> @_Z10Lt128UpperIu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %4 = tail call <vscale x 8 x i1> @llvm.riscv.vmsltu.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1, i64 %3)
  %5 = tail call <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %5, i64 -1, <vscale x 8 x i1> %4, i64 %3)
  %7 = tail call <vscale x 8 x i64> @llvm.riscv.vslide1down.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, i64 0, i64 %3)
  %8 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv8i64(<vscale x 8 x i64> %7)
  %9 = tail call <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64> poison, i64 %3)
  %10 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, <vscale x 8 x i64> %7, <vscale x 8 x i1> %11, i64 %3)
  %13 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.i64.i64(<vscale x 8 x i64> %12, i64 0, i64 %3)
  ret <vscale x 8 x i1> %13
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i1> @_Z5Eq128Iu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %4 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1, i64 %3)
  %5 = tail call <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %5, i64 -1, <vscale x 8 x i1> %4, i64 %3)
  %7 = tail call <vscale x 8 x i64> @llvm.riscv.vslide1up.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 8 x i64> @llvm.riscv.vslide1down.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, i64 0, i64 %3)
  %9 = tail call <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64> poison, i64 %3)
  %10 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %7, <vscale x 8 x i64> %8, <vscale x 8 x i1> %11, i64 %3)
  %13 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, <vscale x 8 x i64> %12, i64 %3)
  %14 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv8i64(<vscale x 8 x i64> %13)
  %15 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.i64.i64(<vscale x 8 x i64> %13, i64 0, i64 %3)
  ret <vscale x 8 x i1> %15
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i1> @_Z10Eq128UpperIu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %4 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1, i64 %3)
  %5 = tail call <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %5, i64 -1, <vscale x 8 x i1> %4, i64 %3)
  %7 = tail call <vscale x 8 x i64> @llvm.riscv.vslide1down.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64> poison, i64 %3)
  %9 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %8, i64 1, i64 %3)
  %10 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64> %9, i64 0, i64 %3)
  %11 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, <vscale x 8 x i64> %7, <vscale x 8 x i1> %10, i64 %3)
  %12 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.i64.i64(<vscale x 8 x i64> %11, i64 0, i64 %3)
  ret <vscale x 8 x i1> %12
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i1> @_Z5Ne128Iu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %4 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1, i64 %3)
  %5 = tail call <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %5, i64 -1, <vscale x 8 x i1> %4, i64 %3)
  %7 = tail call <vscale x 8 x i64> @llvm.riscv.vslide1up.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 8 x i64> @llvm.riscv.vslide1down.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, i64 0, i64 %3)
  %9 = tail call <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64> poison, i64 %3)
  %10 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %7, <vscale x 8 x i64> %8, <vscale x 8 x i1> %11, i64 %3)
  %13 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv8i64(<vscale x 8 x i64> %12)
  %14 = tail call <vscale x 8 x i64> @llvm.riscv.vor.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, <vscale x 8 x i64> %12, i64 %3)
  %15 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.i64.i64(<vscale x 8 x i64> %14, i64 0, i64 %3)
  ret <vscale x 8 x i1> %15
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i1> @_Z10Ne128UpperIu13__rvv_bool8_tu16__rvv_uint64m8_tET_T0_S1_(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %4 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1, i64 %3)
  %5 = tail call <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64> poison, i64 0, i64 %3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %5, i64 -1, <vscale x 8 x i1> %4, i64 %3)
  %7 = tail call <vscale x 8 x i64> @llvm.riscv.vslide1down.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, i64 0, i64 %3)
  %8 = tail call noundef i64 @llvm.riscv.vmv.x.s.nxv8i64(<vscale x 8 x i64> %7)
  %9 = tail call <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64> poison, i64 %3)
  %10 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %9, i64 1, i64 %3)
  %11 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64> %10, i64 0, i64 %3)
  %12 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %6, <vscale x 8 x i64> %7, <vscale x 8 x i1> %11, i64 %3)
  %13 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.i64.i64(<vscale x 8 x i64> %12, i64 0, i64 %3)
  ret <vscale x 8 x i1> %13
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare i64 @llvm.riscv.vsetvlimax.i64(i64 immarg, i64 immarg) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vslide1up.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.nxv2i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmsltu.nxv2i64.nxv2i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vslide1up.nxv2i64.i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vslide1down.nxv2i64.i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.nxv4i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i1> @llvm.riscv.vmsltu.nxv4i64.nxv4i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vslide1up.nxv4i64.i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vslide1down.nxv4i64.i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.nxv8i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i1> @llvm.riscv.vmsltu.nxv8i64.nxv8i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vslide1up.nxv8i64.i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vslide1down.nxv8i64.i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, <vscale x 1 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vor.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.i64.i64(<vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare i64 @llvm.riscv.vmv.x.s.nxv1i64(<vscale x 1 x i64>) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, i64, <vscale x 2 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vor.nxv2i64.nxv2i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, <vscale x 2 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.nxv2i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, <vscale x 2 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, <vscale x 2 x i64>, <vscale x 2 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.i64.i64(<vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare i64 @llvm.riscv.vmv.x.s.nxv2i64(<vscale x 2 x i64>) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i64.nxv2i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, i64, <vscale x 4 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vor.nxv4i64.nxv4i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, <vscale x 4 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.nxv4i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, <vscale x 4 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, <vscale x 4 x i64>, <vscale x 4 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.i64.i64(<vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare i64 @llvm.riscv.vmv.x.s.nxv4i64(<vscale x 4 x i64>) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i64.nxv4i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, i64, <vscale x 8 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vor.nxv8i64.nxv8i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, <vscale x 8 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.nxv8i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, <vscale x 8 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, <vscale x 8 x i64>, <vscale x 8 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.i64.i64(<vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare i64 @llvm.riscv.vmv.x.s.nxv8i64(<vscale x 8 x i64>) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i64.nxv8i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, i64) #1

attributes #0 = { mustprogress uwtable vscale_range(2,1024) "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="sifive-x280" "target-features"="+64bit,+a,+c,+d,+f,+m,+relax,+v,+zicsr,+zifencei,+zmmul,+zve32f,+zve32x,+zve64d,+zve64f,+zve64x,+zvl128b,+zvl32b,+zvl64b,-b,-e,-experimental-smmpm,-experimental-smnpm,-experimental-ssnpm,-experimental-sspm,-experimental-ssqosid,-experimental-supm,-experimental-zacas,-experimental-zalasr,-experimental-zicfilp,-experimental-zicfiss,-h,-shcounterenw,-shgatpa,-shtvala,-shvsatpa,-shvstvala,-shvstvecd,-smaia,-smcdeleg,-smcsrind,-smepmp,-smstateen,-ssaia,-ssccfg,-ssccptr,-sscofpmf,-sscounterenw,-sscsrind,-ssstateen,-ssstrict,-sstc,-sstvala,-sstvecd,-ssu64xl,-svade,-svadu,-svbare,-svinval,-svnapot,-svpbmt,-xcvalu,-xcvbi,-xcvbitmanip,-xcvelw,-xcvmac,-xcvmem,-xcvsimd,-xsfcease,-xsfvcp,-xsfvfnrclipxfqf,-xsfvfwmaccqqq,-xsfvqmaccdod,-xsfvqmaccqoq,-xsifivecdiscarddlone,-xsifivecflushdlone,-xtheadba,-xtheadbb,-xtheadbs,-xtheadcmo,-xtheadcondmov,-xtheadfmemidx,-xtheadmac,-xtheadmemidx,-xtheadmempair,-xtheadsync,-xtheadvdot,-xventanacondops,-xwchc,-za128rs,-za64rs,-zaamo,-zabha,-zalrsc,-zama16b,-zawrs,-zba,-zbb,-zbc,-zbkb,-zbkc,-zbkx,-zbs,-zca,-zcb,-zcd,-zce,-zcf,-zcmop,-zcmp,-zcmt,-zdinx,-zfa,-zfbfmin,-zfh,-zfhmin,-zfinx,-zhinx,-zhinxmin,-zic64b,-zicbom,-zicbop,-zicboz,-ziccamoa,-ziccif,-zicclsm,-ziccrse,-zicntr,-zicond,-zihintntl,-zihintpause,-zihpm,-zimop,-zk,-zkn,-zknd,-zkne,-zknh,-zkr,-zks,-zksed,-zksh,-zkt,-ztso,-zvbb,-zvbc,-zvfbfmin,-zvfbfwma,-zvfh,-zvfhmin,-zvkb,-zvkg,-zvkn,-zvknc,-zvkned,-zvkng,-zvknha,-zvknhb,-zvks,-zvksc,-zvksed,-zvksg,-zvksh,-zvkt,-zvl1024b,-zvl16384b,-zvl2048b,-zvl256b,-zvl32768b,-zvl4096b,-zvl512b,-zvl65536b,-zvl8192b" }
attributes #1 = { mustprogress nocallback nofree nosync nounwind willreturn  }
attributes #2 = { nounwind }

!llvm.module.flags = !{!0, !1, !2, !4, !5, !6, !7}
!llvm.ident = !{!8}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"target-abi", !"lp64d"}
!2 = !{i32 6, !"riscv-isa", !3}
!3 = !{!"rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_v1p0_zicsr2p0_zifencei2p0_zmmul1p0_zve32f1p0_zve32x1p0_zve64d1p0_zve64f1p0_zve64x1p0_zvl128b1p0_zvl32b1p0_zvl64b1p0"}
!4 = !{i32 8, !"PIC Level", i32 2}
!5 = !{i32 7, !"PIE Level", i32 2}
!6 = !{i32 7, !"uwtable", i32 2}
!7 = !{i32 8, !"SmallDataLimit", i32 8}
!8 = !{!"clang version 19.1.0-rc2"}
!9 = !{i64 2011422}
!10 = !{i64 2011956}
!11 = !{i64 2012754}
!12 = !{i64 2013226}
