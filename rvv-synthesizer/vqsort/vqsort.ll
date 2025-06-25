; ModuleID = 'vqsort.cpp'
source_filename = "vqsort.cpp"
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64-unknown-linux-gnu"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @PrevValue_Ascending_128(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 0, i64 %2)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %2)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %2)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %2)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %6, i64 0, i64 %2)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %3, <vscale x 1 x i64> %4, <vscale x 1 x i1> %7, i64 %2)
  %9 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %3, i64 %2)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %3, <vscale x 1 x i64> %8, <vscale x 1 x i1> %9, i64 %2)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %10, <vscale x 1 x i64> %10, i64 1, i64 %2, i64 3)
  %12 = tail call <vscale x 1 x i1> @llvm.riscv.vmslt.nxv1i64.i64.i64(<vscale x 1 x i64> %6, i64 1, i64 %2)
  %13 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %11, i64 0, <vscale x 1 x i1> %12, i64 %2)
  %14 = tail call <vscale x 1 x i64> @llvm.riscv.vsub.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %8, i64 %2)
  %15 = tail call <vscale x 1 x i64> @llvm.riscv.vsub.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %14, <vscale x 1 x i64> %13, i64 %2)
  ret <vscale x 1 x i64> %15
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @PrevValue_Descending_128(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 0, i64 %2)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %2)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %2)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %2)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %6, i64 0, i64 %2)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %3, <vscale x 1 x i64> %4, <vscale x 1 x i1> %7, i64 %2)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vadd.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %8, i64 %2)
  %10 = tail call <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %9, <vscale x 1 x i64> %0, i64 %2)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %3, <vscale x 1 x i64> %8, <vscale x 1 x i1> %10, i64 %2)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %11, <vscale x 1 x i64> %11, i64 1, i64 %2, i64 3)
  %13 = tail call <vscale x 1 x i1> @llvm.riscv.vmslt.nxv1i64.i64.i64(<vscale x 1 x i64> %6, i64 1, i64 %2)
  %14 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %12, i64 0, <vscale x 1 x i1> %13, i64 %2)
  %15 = tail call <vscale x 1 x i64> @llvm.riscv.vadd.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, <vscale x 1 x i64> %14, i64 %2)
  ret <vscale x 1 x i64> %15
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @PrevValue_Ascending_64(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %2)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vsub.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %3, i64 %2)
  ret <vscale x 1 x i64> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @PrevValue_Descending_64(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %2)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vadd.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %3, i64 %2)
  ret <vscale x 1 x i64> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @PrevValue_Ascending_32(<vscale x 2 x i32> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %3 = tail call <vscale x 2 x i32> @llvm.riscv.vmv.v.x.nxv2i32.i64(<vscale x 2 x i32> poison, i32 1, i64 %2)
  %4 = tail call <vscale x 2 x i32> @llvm.riscv.vsub.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %3, i64 %2)
  ret <vscale x 2 x i32> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @PrevValue_Descending_32(<vscale x 2 x i32> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %3 = tail call <vscale x 2 x i32> @llvm.riscv.vmv.v.x.nxv2i32.i64(<vscale x 2 x i32> poison, i32 1, i64 %2)
  %4 = tail call <vscale x 2 x i32> @llvm.riscv.vadd.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %3, i64 %2)
  ret <vscale x 2 x i32> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @SwapAdjacentPairs_64(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 2, i64 %2, i64 3)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %0, i64 2, i64 %2, i64 3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %2)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vsrl.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %2)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 1, i64 %2)
  %8 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %7, i64 0, i64 %2)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %4, <vscale x 1 x i64> %3, <vscale x 1 x i1> %8, i64 %2)
  ret <vscale x 1 x i64> %9
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @SwapAdjacentQuads_64(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = lshr i64 %2, 1
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 %3, i64 %2, i64 3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %0, i64 %3, i64 %2, i64 3)
  ret <vscale x 1 x i64> %5
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @SwapAdjacentPairs_32(<vscale x 2 x i32> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %3 = tail call <vscale x 2 x i32> @llvm.riscv.vslideup.nxv2i32.i64(<vscale x 2 x i32> %0, <vscale x 2 x i32> %0, i64 2, i64 %2, i64 3)
  %4 = tail call <vscale x 2 x i32> @llvm.riscv.vslidedown.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, i64 2, i64 %2, i64 3)
  %5 = tail call <vscale x 2 x i32> @llvm.riscv.vid.nxv2i32.i64(<vscale x 2 x i32> poison, i64 %2)
  %6 = tail call <vscale x 2 x i32> @llvm.riscv.vand.nxv2i32.i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %5, i32 3, i64 %2)
  %7 = tail call <vscale x 2 x i1> @llvm.riscv.vmslt.nxv2i32.i32.i64(<vscale x 2 x i32> %6, i32 2, i64 %2)
  %8 = tail call <vscale x 2 x i32> @llvm.riscv.vmerge.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %3, <vscale x 2 x i32> %4, <vscale x 2 x i1> %7, i64 %2)
  ret <vscale x 2 x i32> %8
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @SwapAdjacentQuads_32(<vscale x 2 x i32> %0) local_unnamed_addr #0 {
  %2 = bitcast <vscale x 2 x i32> %0 to <vscale x 1 x i64>
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %2, i64 2, i64 %3, i64 3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %2, <vscale x 1 x i64> %2, i64 2, i64 %3, i64 3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vsrl.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 1, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %7, i64 1, i64 %3)
  %9 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %8, i64 0, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, <vscale x 1 x i64> %4, <vscale x 1 x i1> %9, i64 %3)
  %11 = bitcast <vscale x 1 x i64> %10 to <vscale x 2 x i32>
  ret <vscale x 2 x i32> %11
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @Sort2_Ascending_128_a(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %8 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, <vscale x 1 x i64> %0, <vscale x 1 x i1> %7, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, <vscale x 1 x i64> %6, <vscale x 1 x i1> %8, i64 %3)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %11, i64 1, i64 %3)
  %13 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %12, i64 0, i64 %3)
  %14 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, <vscale x 1 x i64> %10, <vscale x 1 x i1> %13, i64 %3)
  ret <vscale x 1 x i64> %14
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @Sort2_Ascending_128_b(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %8, i64 1, i64 %3)
  %10 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %9, i64 0, i64 %3)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, <vscale x 1 x i1> %6, i64 %3)
  %13 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %12, <vscale x 1 x i64> %11, <vscale x 1 x i1> %7, i64 %3)
  %14 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %11, <vscale x 1 x i64> %13, <vscale x 1 x i1> %10, i64 %3)
  ret <vscale x 1 x i64> %14
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @Sort2_Descending_128_a(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %8 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, <vscale x 1 x i1> %7, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, <vscale x 1 x i64> %6, <vscale x 1 x i1> %8, i64 %3)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %11, i64 1, i64 %3)
  %13 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %12, i64 0, i64 %3)
  %14 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, <vscale x 1 x i64> %10, <vscale x 1 x i1> %13, i64 %3)
  ret <vscale x 1 x i64> %14
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @Sort2_Descending_128_b(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, i64 0, i64 %3)
  %6 = tail call <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %8, i64 1, i64 %3)
  %10 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %9, i64 0, i64 %3)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, <vscale x 1 x i64> %0, <vscale x 1 x i1> %6, i64 %3)
  %13 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %12, <vscale x 1 x i64> %11, <vscale x 1 x i1> %7, i64 %3)
  %14 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %11, <vscale x 1 x i64> %13, <vscale x 1 x i1> %10, i64 %3)
  ret <vscale x 1 x i64> %14
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @Sort2_Ascending_64_a(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  ret <vscale x 1 x i64> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @Sort2_Ascending_64_b(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  ret <vscale x 1 x i64> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @Sort2_Descending_64_a(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  ret <vscale x 1 x i64> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @Sort2_Descending_64_b(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %3)
  ret <vscale x 1 x i64> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @Sort2_Ascending_32_a(<vscale x 2 x i32> %0, <vscale x 2 x i32> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %4 = tail call <vscale x 2 x i32> @llvm.riscv.vminu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %1, i64 %3)
  ret <vscale x 2 x i32> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @Sort2_Ascending_32_b(<vscale x 2 x i32> %0, <vscale x 2 x i32> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %4 = tail call <vscale x 2 x i32> @llvm.riscv.vmaxu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %1, i64 %3)
  ret <vscale x 2 x i32> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @Sort2_Descending_32_a(<vscale x 2 x i32> %0, <vscale x 2 x i32> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %4 = tail call <vscale x 2 x i32> @llvm.riscv.vmaxu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %1, i64 %3)
  ret <vscale x 2 x i32> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @Sort2_Descending_32_b(<vscale x 2 x i32> %0, <vscale x 2 x i32> %1) local_unnamed_addr #0 {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %4 = tail call <vscale x 2 x i32> @llvm.riscv.vminu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %1, i64 %3)
  ret <vscale x 2 x i32> %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @SortPairsDistance1_Ascending_128(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 2, i64 %2, i64 3)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %0, i64 2, i64 %2, i64 3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %2)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vsrl.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %2)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 1, i64 %2)
  %8 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %7, i64 0, i64 %2)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %4, <vscale x 1 x i64> %3, <vscale x 1 x i1> %8, i64 %2)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %2)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, i64 0, i64 %2)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %9, i64 %2)
  %13 = tail call <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %10, <vscale x 1 x i64> %11, i64 %2)
  %14 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %10, <vscale x 1 x i64> %11, i64 %2)
  %15 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, <vscale x 1 x i64> %0, <vscale x 1 x i1> %13, i64 %2)
  %16 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %15, <vscale x 1 x i64> %12, <vscale x 1 x i1> %14, i64 %2)
  %17 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %2)
  %18 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %17, i64 0, i64 %2)
  %19 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %12, <vscale x 1 x i64> %16, <vscale x 1 x i1> %18, i64 %2)
  %20 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %9, i64 %2)
  %21 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %9, <vscale x 1 x i1> %13, i64 %2)
  %22 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %21, <vscale x 1 x i64> %20, <vscale x 1 x i1> %14, i64 %2)
  %23 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %20, <vscale x 1 x i64> %22, <vscale x 1 x i1> %18, i64 %2)
  %24 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %23, <vscale x 1 x i64> %19, <vscale x 1 x i1> %8, i64 %2)
  ret <vscale x 1 x i64> %24
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @SortPairsDistance1_Ascending_64(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1up.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %2)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %2)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %2)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %2)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %6, i64 0, i64 %2)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %3, <vscale x 1 x i64> %4, <vscale x 1 x i1> %7, i64 %2)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %8, i64 %2)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %8, i64 %2)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %10, <vscale x 1 x i64> %9, <vscale x 1 x i1> %7, i64 %2)
  ret <vscale x 1 x i64> %11
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @SortPairsDistance1_Ascending_32(<vscale x 2 x i32> %0) local_unnamed_addr #0 {
  %2 = bitcast <vscale x 2 x i32> %0 to <vscale x 1 x i64>
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vsrl.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %2, i64 32, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vsll.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %2, i64 32, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vor.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %7 = bitcast <vscale x 1 x i64> %6 to <vscale x 2 x i32>
  %8 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %9 = tail call <vscale x 2 x i32> @llvm.riscv.vminu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %7, i64 %8)
  %10 = tail call <vscale x 2 x i32> @llvm.riscv.vmaxu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %7, i64 %8)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %3)
  %12 = bitcast <vscale x 1 x i64> %11 to <vscale x 2 x i32>
  %13 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i32.i32.i64(<vscale x 2 x i32> %12, i32 0, i64 %8)
  %14 = tail call <vscale x 2 x i32> @llvm.riscv.vmerge.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %10, <vscale x 2 x i32> %9, <vscale x 2 x i1> %13, i64 %8)
  ret <vscale x 2 x i32> %14
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @SortPairsDistance1_Descending_128(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 2, i64 %2, i64 3)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %0, i64 2, i64 %2, i64 3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %2)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vsrl.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %2)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 1, i64 %2)
  %8 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %7, i64 0, i64 %2)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %4, <vscale x 1 x i64> %3, <vscale x 1 x i1> %8, i64 %2)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %2)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, i64 0, i64 %2)
  %12 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %9, i64 %2)
  %13 = tail call <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %10, <vscale x 1 x i64> %11, i64 %2)
  %14 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> %10, <vscale x 1 x i64> %11, i64 %2)
  %15 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %9, <vscale x 1 x i1> %13, i64 %2)
  %16 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %15, <vscale x 1 x i64> %12, <vscale x 1 x i1> %14, i64 %2)
  %17 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %2)
  %18 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %17, i64 0, i64 %2)
  %19 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %12, <vscale x 1 x i64> %16, <vscale x 1 x i1> %18, i64 %2)
  %20 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %9, i64 %2)
  %21 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %9, <vscale x 1 x i64> %0, <vscale x 1 x i1> %13, i64 %2)
  %22 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %21, <vscale x 1 x i64> %20, <vscale x 1 x i1> %14, i64 %2)
  %23 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %20, <vscale x 1 x i64> %22, <vscale x 1 x i1> %18, i64 %2)
  %24 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %23, <vscale x 1 x i64> %19, <vscale x 1 x i1> %8, i64 %2)
  ret <vscale x 1 x i64> %24
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @SortPairsDistance1_Descending_64(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1up.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %2)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 0, i64 %2)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %2)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %2)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %6, i64 0, i64 %2)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %3, <vscale x 1 x i64> %4, <vscale x 1 x i1> %7, i64 %2)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %8, i64 %2)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %8, i64 %2)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %10, <vscale x 1 x i64> %9, <vscale x 1 x i1> %7, i64 %2)
  ret <vscale x 1 x i64> %11
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @SortPairsDistance1_Descending_32(<vscale x 2 x i32> %0) local_unnamed_addr #0 {
  %2 = bitcast <vscale x 2 x i32> %0 to <vscale x 1 x i64>
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vsrl.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %2, i64 32, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vsll.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %2, i64 32, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vor.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %4, <vscale x 1 x i64> %5, i64 %3)
  %7 = bitcast <vscale x 1 x i64> %6 to <vscale x 2 x i32>
  %8 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %9 = tail call <vscale x 2 x i32> @llvm.riscv.vmaxu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %7, i64 %8)
  %10 = tail call <vscale x 2 x i32> @llvm.riscv.vminu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %7, i64 %8)
  %11 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %3)
  %12 = bitcast <vscale x 1 x i64> %11 to <vscale x 2 x i32>
  %13 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i32.i32.i64(<vscale x 2 x i32> %12, i32 0, i64 %8)
  %14 = tail call <vscale x 2 x i32> @llvm.riscv.vmerge.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %10, <vscale x 2 x i32> %9, <vscale x 2 x i1> %13, i64 %8)
  ret <vscale x 2 x i32> %14
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @SortPairsDistance4_Ascending_64(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = lshr i64 %2, 1
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 %3, i64 %2, i64 3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %0, i64 %3, i64 %2, i64 3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %5, i64 %2)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %5, i64 %2)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %7, i64 %3, i64 %2, i64 3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %6, <vscale x 1 x i64> %8, i64 %3, i64 %2, i64 3)
  ret <vscale x 1 x i64> %9
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @SortPairsDistance4_Ascending_32(<vscale x 2 x i32> %0) local_unnamed_addr #0 {
  %2 = bitcast <vscale x 2 x i32> %0 to <vscale x 1 x i64>
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %2, i64 2, i64 %3, i64 3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %2, <vscale x 1 x i64> %2, i64 2, i64 %3, i64 3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vsrl.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 1, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %7, i64 1, i64 %3)
  %9 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %8, i64 0, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, <vscale x 1 x i64> %4, <vscale x 1 x i1> %9, i64 %3)
  %11 = bitcast <vscale x 1 x i64> %10 to <vscale x 2 x i32>
  %12 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %13 = tail call <vscale x 2 x i32> @llvm.riscv.vminu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %11, i64 %12)
  %14 = tail call <vscale x 2 x i32> @llvm.riscv.vmaxu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %11, i64 %12)
  %15 = bitcast <vscale x 2 x i32> %14 to <vscale x 1 x i64>
  %16 = bitcast <vscale x 2 x i32> %13 to <vscale x 1 x i64>
  %17 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %15, <vscale x 1 x i64> %16, <vscale x 1 x i1> %9, i64 %3)
  %18 = bitcast <vscale x 1 x i64> %17 to <vscale x 2 x i32>
  ret <vscale x 2 x i32> %18
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 1 x i64> @SortPairsDistance4_Descending_64(<vscale x 1 x i64> %0) local_unnamed_addr #0 {
  %2 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %3 = lshr i64 %2, 1
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, i64 %3, i64 %2, i64 3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %4, <vscale x 1 x i64> %0, i64 %3, i64 %2, i64 3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %5, i64 %2)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %5, i64 %2)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %7, i64 %3, i64 %2, i64 3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %6, <vscale x 1 x i64> %8, i64 %3, i64 %2, i64 3)
  ret <vscale x 1 x i64> %9
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024)
define dso_local <vscale x 2 x i32> @SortPairsDistance4_Descending_32(<vscale x 2 x i32> %0) local_unnamed_addr #0 {
  %2 = bitcast <vscale x 2 x i32> %0 to <vscale x 1 x i64>
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %2, i64 2, i64 %3, i64 3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64> %2, <vscale x 1 x i64> %2, i64 2, i64 %3, i64 3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %7 = tail call <vscale x 1 x i64> @llvm.riscv.vsrl.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %6, i64 1, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %7, i64 1, i64 %3)
  %9 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %8, i64 0, i64 %3)
  %10 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, <vscale x 1 x i64> %4, <vscale x 1 x i1> %9, i64 %3)
  %11 = bitcast <vscale x 1 x i64> %10 to <vscale x 2 x i32>
  %12 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %13 = tail call <vscale x 2 x i32> @llvm.riscv.vmaxu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %11, i64 %12)
  %14 = tail call <vscale x 2 x i32> @llvm.riscv.vminu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %11, i64 %12)
  %15 = bitcast <vscale x 2 x i32> %14 to <vscale x 1 x i64>
  %16 = bitcast <vscale x 2 x i32> %13 to <vscale x 1 x i64>
  %17 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %15, <vscale x 1 x i64> %16, <vscale x 1 x i1> %9, i64 %3)
  %18 = bitcast <vscale x 1 x i64> %17 to <vscale x 2 x i32>
  ret <vscale x 2 x i32> %18
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare i64 @llvm.riscv.vsetvlimax.i64(i64 immarg, i64 immarg) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vslideup.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64, i64 immarg) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmslt.nxv1i64.i64.i64(<vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, <vscale x 1 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vsub.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vadd.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmsltu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vsub.nxv2i32.nxv2i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, <vscale x 2 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vmv.v.x.nxv2i32.i64(<vscale x 2 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vadd.nxv2i32.nxv2i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, <vscale x 2 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vslidedown.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64, i64 immarg) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vsrl.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vslideup.nxv2i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, i64, i64, i64 immarg) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vslidedown.nxv2i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, i64, i64, i64 immarg) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vand.nxv2i32.i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vid.nxv2i32.i64(<vscale x 2 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmslt.nxv2i32.i32.i64(<vscale x 2 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vmerge.nxv2i32.nxv2i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, <vscale x 2 x i32>, <vscale x 2 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vslide1down.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vminu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vmaxu.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vminu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, <vscale x 2 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vmaxu.nxv2i32.nxv2i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, <vscale x 2 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vslide1up.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vor.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vsll.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i32.i32.i64(<vscale x 2 x i32>, i32, i64) #1

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn  uwtable vscale_range(2,1024) "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="sifive-x280" "target-features"="+64bit,+a,+c,+d,+f,+m,+relax,+v,+zicsr,+zifencei,+zmmul,+zve32f,+zve32x,+zve64d,+zve64f,+zve64x,+zvl128b,+zvl32b,+zvl64b,-b,-e,-experimental-smmpm,-experimental-smnpm,-experimental-ssnpm,-experimental-sspm,-experimental-ssqosid,-experimental-supm,-experimental-zacas,-experimental-zalasr,-experimental-zicfilp,-experimental-zicfiss,-h,-shcounterenw,-shgatpa,-shtvala,-shvsatpa,-shvstvala,-shvstvecd,-smaia,-smcdeleg,-smcsrind,-smepmp,-smstateen,-ssaia,-ssccfg,-ssccptr,-sscofpmf,-sscounterenw,-sscsrind,-ssstateen,-ssstrict,-sstc,-sstvala,-sstvecd,-ssu64xl,-svade,-svadu,-svbare,-svinval,-svnapot,-svpbmt,-xcvalu,-xcvbi,-xcvbitmanip,-xcvelw,-xcvmac,-xcvmem,-xcvsimd,-xsfcease,-xsfvcp,-xsfvfnrclipxfqf,-xsfvfwmaccqqq,-xsfvqmaccdod,-xsfvqmaccqoq,-xsifivecdiscarddlone,-xsifivecflushdlone,-xtheadba,-xtheadbb,-xtheadbs,-xtheadcmo,-xtheadcondmov,-xtheadfmemidx,-xtheadmac,-xtheadmemidx,-xtheadmempair,-xtheadsync,-xtheadvdot,-xventanacondops,-xwchc,-za128rs,-za64rs,-zaamo,-zabha,-zalrsc,-zama16b,-zawrs,-zba,-zbb,-zbc,-zbkb,-zbkc,-zbkx,-zbs,-zca,-zcb,-zcd,-zce,-zcf,-zcmop,-zcmp,-zcmt,-zdinx,-zfa,-zfbfmin,-zfh,-zfhmin,-zfinx,-zhinx,-zhinxmin,-zic64b,-zicbom,-zicbop,-zicboz,-ziccamoa,-ziccif,-zicclsm,-ziccrse,-zicntr,-zicond,-zihintntl,-zihintpause,-zihpm,-zimop,-zk,-zkn,-zknd,-zkne,-zknh,-zkr,-zks,-zksed,-zksh,-zkt,-ztso,-zvbb,-zvbc,-zvfbfmin,-zvfbfwma,-zvfh,-zvfhmin,-zvkb,-zvkg,-zvkn,-zvknc,-zvkned,-zvkng,-zvknha,-zvknhb,-zvks,-zvksc,-zvksed,-zvksg,-zvksh,-zvkt,-zvl1024b,-zvl16384b,-zvl2048b,-zvl256b,-zvl32768b,-zvl4096b,-zvl512b,-zvl65536b,-zvl8192b" }
attributes #1 = { mustprogress nocallback nofree nosync nounwind willreturn  }

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
!8 = !{!"clang version 19.1.7"}
