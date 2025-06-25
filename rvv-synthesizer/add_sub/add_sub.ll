; ModuleID = 'add_sub.cpp'
source_filename = "add_sub.cpp"
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64-unknown-linux-gnu"

$_Z6AddSubIu16__rvv_uint8mf8_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint8mf4_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint8mf2_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_uint8m1_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_uint8m2_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_uint8m4_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_uint8m8_tET_S0_S0_ = comdat any

$_Z6AddSubIu17__rvv_uint16mf4_tET_S0_S0_ = comdat any

$_Z6AddSubIu17__rvv_uint16mf2_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint16m1_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint16m2_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint16m4_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint16m8_tET_S0_S0_ = comdat any

$_Z6AddSubIu17__rvv_uint32mf2_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint32m1_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint32m2_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint32m4_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint32m8_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint64m1_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint64m2_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint64m4_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_uint64m8_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int8mf8_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int8mf4_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int8mf2_tET_S0_S0_ = comdat any

$_Z6AddSubIu14__rvv_int8m1_tET_S0_S0_ = comdat any

$_Z6AddSubIu14__rvv_int8m2_tET_S0_S0_ = comdat any

$_Z6AddSubIu14__rvv_int8m4_tET_S0_S0_ = comdat any

$_Z6AddSubIu14__rvv_int8m8_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_int16mf4_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_int16mf2_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int16m1_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int16m2_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int16m4_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int16m8_tET_S0_S0_ = comdat any

$_Z6AddSubIu16__rvv_int32mf2_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int32m1_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int32m2_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int32m4_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int32m8_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int64m1_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int64m2_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int64m4_tET_S0_S0_ = comdat any

$_Z6AddSubIu15__rvv_int64m8_tET_S0_S0_ = comdat any

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i8> @_Z6AddSubIu16__rvv_uint8mf8_tET_S0_S0_(<vscale x 1 x i8> %0, <vscale x 1 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 5)
  %4 = tail call <vscale x 1 x i8> @llvm.riscv.vrsub.nxv1i8.i8.i64(<vscale x 1 x i8> poison, <vscale x 1 x i8> %1, i8 0, i64 %3)
  %5 = tail call i64 @llvm.read_register.i64(metadata !9)
  %6 = lshr i64 %5, 4
  %7 = tail call noundef i64 @llvm.umin.i64(i64 %6, i64 512)
  %8 = tail call <vscale x 1 x i16> @llvm.riscv.vmv.v.x.nxv1i16.i64(<vscale x 1 x i16> poison, i16 1, i64 %7)
  %9 = bitcast <vscale x 1 x i16> %8 to <vscale x 2 x i8>
  %10 = tail call <vscale x 1 x i8> @llvm.vector.extract.nxv1i8.nxv2i8(<vscale x 2 x i8> %9, i64 0)
  %11 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i8.i8.i64(<vscale x 1 x i8> %10, i8 0, i64 %3)
  %12 = tail call <vscale x 1 x i8> @llvm.riscv.vmerge.nxv1i8.nxv1i8.i64(<vscale x 1 x i8> poison, <vscale x 1 x i8> %1, <vscale x 1 x i8> %4, <vscale x 1 x i1> %11, i64 %3)
  %13 = tail call <vscale x 1 x i8> @llvm.riscv.vadd.nxv1i8.nxv1i8.i64(<vscale x 1 x i8> poison, <vscale x 1 x i8> %0, <vscale x 1 x i8> %12, i64 %3)
  ret <vscale x 1 x i8> %13
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i8> @_Z6AddSubIu16__rvv_uint8mf4_tET_S0_S0_(<vscale x 2 x i8> %0, <vscale x 2 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 6)
  %4 = tail call <vscale x 2 x i8> @llvm.riscv.vrsub.nxv2i8.i8.i64(<vscale x 2 x i8> poison, <vscale x 2 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 6)
  %6 = tail call <vscale x 1 x i16> @llvm.riscv.vmv.v.x.nxv1i16.i64(<vscale x 1 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 1 x i16> %6 to <vscale x 2 x i8>
  %8 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i8.i8.i64(<vscale x 2 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 2 x i8> @llvm.riscv.vmerge.nxv2i8.nxv2i8.i64(<vscale x 2 x i8> poison, <vscale x 2 x i8> %1, <vscale x 2 x i8> %4, <vscale x 2 x i1> %8, i64 %3)
  %10 = tail call <vscale x 2 x i8> @llvm.riscv.vadd.nxv2i8.nxv2i8.i64(<vscale x 2 x i8> poison, <vscale x 2 x i8> %0, <vscale x 2 x i8> %9, i64 %3)
  ret <vscale x 2 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i8> @_Z6AddSubIu16__rvv_uint8mf2_tET_S0_S0_(<vscale x 4 x i8> %0, <vscale x 4 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 7)
  %4 = tail call <vscale x 4 x i8> @llvm.riscv.vrsub.nxv4i8.i8.i64(<vscale x 4 x i8> poison, <vscale x 4 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 7)
  %6 = tail call <vscale x 2 x i16> @llvm.riscv.vmv.v.x.nxv2i16.i64(<vscale x 2 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 2 x i16> %6 to <vscale x 4 x i8>
  %8 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i8.i8.i64(<vscale x 4 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 4 x i8> @llvm.riscv.vmerge.nxv4i8.nxv4i8.i64(<vscale x 4 x i8> poison, <vscale x 4 x i8> %1, <vscale x 4 x i8> %4, <vscale x 4 x i1> %8, i64 %3)
  %10 = tail call <vscale x 4 x i8> @llvm.riscv.vadd.nxv4i8.nxv4i8.i64(<vscale x 4 x i8> poison, <vscale x 4 x i8> %0, <vscale x 4 x i8> %9, i64 %3)
  ret <vscale x 4 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i8> @_Z6AddSubIu15__rvv_uint8m1_tET_S0_S0_(<vscale x 8 x i8> %0, <vscale x 8 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vrsub.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 0)
  %6 = tail call <vscale x 4 x i16> @llvm.riscv.vmv.v.x.nxv4i16.i64(<vscale x 4 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 4 x i16> %6 to <vscale x 8 x i8>
  %8 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i8.i8.i64(<vscale x 8 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 8 x i8> @llvm.riscv.vmerge.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %1, <vscale x 8 x i8> %4, <vscale x 8 x i1> %8, i64 %3)
  %10 = tail call <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %0, <vscale x 8 x i8> %9, i64 %3)
  ret <vscale x 8 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 16 x i8> @_Z6AddSubIu15__rvv_uint8m2_tET_S0_S0_(<vscale x 16 x i8> %0, <vscale x 16 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 1)
  %4 = tail call <vscale x 16 x i8> @llvm.riscv.vrsub.nxv16i8.i8.i64(<vscale x 16 x i8> poison, <vscale x 16 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 1)
  %6 = tail call <vscale x 8 x i16> @llvm.riscv.vmv.v.x.nxv8i16.i64(<vscale x 8 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 8 x i16> %6 to <vscale x 16 x i8>
  %8 = tail call <vscale x 16 x i1> @llvm.riscv.vmsne.nxv16i8.i8.i64(<vscale x 16 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 16 x i8> @llvm.riscv.vmerge.nxv16i8.nxv16i8.i64(<vscale x 16 x i8> poison, <vscale x 16 x i8> %1, <vscale x 16 x i8> %4, <vscale x 16 x i1> %8, i64 %3)
  %10 = tail call <vscale x 16 x i8> @llvm.riscv.vadd.nxv16i8.nxv16i8.i64(<vscale x 16 x i8> poison, <vscale x 16 x i8> %0, <vscale x 16 x i8> %9, i64 %3)
  ret <vscale x 16 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 32 x i8> @_Z6AddSubIu15__rvv_uint8m4_tET_S0_S0_(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 2)
  %4 = tail call <vscale x 32 x i8> @llvm.riscv.vrsub.nxv32i8.i8.i64(<vscale x 32 x i8> poison, <vscale x 32 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 2)
  %6 = tail call <vscale x 16 x i16> @llvm.riscv.vmv.v.x.nxv16i16.i64(<vscale x 16 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 16 x i16> %6 to <vscale x 32 x i8>
  %8 = tail call <vscale x 32 x i1> @llvm.riscv.vmsne.nxv32i8.i8.i64(<vscale x 32 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 32 x i8> @llvm.riscv.vmerge.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> poison, <vscale x 32 x i8> %1, <vscale x 32 x i8> %4, <vscale x 32 x i1> %8, i64 %3)
  %10 = tail call <vscale x 32 x i8> @llvm.riscv.vadd.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> poison, <vscale x 32 x i8> %0, <vscale x 32 x i8> %9, i64 %3)
  ret <vscale x 32 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 64 x i8> @_Z6AddSubIu15__rvv_uint8m8_tET_S0_S0_(<vscale x 64 x i8> %0, <vscale x 64 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 3)
  %4 = tail call <vscale x 64 x i8> @llvm.riscv.vrsub.nxv64i8.i8.i64(<vscale x 64 x i8> poison, <vscale x 64 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 3)
  %6 = tail call <vscale x 32 x i16> @llvm.riscv.vmv.v.x.nxv32i16.i64(<vscale x 32 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 32 x i16> %6 to <vscale x 64 x i8>
  %8 = tail call <vscale x 64 x i1> @llvm.riscv.vmsne.nxv64i8.i8.i64(<vscale x 64 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 64 x i8> @llvm.riscv.vmerge.nxv64i8.nxv64i8.i64(<vscale x 64 x i8> poison, <vscale x 64 x i8> %1, <vscale x 64 x i8> %4, <vscale x 64 x i1> %8, i64 %3)
  %10 = tail call <vscale x 64 x i8> @llvm.riscv.vadd.nxv64i8.nxv64i8.i64(<vscale x 64 x i8> poison, <vscale x 64 x i8> %0, <vscale x 64 x i8> %9, i64 %3)
  ret <vscale x 64 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i16> @_Z6AddSubIu17__rvv_uint16mf4_tET_S0_S0_(<vscale x 1 x i16> %0, <vscale x 1 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 6)
  %4 = tail call <vscale x 1 x i16> @llvm.riscv.vrsub.nxv1i16.i16.i64(<vscale x 1 x i16> poison, <vscale x 1 x i16> %1, i16 0, i64 %3)
  %5 = tail call i64 @llvm.read_register.i64(metadata !9)
  %6 = lshr i64 %5, 4
  %7 = tail call noundef i64 @llvm.umin.i64(i64 %6, i64 512)
  %8 = tail call <vscale x 1 x i32> @llvm.riscv.vmv.v.x.nxv1i32.i64(<vscale x 1 x i32> poison, i32 1, i64 %7)
  %9 = bitcast <vscale x 1 x i32> %8 to <vscale x 4 x i8>
  %10 = tail call <vscale x 2 x i8> @llvm.vector.extract.nxv2i8.nxv4i8(<vscale x 4 x i8> %9, i64 0)
  %11 = bitcast <vscale x 2 x i8> %10 to <vscale x 1 x i16>
  %12 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i16.i16.i64(<vscale x 1 x i16> %11, i16 0, i64 %3)
  %13 = tail call <vscale x 1 x i16> @llvm.riscv.vmerge.nxv1i16.nxv1i16.i64(<vscale x 1 x i16> poison, <vscale x 1 x i16> %1, <vscale x 1 x i16> %4, <vscale x 1 x i1> %12, i64 %3)
  %14 = tail call <vscale x 1 x i16> @llvm.riscv.vadd.nxv1i16.nxv1i16.i64(<vscale x 1 x i16> poison, <vscale x 1 x i16> %0, <vscale x 1 x i16> %13, i64 %3)
  ret <vscale x 1 x i16> %14
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i16> @_Z6AddSubIu17__rvv_uint16mf2_tET_S0_S0_(<vscale x 2 x i16> %0, <vscale x 2 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 7)
  %4 = tail call <vscale x 2 x i16> @llvm.riscv.vrsub.nxv2i16.i16.i64(<vscale x 2 x i16> poison, <vscale x 2 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 7)
  %6 = tail call <vscale x 1 x i32> @llvm.riscv.vmv.v.x.nxv1i32.i64(<vscale x 1 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 1 x i32> %6 to <vscale x 2 x i16>
  %8 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i16.i16.i64(<vscale x 2 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 2 x i16> @llvm.riscv.vmerge.nxv2i16.nxv2i16.i64(<vscale x 2 x i16> poison, <vscale x 2 x i16> %1, <vscale x 2 x i16> %4, <vscale x 2 x i1> %8, i64 %3)
  %10 = tail call <vscale x 2 x i16> @llvm.riscv.vadd.nxv2i16.nxv2i16.i64(<vscale x 2 x i16> poison, <vscale x 2 x i16> %0, <vscale x 2 x i16> %9, i64 %3)
  ret <vscale x 2 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i16> @_Z6AddSubIu16__rvv_uint16m1_tET_S0_S0_(<vscale x 4 x i16> %0, <vscale x 4 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 0)
  %4 = tail call <vscale x 4 x i16> @llvm.riscv.vrsub.nxv4i16.i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %6 = tail call <vscale x 2 x i32> @llvm.riscv.vmv.v.x.nxv2i32.i64(<vscale x 2 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 2 x i32> %6 to <vscale x 4 x i16>
  %8 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i16.i16.i64(<vscale x 4 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 4 x i16> @llvm.riscv.vmerge.nxv4i16.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i16> %1, <vscale x 4 x i16> %4, <vscale x 4 x i1> %8, i64 %3)
  %10 = tail call <vscale x 4 x i16> @llvm.riscv.vadd.nxv4i16.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i16> %0, <vscale x 4 x i16> %9, i64 %3)
  ret <vscale x 4 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i16> @_Z6AddSubIu16__rvv_uint16m2_tET_S0_S0_(<vscale x 8 x i16> %0, <vscale x 8 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 1)
  %4 = tail call <vscale x 8 x i16> @llvm.riscv.vrsub.nxv8i16.i16.i64(<vscale x 8 x i16> poison, <vscale x 8 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 1)
  %6 = tail call <vscale x 4 x i32> @llvm.riscv.vmv.v.x.nxv4i32.i64(<vscale x 4 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 4 x i32> %6 to <vscale x 8 x i16>
  %8 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i16.i16.i64(<vscale x 8 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 8 x i16> @llvm.riscv.vmerge.nxv8i16.nxv8i16.i64(<vscale x 8 x i16> poison, <vscale x 8 x i16> %1, <vscale x 8 x i16> %4, <vscale x 8 x i1> %8, i64 %3)
  %10 = tail call <vscale x 8 x i16> @llvm.riscv.vadd.nxv8i16.nxv8i16.i64(<vscale x 8 x i16> poison, <vscale x 8 x i16> %0, <vscale x 8 x i16> %9, i64 %3)
  ret <vscale x 8 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 16 x i16> @_Z6AddSubIu16__rvv_uint16m4_tET_S0_S0_(<vscale x 16 x i16> %0, <vscale x 16 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 2)
  %4 = tail call <vscale x 16 x i16> @llvm.riscv.vrsub.nxv16i16.i16.i64(<vscale x 16 x i16> poison, <vscale x 16 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 2)
  %6 = tail call <vscale x 8 x i32> @llvm.riscv.vmv.v.x.nxv8i32.i64(<vscale x 8 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 8 x i32> %6 to <vscale x 16 x i16>
  %8 = tail call <vscale x 16 x i1> @llvm.riscv.vmsne.nxv16i16.i16.i64(<vscale x 16 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 16 x i16> @llvm.riscv.vmerge.nxv16i16.nxv16i16.i64(<vscale x 16 x i16> poison, <vscale x 16 x i16> %1, <vscale x 16 x i16> %4, <vscale x 16 x i1> %8, i64 %3)
  %10 = tail call <vscale x 16 x i16> @llvm.riscv.vadd.nxv16i16.nxv16i16.i64(<vscale x 16 x i16> poison, <vscale x 16 x i16> %0, <vscale x 16 x i16> %9, i64 %3)
  ret <vscale x 16 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 32 x i16> @_Z6AddSubIu16__rvv_uint16m8_tET_S0_S0_(<vscale x 32 x i16> %0, <vscale x 32 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 3)
  %4 = tail call <vscale x 32 x i16> @llvm.riscv.vrsub.nxv32i16.i16.i64(<vscale x 32 x i16> poison, <vscale x 32 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 3)
  %6 = tail call <vscale x 16 x i32> @llvm.riscv.vmv.v.x.nxv16i32.i64(<vscale x 16 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 16 x i32> %6 to <vscale x 32 x i16>
  %8 = tail call <vscale x 32 x i1> @llvm.riscv.vmsne.nxv32i16.i16.i64(<vscale x 32 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 32 x i16> @llvm.riscv.vmerge.nxv32i16.nxv32i16.i64(<vscale x 32 x i16> poison, <vscale x 32 x i16> %1, <vscale x 32 x i16> %4, <vscale x 32 x i1> %8, i64 %3)
  %10 = tail call <vscale x 32 x i16> @llvm.riscv.vadd.nxv32i16.nxv32i16.i64(<vscale x 32 x i16> poison, <vscale x 32 x i16> %0, <vscale x 32 x i16> %9, i64 %3)
  ret <vscale x 32 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i32> @_Z6AddSubIu17__rvv_uint32mf2_tET_S0_S0_(<vscale x 1 x i32> %0, <vscale x 1 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 7)
  %4 = tail call <vscale x 1 x i32> @llvm.riscv.vrsub.nxv1i32.i32.i64(<vscale x 1 x i32> poison, <vscale x 1 x i32> %1, i32 0, i64 %3)
  %5 = tail call i64 @llvm.read_register.i64(metadata !9)
  %6 = lshr i64 %5, 4
  %7 = tail call noundef i64 @llvm.umin.i64(i64 %6, i64 512)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %7)
  %9 = bitcast <vscale x 1 x i64> %8 to <vscale x 8 x i8>
  %10 = tail call <vscale x 4 x i8> @llvm.vector.extract.nxv4i8.nxv8i8(<vscale x 8 x i8> %9, i64 0)
  %11 = bitcast <vscale x 4 x i8> %10 to <vscale x 1 x i32>
  %12 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i32.i32.i64(<vscale x 1 x i32> %11, i32 0, i64 %3)
  %13 = tail call <vscale x 1 x i32> @llvm.riscv.vmerge.nxv1i32.nxv1i32.i64(<vscale x 1 x i32> poison, <vscale x 1 x i32> %1, <vscale x 1 x i32> %4, <vscale x 1 x i1> %12, i64 %3)
  %14 = tail call <vscale x 1 x i32> @llvm.riscv.vadd.nxv1i32.nxv1i32.i64(<vscale x 1 x i32> poison, <vscale x 1 x i32> %0, <vscale x 1 x i32> %13, i64 %3)
  ret <vscale x 1 x i32> %14
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i32> @_Z6AddSubIu16__rvv_uint32m1_tET_S0_S0_(<vscale x 2 x i32> %0, <vscale x 2 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %4 = tail call <vscale x 2 x i32> @llvm.riscv.vrsub.nxv2i32.i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %1, i32 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %5)
  %7 = bitcast <vscale x 1 x i64> %6 to <vscale x 2 x i32>
  %8 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i32.i32.i64(<vscale x 2 x i32> %7, i32 0, i64 %3)
  %9 = tail call <vscale x 2 x i32> @llvm.riscv.vmerge.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %1, <vscale x 2 x i32> %4, <vscale x 2 x i1> %8, i64 %3)
  %10 = tail call <vscale x 2 x i32> @llvm.riscv.vadd.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %9, i64 %3)
  ret <vscale x 2 x i32> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i32> @_Z6AddSubIu16__rvv_uint32m2_tET_S0_S0_(<vscale x 4 x i32> %0, <vscale x 4 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 1)
  %4 = tail call <vscale x 4 x i32> @llvm.riscv.vrsub.nxv4i32.i32.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %1, i32 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64> poison, i64 1, i64 %5)
  %7 = bitcast <vscale x 2 x i64> %6 to <vscale x 4 x i32>
  %8 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i32.i32.i64(<vscale x 4 x i32> %7, i32 0, i64 %3)
  %9 = tail call <vscale x 4 x i32> @llvm.riscv.vmerge.nxv4i32.nxv4i32.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %1, <vscale x 4 x i32> %4, <vscale x 4 x i1> %8, i64 %3)
  %10 = tail call <vscale x 4 x i32> @llvm.riscv.vadd.nxv4i32.nxv4i32.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %0, <vscale x 4 x i32> %9, i64 %3)
  ret <vscale x 4 x i32> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i32> @_Z6AddSubIu16__rvv_uint32m4_tET_S0_S0_(<vscale x 8 x i32> %0, <vscale x 8 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 2)
  %4 = tail call <vscale x 8 x i32> @llvm.riscv.vrsub.nxv8i32.i32.i64(<vscale x 8 x i32> poison, <vscale x 8 x i32> %1, i32 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64> poison, i64 1, i64 %5)
  %7 = bitcast <vscale x 4 x i64> %6 to <vscale x 8 x i32>
  %8 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i32.i32.i64(<vscale x 8 x i32> %7, i32 0, i64 %3)
  %9 = tail call <vscale x 8 x i32> @llvm.riscv.vmerge.nxv8i32.nxv8i32.i64(<vscale x 8 x i32> poison, <vscale x 8 x i32> %1, <vscale x 8 x i32> %4, <vscale x 8 x i1> %8, i64 %3)
  %10 = tail call <vscale x 8 x i32> @llvm.riscv.vadd.nxv8i32.nxv8i32.i64(<vscale x 8 x i32> poison, <vscale x 8 x i32> %0, <vscale x 8 x i32> %9, i64 %3)
  ret <vscale x 8 x i32> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 16 x i32> @_Z6AddSubIu16__rvv_uint32m8_tET_S0_S0_(<vscale x 16 x i32> %0, <vscale x 16 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 3)
  %4 = tail call <vscale x 16 x i32> @llvm.riscv.vrsub.nxv16i32.i32.i64(<vscale x 16 x i32> poison, <vscale x 16 x i32> %1, i32 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64> poison, i64 1, i64 %5)
  %7 = bitcast <vscale x 8 x i64> %6 to <vscale x 16 x i32>
  %8 = tail call <vscale x 16 x i1> @llvm.riscv.vmsne.nxv16i32.i32.i64(<vscale x 16 x i32> %7, i32 0, i64 %3)
  %9 = tail call <vscale x 16 x i32> @llvm.riscv.vmerge.nxv16i32.nxv16i32.i64(<vscale x 16 x i32> poison, <vscale x 16 x i32> %1, <vscale x 16 x i32> %4, <vscale x 16 x i1> %8, i64 %3)
  %10 = tail call <vscale x 16 x i32> @llvm.riscv.vadd.nxv16i32.nxv16i32.i64(<vscale x 16 x i32> poison, <vscale x 16 x i32> %0, <vscale x 16 x i32> %9, i64 %3)
  ret <vscale x 16 x i32> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i64> @_Z6AddSubIu16__rvv_uint64m1_tET_S0_S0_(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vrsub.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, i64 0, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %3)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, <vscale x 1 x i64> %4, <vscale x 1 x i1> %7, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vadd.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %8, i64 %3)
  ret <vscale x 1 x i64> %9
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i64> @_Z6AddSubIu16__rvv_uint64m2_tET_S0_S0_(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %4 = tail call <vscale x 2 x i64> @llvm.riscv.vrsub.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %1, i64 0, i64 %3)
  %5 = tail call <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64> poison, i64 %3)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %5, i64 1, i64 %3)
  %7 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %1, <vscale x 2 x i64> %4, <vscale x 2 x i1> %7, i64 %3)
  %9 = tail call <vscale x 2 x i64> @llvm.riscv.vadd.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %0, <vscale x 2 x i64> %8, i64 %3)
  ret <vscale x 2 x i64> %9
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i64> @_Z6AddSubIu16__rvv_uint64m4_tET_S0_S0_(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %4 = tail call <vscale x 4 x i64> @llvm.riscv.vrsub.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %1, i64 0, i64 %3)
  %5 = tail call <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64> poison, i64 %3)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %5, i64 1, i64 %3)
  %7 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %1, <vscale x 4 x i64> %4, <vscale x 4 x i1> %7, i64 %3)
  %9 = tail call <vscale x 4 x i64> @llvm.riscv.vadd.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %0, <vscale x 4 x i64> %8, i64 %3)
  ret <vscale x 4 x i64> %9
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i64> @_Z6AddSubIu16__rvv_uint64m8_tET_S0_S0_(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %4 = tail call <vscale x 8 x i64> @llvm.riscv.vrsub.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %1, i64 0, i64 %3)
  %5 = tail call <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64> poison, i64 %3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %5, i64 1, i64 %3)
  %7 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %1, <vscale x 8 x i64> %4, <vscale x 8 x i1> %7, i64 %3)
  %9 = tail call <vscale x 8 x i64> @llvm.riscv.vadd.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %0, <vscale x 8 x i64> %8, i64 %3)
  ret <vscale x 8 x i64> %9
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i8> @_Z6AddSubIu15__rvv_int8mf8_tET_S0_S0_(<vscale x 1 x i8> %0, <vscale x 1 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 5)
  %4 = tail call <vscale x 1 x i8> @llvm.riscv.vrsub.nxv1i8.i8.i64(<vscale x 1 x i8> poison, <vscale x 1 x i8> %1, i8 0, i64 %3)
  %5 = tail call i64 @llvm.read_register.i64(metadata !9)
  %6 = lshr i64 %5, 4
  %7 = tail call noundef i64 @llvm.umin.i64(i64 %6, i64 512)
  %8 = tail call <vscale x 1 x i16> @llvm.riscv.vmv.v.x.nxv1i16.i64(<vscale x 1 x i16> poison, i16 1, i64 %7)
  %9 = bitcast <vscale x 1 x i16> %8 to <vscale x 2 x i8>
  %10 = tail call <vscale x 1 x i8> @llvm.vector.extract.nxv1i8.nxv2i8(<vscale x 2 x i8> %9, i64 0)
  %11 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i8.i8.i64(<vscale x 1 x i8> %10, i8 0, i64 %3)
  %12 = tail call <vscale x 1 x i8> @llvm.riscv.vmerge.nxv1i8.nxv1i8.i64(<vscale x 1 x i8> poison, <vscale x 1 x i8> %1, <vscale x 1 x i8> %4, <vscale x 1 x i1> %11, i64 %3)
  %13 = tail call <vscale x 1 x i8> @llvm.riscv.vadd.nxv1i8.nxv1i8.i64(<vscale x 1 x i8> poison, <vscale x 1 x i8> %0, <vscale x 1 x i8> %12, i64 %3)
  ret <vscale x 1 x i8> %13
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i8> @_Z6AddSubIu15__rvv_int8mf4_tET_S0_S0_(<vscale x 2 x i8> %0, <vscale x 2 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 6)
  %4 = tail call <vscale x 2 x i8> @llvm.riscv.vrsub.nxv2i8.i8.i64(<vscale x 2 x i8> poison, <vscale x 2 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 6)
  %6 = tail call <vscale x 1 x i16> @llvm.riscv.vmv.v.x.nxv1i16.i64(<vscale x 1 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 1 x i16> %6 to <vscale x 2 x i8>
  %8 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i8.i8.i64(<vscale x 2 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 2 x i8> @llvm.riscv.vmerge.nxv2i8.nxv2i8.i64(<vscale x 2 x i8> poison, <vscale x 2 x i8> %1, <vscale x 2 x i8> %4, <vscale x 2 x i1> %8, i64 %3)
  %10 = tail call <vscale x 2 x i8> @llvm.riscv.vadd.nxv2i8.nxv2i8.i64(<vscale x 2 x i8> poison, <vscale x 2 x i8> %0, <vscale x 2 x i8> %9, i64 %3)
  ret <vscale x 2 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i8> @_Z6AddSubIu15__rvv_int8mf2_tET_S0_S0_(<vscale x 4 x i8> %0, <vscale x 4 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 7)
  %4 = tail call <vscale x 4 x i8> @llvm.riscv.vrsub.nxv4i8.i8.i64(<vscale x 4 x i8> poison, <vscale x 4 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 7)
  %6 = tail call <vscale x 2 x i16> @llvm.riscv.vmv.v.x.nxv2i16.i64(<vscale x 2 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 2 x i16> %6 to <vscale x 4 x i8>
  %8 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i8.i8.i64(<vscale x 4 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 4 x i8> @llvm.riscv.vmerge.nxv4i8.nxv4i8.i64(<vscale x 4 x i8> poison, <vscale x 4 x i8> %1, <vscale x 4 x i8> %4, <vscale x 4 x i1> %8, i64 %3)
  %10 = tail call <vscale x 4 x i8> @llvm.riscv.vadd.nxv4i8.nxv4i8.i64(<vscale x 4 x i8> poison, <vscale x 4 x i8> %0, <vscale x 4 x i8> %9, i64 %3)
  ret <vscale x 4 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i8> @_Z6AddSubIu14__rvv_int8m1_tET_S0_S0_(<vscale x 8 x i8> %0, <vscale x 8 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vrsub.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 0)
  %6 = tail call <vscale x 4 x i16> @llvm.riscv.vmv.v.x.nxv4i16.i64(<vscale x 4 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 4 x i16> %6 to <vscale x 8 x i8>
  %8 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i8.i8.i64(<vscale x 8 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 8 x i8> @llvm.riscv.vmerge.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %1, <vscale x 8 x i8> %4, <vscale x 8 x i1> %8, i64 %3)
  %10 = tail call <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %0, <vscale x 8 x i8> %9, i64 %3)
  ret <vscale x 8 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 16 x i8> @_Z6AddSubIu14__rvv_int8m2_tET_S0_S0_(<vscale x 16 x i8> %0, <vscale x 16 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 1)
  %4 = tail call <vscale x 16 x i8> @llvm.riscv.vrsub.nxv16i8.i8.i64(<vscale x 16 x i8> poison, <vscale x 16 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 1)
  %6 = tail call <vscale x 8 x i16> @llvm.riscv.vmv.v.x.nxv8i16.i64(<vscale x 8 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 8 x i16> %6 to <vscale x 16 x i8>
  %8 = tail call <vscale x 16 x i1> @llvm.riscv.vmsne.nxv16i8.i8.i64(<vscale x 16 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 16 x i8> @llvm.riscv.vmerge.nxv16i8.nxv16i8.i64(<vscale x 16 x i8> poison, <vscale x 16 x i8> %1, <vscale x 16 x i8> %4, <vscale x 16 x i1> %8, i64 %3)
  %10 = tail call <vscale x 16 x i8> @llvm.riscv.vadd.nxv16i8.nxv16i8.i64(<vscale x 16 x i8> poison, <vscale x 16 x i8> %0, <vscale x 16 x i8> %9, i64 %3)
  ret <vscale x 16 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 32 x i8> @_Z6AddSubIu14__rvv_int8m4_tET_S0_S0_(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 2)
  %4 = tail call <vscale x 32 x i8> @llvm.riscv.vrsub.nxv32i8.i8.i64(<vscale x 32 x i8> poison, <vscale x 32 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 2)
  %6 = tail call <vscale x 16 x i16> @llvm.riscv.vmv.v.x.nxv16i16.i64(<vscale x 16 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 16 x i16> %6 to <vscale x 32 x i8>
  %8 = tail call <vscale x 32 x i1> @llvm.riscv.vmsne.nxv32i8.i8.i64(<vscale x 32 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 32 x i8> @llvm.riscv.vmerge.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> poison, <vscale x 32 x i8> %1, <vscale x 32 x i8> %4, <vscale x 32 x i1> %8, i64 %3)
  %10 = tail call <vscale x 32 x i8> @llvm.riscv.vadd.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> poison, <vscale x 32 x i8> %0, <vscale x 32 x i8> %9, i64 %3)
  ret <vscale x 32 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 64 x i8> @_Z6AddSubIu14__rvv_int8m8_tET_S0_S0_(<vscale x 64 x i8> %0, <vscale x 64 x i8> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 3)
  %4 = tail call <vscale x 64 x i8> @llvm.riscv.vrsub.nxv64i8.i8.i64(<vscale x 64 x i8> poison, <vscale x 64 x i8> %1, i8 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 3)
  %6 = tail call <vscale x 32 x i16> @llvm.riscv.vmv.v.x.nxv32i16.i64(<vscale x 32 x i16> poison, i16 1, i64 %5)
  %7 = bitcast <vscale x 32 x i16> %6 to <vscale x 64 x i8>
  %8 = tail call <vscale x 64 x i1> @llvm.riscv.vmsne.nxv64i8.i8.i64(<vscale x 64 x i8> %7, i8 0, i64 %3)
  %9 = tail call <vscale x 64 x i8> @llvm.riscv.vmerge.nxv64i8.nxv64i8.i64(<vscale x 64 x i8> poison, <vscale x 64 x i8> %1, <vscale x 64 x i8> %4, <vscale x 64 x i1> %8, i64 %3)
  %10 = tail call <vscale x 64 x i8> @llvm.riscv.vadd.nxv64i8.nxv64i8.i64(<vscale x 64 x i8> poison, <vscale x 64 x i8> %0, <vscale x 64 x i8> %9, i64 %3)
  ret <vscale x 64 x i8> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i16> @_Z6AddSubIu16__rvv_int16mf4_tET_S0_S0_(<vscale x 1 x i16> %0, <vscale x 1 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 6)
  %4 = tail call <vscale x 1 x i16> @llvm.riscv.vrsub.nxv1i16.i16.i64(<vscale x 1 x i16> poison, <vscale x 1 x i16> %1, i16 0, i64 %3)
  %5 = tail call i64 @llvm.read_register.i64(metadata !9)
  %6 = lshr i64 %5, 4
  %7 = tail call noundef i64 @llvm.umin.i64(i64 %6, i64 512)
  %8 = tail call <vscale x 1 x i32> @llvm.riscv.vmv.v.x.nxv1i32.i64(<vscale x 1 x i32> poison, i32 1, i64 %7)
  %9 = bitcast <vscale x 1 x i32> %8 to <vscale x 4 x i8>
  %10 = tail call <vscale x 2 x i8> @llvm.vector.extract.nxv2i8.nxv4i8(<vscale x 4 x i8> %9, i64 0)
  %11 = bitcast <vscale x 2 x i8> %10 to <vscale x 1 x i16>
  %12 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i16.i16.i64(<vscale x 1 x i16> %11, i16 0, i64 %3)
  %13 = tail call <vscale x 1 x i16> @llvm.riscv.vmerge.nxv1i16.nxv1i16.i64(<vscale x 1 x i16> poison, <vscale x 1 x i16> %1, <vscale x 1 x i16> %4, <vscale x 1 x i1> %12, i64 %3)
  %14 = tail call <vscale x 1 x i16> @llvm.riscv.vadd.nxv1i16.nxv1i16.i64(<vscale x 1 x i16> poison, <vscale x 1 x i16> %0, <vscale x 1 x i16> %13, i64 %3)
  ret <vscale x 1 x i16> %14
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i16> @_Z6AddSubIu16__rvv_int16mf2_tET_S0_S0_(<vscale x 2 x i16> %0, <vscale x 2 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 7)
  %4 = tail call <vscale x 2 x i16> @llvm.riscv.vrsub.nxv2i16.i16.i64(<vscale x 2 x i16> poison, <vscale x 2 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 7)
  %6 = tail call <vscale x 1 x i32> @llvm.riscv.vmv.v.x.nxv1i32.i64(<vscale x 1 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 1 x i32> %6 to <vscale x 2 x i16>
  %8 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i16.i16.i64(<vscale x 2 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 2 x i16> @llvm.riscv.vmerge.nxv2i16.nxv2i16.i64(<vscale x 2 x i16> poison, <vscale x 2 x i16> %1, <vscale x 2 x i16> %4, <vscale x 2 x i1> %8, i64 %3)
  %10 = tail call <vscale x 2 x i16> @llvm.riscv.vadd.nxv2i16.nxv2i16.i64(<vscale x 2 x i16> poison, <vscale x 2 x i16> %0, <vscale x 2 x i16> %9, i64 %3)
  ret <vscale x 2 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i16> @_Z6AddSubIu15__rvv_int16m1_tET_S0_S0_(<vscale x 4 x i16> %0, <vscale x 4 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 0)
  %4 = tail call <vscale x 4 x i16> @llvm.riscv.vrsub.nxv4i16.i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %6 = tail call <vscale x 2 x i32> @llvm.riscv.vmv.v.x.nxv2i32.i64(<vscale x 2 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 2 x i32> %6 to <vscale x 4 x i16>
  %8 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i16.i16.i64(<vscale x 4 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 4 x i16> @llvm.riscv.vmerge.nxv4i16.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i16> %1, <vscale x 4 x i16> %4, <vscale x 4 x i1> %8, i64 %3)
  %10 = tail call <vscale x 4 x i16> @llvm.riscv.vadd.nxv4i16.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i16> %0, <vscale x 4 x i16> %9, i64 %3)
  ret <vscale x 4 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i16> @_Z6AddSubIu15__rvv_int16m2_tET_S0_S0_(<vscale x 8 x i16> %0, <vscale x 8 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 1)
  %4 = tail call <vscale x 8 x i16> @llvm.riscv.vrsub.nxv8i16.i16.i64(<vscale x 8 x i16> poison, <vscale x 8 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 1)
  %6 = tail call <vscale x 4 x i32> @llvm.riscv.vmv.v.x.nxv4i32.i64(<vscale x 4 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 4 x i32> %6 to <vscale x 8 x i16>
  %8 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i16.i16.i64(<vscale x 8 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 8 x i16> @llvm.riscv.vmerge.nxv8i16.nxv8i16.i64(<vscale x 8 x i16> poison, <vscale x 8 x i16> %1, <vscale x 8 x i16> %4, <vscale x 8 x i1> %8, i64 %3)
  %10 = tail call <vscale x 8 x i16> @llvm.riscv.vadd.nxv8i16.nxv8i16.i64(<vscale x 8 x i16> poison, <vscale x 8 x i16> %0, <vscale x 8 x i16> %9, i64 %3)
  ret <vscale x 8 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 16 x i16> @_Z6AddSubIu15__rvv_int16m4_tET_S0_S0_(<vscale x 16 x i16> %0, <vscale x 16 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 2)
  %4 = tail call <vscale x 16 x i16> @llvm.riscv.vrsub.nxv16i16.i16.i64(<vscale x 16 x i16> poison, <vscale x 16 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 2)
  %6 = tail call <vscale x 8 x i32> @llvm.riscv.vmv.v.x.nxv8i32.i64(<vscale x 8 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 8 x i32> %6 to <vscale x 16 x i16>
  %8 = tail call <vscale x 16 x i1> @llvm.riscv.vmsne.nxv16i16.i16.i64(<vscale x 16 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 16 x i16> @llvm.riscv.vmerge.nxv16i16.nxv16i16.i64(<vscale x 16 x i16> poison, <vscale x 16 x i16> %1, <vscale x 16 x i16> %4, <vscale x 16 x i1> %8, i64 %3)
  %10 = tail call <vscale x 16 x i16> @llvm.riscv.vadd.nxv16i16.nxv16i16.i64(<vscale x 16 x i16> poison, <vscale x 16 x i16> %0, <vscale x 16 x i16> %9, i64 %3)
  ret <vscale x 16 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 32 x i16> @_Z6AddSubIu15__rvv_int16m8_tET_S0_S0_(<vscale x 32 x i16> %0, <vscale x 32 x i16> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 3)
  %4 = tail call <vscale x 32 x i16> @llvm.riscv.vrsub.nxv32i16.i16.i64(<vscale x 32 x i16> poison, <vscale x 32 x i16> %1, i16 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 3)
  %6 = tail call <vscale x 16 x i32> @llvm.riscv.vmv.v.x.nxv16i32.i64(<vscale x 16 x i32> poison, i32 1, i64 %5)
  %7 = bitcast <vscale x 16 x i32> %6 to <vscale x 32 x i16>
  %8 = tail call <vscale x 32 x i1> @llvm.riscv.vmsne.nxv32i16.i16.i64(<vscale x 32 x i16> %7, i16 0, i64 %3)
  %9 = tail call <vscale x 32 x i16> @llvm.riscv.vmerge.nxv32i16.nxv32i16.i64(<vscale x 32 x i16> poison, <vscale x 32 x i16> %1, <vscale x 32 x i16> %4, <vscale x 32 x i1> %8, i64 %3)
  %10 = tail call <vscale x 32 x i16> @llvm.riscv.vadd.nxv32i16.nxv32i16.i64(<vscale x 32 x i16> poison, <vscale x 32 x i16> %0, <vscale x 32 x i16> %9, i64 %3)
  ret <vscale x 32 x i16> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i32> @_Z6AddSubIu16__rvv_int32mf2_tET_S0_S0_(<vscale x 1 x i32> %0, <vscale x 1 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 7)
  %4 = tail call <vscale x 1 x i32> @llvm.riscv.vrsub.nxv1i32.i32.i64(<vscale x 1 x i32> poison, <vscale x 1 x i32> %1, i32 0, i64 %3)
  %5 = tail call i64 @llvm.read_register.i64(metadata !9)
  %6 = lshr i64 %5, 4
  %7 = tail call noundef i64 @llvm.umin.i64(i64 %6, i64 512)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %7)
  %9 = bitcast <vscale x 1 x i64> %8 to <vscale x 8 x i8>
  %10 = tail call <vscale x 4 x i8> @llvm.vector.extract.nxv4i8.nxv8i8(<vscale x 8 x i8> %9, i64 0)
  %11 = bitcast <vscale x 4 x i8> %10 to <vscale x 1 x i32>
  %12 = tail call <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i32.i32.i64(<vscale x 1 x i32> %11, i32 0, i64 %3)
  %13 = tail call <vscale x 1 x i32> @llvm.riscv.vmerge.nxv1i32.nxv1i32.i64(<vscale x 1 x i32> poison, <vscale x 1 x i32> %1, <vscale x 1 x i32> %4, <vscale x 1 x i1> %12, i64 %3)
  %14 = tail call <vscale x 1 x i32> @llvm.riscv.vadd.nxv1i32.nxv1i32.i64(<vscale x 1 x i32> poison, <vscale x 1 x i32> %0, <vscale x 1 x i32> %13, i64 %3)
  ret <vscale x 1 x i32> %14
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i32> @_Z6AddSubIu15__rvv_int32m1_tET_S0_S0_(<vscale x 2 x i32> %0, <vscale x 2 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 0)
  %4 = tail call <vscale x 2 x i32> @llvm.riscv.vrsub.nxv2i32.i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %1, i32 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64> poison, i64 1, i64 %5)
  %7 = bitcast <vscale x 1 x i64> %6 to <vscale x 2 x i32>
  %8 = tail call <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i32.i32.i64(<vscale x 2 x i32> %7, i32 0, i64 %3)
  %9 = tail call <vscale x 2 x i32> @llvm.riscv.vmerge.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %1, <vscale x 2 x i32> %4, <vscale x 2 x i1> %8, i64 %3)
  %10 = tail call <vscale x 2 x i32> @llvm.riscv.vadd.nxv2i32.nxv2i32.i64(<vscale x 2 x i32> poison, <vscale x 2 x i32> %0, <vscale x 2 x i32> %9, i64 %3)
  ret <vscale x 2 x i32> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i32> @_Z6AddSubIu15__rvv_int32m2_tET_S0_S0_(<vscale x 4 x i32> %0, <vscale x 4 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 1)
  %4 = tail call <vscale x 4 x i32> @llvm.riscv.vrsub.nxv4i32.i32.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %1, i32 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64> poison, i64 1, i64 %5)
  %7 = bitcast <vscale x 2 x i64> %6 to <vscale x 4 x i32>
  %8 = tail call <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i32.i32.i64(<vscale x 4 x i32> %7, i32 0, i64 %3)
  %9 = tail call <vscale x 4 x i32> @llvm.riscv.vmerge.nxv4i32.nxv4i32.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %1, <vscale x 4 x i32> %4, <vscale x 4 x i1> %8, i64 %3)
  %10 = tail call <vscale x 4 x i32> @llvm.riscv.vadd.nxv4i32.nxv4i32.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %0, <vscale x 4 x i32> %9, i64 %3)
  ret <vscale x 4 x i32> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i32> @_Z6AddSubIu15__rvv_int32m4_tET_S0_S0_(<vscale x 8 x i32> %0, <vscale x 8 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 2)
  %4 = tail call <vscale x 8 x i32> @llvm.riscv.vrsub.nxv8i32.i32.i64(<vscale x 8 x i32> poison, <vscale x 8 x i32> %1, i32 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64> poison, i64 1, i64 %5)
  %7 = bitcast <vscale x 4 x i64> %6 to <vscale x 8 x i32>
  %8 = tail call <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i32.i32.i64(<vscale x 8 x i32> %7, i32 0, i64 %3)
  %9 = tail call <vscale x 8 x i32> @llvm.riscv.vmerge.nxv8i32.nxv8i32.i64(<vscale x 8 x i32> poison, <vscale x 8 x i32> %1, <vscale x 8 x i32> %4, <vscale x 8 x i1> %8, i64 %3)
  %10 = tail call <vscale x 8 x i32> @llvm.riscv.vadd.nxv8i32.nxv8i32.i64(<vscale x 8 x i32> poison, <vscale x 8 x i32> %0, <vscale x 8 x i32> %9, i64 %3)
  ret <vscale x 8 x i32> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 16 x i32> @_Z6AddSubIu15__rvv_int32m8_tET_S0_S0_(<vscale x 16 x i32> %0, <vscale x 16 x i32> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 3)
  %4 = tail call <vscale x 16 x i32> @llvm.riscv.vrsub.nxv16i32.i32.i64(<vscale x 16 x i32> poison, <vscale x 16 x i32> %1, i32 0, i64 %3)
  %5 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64> poison, i64 1, i64 %5)
  %7 = bitcast <vscale x 8 x i64> %6 to <vscale x 16 x i32>
  %8 = tail call <vscale x 16 x i1> @llvm.riscv.vmsne.nxv16i32.i32.i64(<vscale x 16 x i32> %7, i32 0, i64 %3)
  %9 = tail call <vscale x 16 x i32> @llvm.riscv.vmerge.nxv16i32.nxv16i32.i64(<vscale x 16 x i32> poison, <vscale x 16 x i32> %1, <vscale x 16 x i32> %4, <vscale x 16 x i1> %8, i64 %3)
  %10 = tail call <vscale x 16 x i32> @llvm.riscv.vadd.nxv16i32.nxv16i32.i64(<vscale x 16 x i32> poison, <vscale x 16 x i32> %0, <vscale x 16 x i32> %9, i64 %3)
  ret <vscale x 16 x i32> %10
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 1 x i64> @_Z6AddSubIu15__rvv_int64m1_tET_S0_S0_(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 0)
  %4 = tail call <vscale x 1 x i64> @llvm.riscv.vrsub.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, i64 0, i64 %3)
  %5 = tail call <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64> poison, i64 %3)
  %6 = tail call <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %5, i64 1, i64 %3)
  %7 = tail call <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %1, <vscale x 1 x i64> %4, <vscale x 1 x i1> %7, i64 %3)
  %9 = tail call <vscale x 1 x i64> @llvm.riscv.vadd.nxv1i64.nxv1i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %0, <vscale x 1 x i64> %8, i64 %3)
  ret <vscale x 1 x i64> %9
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 2 x i64> @_Z6AddSubIu15__rvv_int64m2_tET_S0_S0_(<vscale x 2 x i64> %0, <vscale x 2 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 1)
  %4 = tail call <vscale x 2 x i64> @llvm.riscv.vrsub.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %1, i64 0, i64 %3)
  %5 = tail call <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64> poison, i64 %3)
  %6 = tail call <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %5, i64 1, i64 %3)
  %7 = tail call <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %1, <vscale x 2 x i64> %4, <vscale x 2 x i1> %7, i64 %3)
  %9 = tail call <vscale x 2 x i64> @llvm.riscv.vadd.nxv2i64.nxv2i64.i64(<vscale x 2 x i64> poison, <vscale x 2 x i64> %0, <vscale x 2 x i64> %8, i64 %3)
  ret <vscale x 2 x i64> %9
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 4 x i64> @_Z6AddSubIu15__rvv_int64m4_tET_S0_S0_(<vscale x 4 x i64> %0, <vscale x 4 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 2)
  %4 = tail call <vscale x 4 x i64> @llvm.riscv.vrsub.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %1, i64 0, i64 %3)
  %5 = tail call <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64> poison, i64 %3)
  %6 = tail call <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %5, i64 1, i64 %3)
  %7 = tail call <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %1, <vscale x 4 x i64> %4, <vscale x 4 x i1> %7, i64 %3)
  %9 = tail call <vscale x 4 x i64> @llvm.riscv.vadd.nxv4i64.nxv4i64.i64(<vscale x 4 x i64> poison, <vscale x 4 x i64> %0, <vscale x 4 x i64> %8, i64 %3)
  ret <vscale x 4 x i64> %9
}

; Function Attrs: mustprogress uwtable vscale_range(2,1024)
define weak_odr dso_local <vscale x 8 x i64> @_Z6AddSubIu15__rvv_int64m8_tET_S0_S0_(<vscale x 8 x i64> %0, <vscale x 8 x i64> %1) local_unnamed_addr #0 comdat {
  %3 = tail call noundef i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  %4 = tail call <vscale x 8 x i64> @llvm.riscv.vrsub.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %1, i64 0, i64 %3)
  %5 = tail call <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64> poison, i64 %3)
  %6 = tail call <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %5, i64 1, i64 %3)
  %7 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64> %6, i64 0, i64 %3)
  %8 = tail call <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %1, <vscale x 8 x i64> %4, <vscale x 8 x i1> %7, i64 %3)
  %9 = tail call <vscale x 8 x i64> @llvm.riscv.vadd.nxv8i64.nxv8i64.i64(<vscale x 8 x i64> poison, <vscale x 8 x i64> %0, <vscale x 8 x i64> %8, i64 %3)
  ret <vscale x 8 x i64> %9
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i8> @llvm.riscv.vmerge.nxv1i8.nxv1i8.i64(<vscale x 1 x i8>, <vscale x 1 x i8>, <vscale x 1 x i8>, <vscale x 1 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare i64 @llvm.riscv.vsetvlimax.i64(i64 immarg, i64 immarg) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i8.i8.i64(<vscale x 1 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind speculatable willreturn 
declare <vscale x 1 x i8> @llvm.vector.extract.nxv1i8.nxv2i8(<vscale x 2 x i8>, i64 immarg) #2

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i16> @llvm.riscv.vmv.v.x.nxv1i16.i64(<vscale x 1 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare i64 @llvm.read_register.i64(metadata) #3

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i8> @llvm.riscv.vrsub.nxv1i8.i8.i64(<vscale x 1 x i8>, <vscale x 1 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i8> @llvm.riscv.vadd.nxv1i8.nxv1i8.i64(<vscale x 1 x i8>, <vscale x 1 x i8>, <vscale x 1 x i8>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i8> @llvm.riscv.vmerge.nxv2i8.nxv2i8.i64(<vscale x 2 x i8>, <vscale x 2 x i8>, <vscale x 2 x i8>, <vscale x 2 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i8.i8.i64(<vscale x 2 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i8> @llvm.riscv.vrsub.nxv2i8.i8.i64(<vscale x 2 x i8>, <vscale x 2 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i8> @llvm.riscv.vadd.nxv2i8.nxv2i8.i64(<vscale x 2 x i8>, <vscale x 2 x i8>, <vscale x 2 x i8>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i8> @llvm.riscv.vmerge.nxv4i8.nxv4i8.i64(<vscale x 4 x i8>, <vscale x 4 x i8>, <vscale x 4 x i8>, <vscale x 4 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i8.i8.i64(<vscale x 4 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i16> @llvm.riscv.vmv.v.x.nxv2i16.i64(<vscale x 2 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i8> @llvm.riscv.vrsub.nxv4i8.i8.i64(<vscale x 4 x i8>, <vscale x 4 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i8> @llvm.riscv.vadd.nxv4i8.nxv4i8.i64(<vscale x 4 x i8>, <vscale x 4 x i8>, <vscale x 4 x i8>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i8> @llvm.riscv.vmerge.nxv8i8.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, <vscale x 8 x i8>, <vscale x 8 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i8.i8.i64(<vscale x 8 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i16> @llvm.riscv.vmv.v.x.nxv4i16.i64(<vscale x 4 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i8> @llvm.riscv.vrsub.nxv8i8.i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, <vscale x 8 x i8>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i8> @llvm.riscv.vmerge.nxv16i8.nxv16i8.i64(<vscale x 16 x i8>, <vscale x 16 x i8>, <vscale x 16 x i8>, <vscale x 16 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i1> @llvm.riscv.vmsne.nxv16i8.i8.i64(<vscale x 16 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i16> @llvm.riscv.vmv.v.x.nxv8i16.i64(<vscale x 8 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i8> @llvm.riscv.vrsub.nxv16i8.i8.i64(<vscale x 16 x i8>, <vscale x 16 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i8> @llvm.riscv.vadd.nxv16i8.nxv16i8.i64(<vscale x 16 x i8>, <vscale x 16 x i8>, <vscale x 16 x i8>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 32 x i8> @llvm.riscv.vmerge.nxv32i8.nxv32i8.i64(<vscale x 32 x i8>, <vscale x 32 x i8>, <vscale x 32 x i8>, <vscale x 32 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 32 x i1> @llvm.riscv.vmsne.nxv32i8.i8.i64(<vscale x 32 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i16> @llvm.riscv.vmv.v.x.nxv16i16.i64(<vscale x 16 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 32 x i8> @llvm.riscv.vrsub.nxv32i8.i8.i64(<vscale x 32 x i8>, <vscale x 32 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 32 x i8> @llvm.riscv.vadd.nxv32i8.nxv32i8.i64(<vscale x 32 x i8>, <vscale x 32 x i8>, <vscale x 32 x i8>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 64 x i8> @llvm.riscv.vmerge.nxv64i8.nxv64i8.i64(<vscale x 64 x i8>, <vscale x 64 x i8>, <vscale x 64 x i8>, <vscale x 64 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 64 x i1> @llvm.riscv.vmsne.nxv64i8.i8.i64(<vscale x 64 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 32 x i16> @llvm.riscv.vmv.v.x.nxv32i16.i64(<vscale x 32 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 64 x i8> @llvm.riscv.vrsub.nxv64i8.i8.i64(<vscale x 64 x i8>, <vscale x 64 x i8>, i8, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 64 x i8> @llvm.riscv.vadd.nxv64i8.nxv64i8.i64(<vscale x 64 x i8>, <vscale x 64 x i8>, <vscale x 64 x i8>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i16> @llvm.riscv.vmerge.nxv1i16.nxv1i16.i64(<vscale x 1 x i16>, <vscale x 1 x i16>, <vscale x 1 x i16>, <vscale x 1 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i16.i16.i64(<vscale x 1 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind speculatable willreturn 
declare <vscale x 2 x i8> @llvm.vector.extract.nxv2i8.nxv4i8(<vscale x 4 x i8>, i64 immarg) #2

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i32> @llvm.riscv.vmv.v.x.nxv1i32.i64(<vscale x 1 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i16> @llvm.riscv.vrsub.nxv1i16.i16.i64(<vscale x 1 x i16>, <vscale x 1 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i16> @llvm.riscv.vadd.nxv1i16.nxv1i16.i64(<vscale x 1 x i16>, <vscale x 1 x i16>, <vscale x 1 x i16>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i16> @llvm.riscv.vmerge.nxv2i16.nxv2i16.i64(<vscale x 2 x i16>, <vscale x 2 x i16>, <vscale x 2 x i16>, <vscale x 2 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i16.i16.i64(<vscale x 2 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i16> @llvm.riscv.vrsub.nxv2i16.i16.i64(<vscale x 2 x i16>, <vscale x 2 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i16> @llvm.riscv.vadd.nxv2i16.nxv2i16.i64(<vscale x 2 x i16>, <vscale x 2 x i16>, <vscale x 2 x i16>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i16> @llvm.riscv.vmerge.nxv4i16.nxv4i16.i64(<vscale x 4 x i16>, <vscale x 4 x i16>, <vscale x 4 x i16>, <vscale x 4 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i16.i16.i64(<vscale x 4 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vmv.v.x.nxv2i32.i64(<vscale x 2 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i16> @llvm.riscv.vrsub.nxv4i16.i16.i64(<vscale x 4 x i16>, <vscale x 4 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i16> @llvm.riscv.vadd.nxv4i16.nxv4i16.i64(<vscale x 4 x i16>, <vscale x 4 x i16>, <vscale x 4 x i16>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i16> @llvm.riscv.vmerge.nxv8i16.nxv8i16.i64(<vscale x 8 x i16>, <vscale x 8 x i16>, <vscale x 8 x i16>, <vscale x 8 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i16.i16.i64(<vscale x 8 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i32> @llvm.riscv.vmv.v.x.nxv4i32.i64(<vscale x 4 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i16> @llvm.riscv.vrsub.nxv8i16.i16.i64(<vscale x 8 x i16>, <vscale x 8 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i16> @llvm.riscv.vadd.nxv8i16.nxv8i16.i64(<vscale x 8 x i16>, <vscale x 8 x i16>, <vscale x 8 x i16>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i16> @llvm.riscv.vmerge.nxv16i16.nxv16i16.i64(<vscale x 16 x i16>, <vscale x 16 x i16>, <vscale x 16 x i16>, <vscale x 16 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i1> @llvm.riscv.vmsne.nxv16i16.i16.i64(<vscale x 16 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i32> @llvm.riscv.vmv.v.x.nxv8i32.i64(<vscale x 8 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i16> @llvm.riscv.vrsub.nxv16i16.i16.i64(<vscale x 16 x i16>, <vscale x 16 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i16> @llvm.riscv.vadd.nxv16i16.nxv16i16.i64(<vscale x 16 x i16>, <vscale x 16 x i16>, <vscale x 16 x i16>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 32 x i16> @llvm.riscv.vmerge.nxv32i16.nxv32i16.i64(<vscale x 32 x i16>, <vscale x 32 x i16>, <vscale x 32 x i16>, <vscale x 32 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 32 x i1> @llvm.riscv.vmsne.nxv32i16.i16.i64(<vscale x 32 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i32> @llvm.riscv.vmv.v.x.nxv16i32.i64(<vscale x 16 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 32 x i16> @llvm.riscv.vrsub.nxv32i16.i16.i64(<vscale x 32 x i16>, <vscale x 32 x i16>, i16, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 32 x i16> @llvm.riscv.vadd.nxv32i16.nxv32i16.i64(<vscale x 32 x i16>, <vscale x 32 x i16>, <vscale x 32 x i16>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i32> @llvm.riscv.vmerge.nxv1i32.nxv1i32.i64(<vscale x 1 x i32>, <vscale x 1 x i32>, <vscale x 1 x i32>, <vscale x 1 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmsne.nxv1i32.i32.i64(<vscale x 1 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind speculatable willreturn 
declare <vscale x 4 x i8> @llvm.vector.extract.nxv4i8.nxv8i8(<vscale x 8 x i8>, i64 immarg) #2

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vmv.v.x.nxv1i64.i64(<vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i32> @llvm.riscv.vrsub.nxv1i32.i32.i64(<vscale x 1 x i32>, <vscale x 1 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i32> @llvm.riscv.vadd.nxv1i32.nxv1i32.i64(<vscale x 1 x i32>, <vscale x 1 x i32>, <vscale x 1 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vmerge.nxv2i32.nxv2i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, <vscale x 2 x i32>, <vscale x 2 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmsne.nxv2i32.i32.i64(<vscale x 2 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vrsub.nxv2i32.i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i32> @llvm.riscv.vadd.nxv2i32.nxv2i32.i64(<vscale x 2 x i32>, <vscale x 2 x i32>, <vscale x 2 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i32> @llvm.riscv.vmerge.nxv4i32.nxv4i32.i64(<vscale x 4 x i32>, <vscale x 4 x i32>, <vscale x 4 x i32>, <vscale x 4 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i1> @llvm.riscv.vmsne.nxv4i32.i32.i64(<vscale x 4 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vmv.v.x.nxv2i64.i64(<vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i32> @llvm.riscv.vrsub.nxv4i32.i32.i64(<vscale x 4 x i32>, <vscale x 4 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i32> @llvm.riscv.vadd.nxv4i32.nxv4i32.i64(<vscale x 4 x i32>, <vscale x 4 x i32>, <vscale x 4 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i32> @llvm.riscv.vmerge.nxv8i32.nxv8i32.i64(<vscale x 8 x i32>, <vscale x 8 x i32>, <vscale x 8 x i32>, <vscale x 8 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i1> @llvm.riscv.vmsne.nxv8i32.i32.i64(<vscale x 8 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vmv.v.x.nxv4i64.i64(<vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i32> @llvm.riscv.vrsub.nxv8i32.i32.i64(<vscale x 8 x i32>, <vscale x 8 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i32> @llvm.riscv.vadd.nxv8i32.nxv8i32.i64(<vscale x 8 x i32>, <vscale x 8 x i32>, <vscale x 8 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i32> @llvm.riscv.vmerge.nxv16i32.nxv16i32.i64(<vscale x 16 x i32>, <vscale x 16 x i32>, <vscale x 16 x i32>, <vscale x 16 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i1> @llvm.riscv.vmsne.nxv16i32.i32.i64(<vscale x 16 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vmv.v.x.nxv8i64.i64(<vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i32> @llvm.riscv.vrsub.nxv16i32.i32.i64(<vscale x 16 x i32>, <vscale x 16 x i32>, i32, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 16 x i32> @llvm.riscv.vadd.nxv16i32.nxv16i32.i64(<vscale x 16 x i32>, <vscale x 16 x i32>, <vscale x 16 x i32>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vmerge.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i1> @llvm.riscv.vmseq.nxv1i64.i64.i64(<vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vand.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vid.nxv1i64.i64(<vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vrsub.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 1 x i64> @llvm.riscv.vadd.nxv1i64.nxv1i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, <vscale x 1 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vmerge.nxv2i64.nxv2i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, <vscale x 2 x i64>, <vscale x 2 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i1> @llvm.riscv.vmseq.nxv2i64.i64.i64(<vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vand.nxv2i64.i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vid.nxv2i64.i64(<vscale x 2 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vrsub.nxv2i64.i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 2 x i64> @llvm.riscv.vadd.nxv2i64.nxv2i64.i64(<vscale x 2 x i64>, <vscale x 2 x i64>, <vscale x 2 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vmerge.nxv4i64.nxv4i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, <vscale x 4 x i64>, <vscale x 4 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i1> @llvm.riscv.vmseq.nxv4i64.i64.i64(<vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vand.nxv4i64.i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vid.nxv4i64.i64(<vscale x 4 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vrsub.nxv4i64.i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 4 x i64> @llvm.riscv.vadd.nxv4i64.nxv4i64.i64(<vscale x 4 x i64>, <vscale x 4 x i64>, <vscale x 4 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vmerge.nxv8i64.nxv8i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, <vscale x 8 x i64>, <vscale x 8 x i1>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i64.i64.i64(<vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vand.nxv8i64.i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vid.nxv8i64.i64(<vscale x 8 x i64>, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vrsub.nxv8i64.i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, i64, i64) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn 
declare <vscale x 8 x i64> @llvm.riscv.vadd.nxv8i64.nxv8i64.i64(<vscale x 8 x i64>, <vscale x 8 x i64>, <vscale x 8 x i64>, i64) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn 
declare i64 @llvm.umin.i64(i64, i64) #4

attributes #0 = { mustprogress uwtable vscale_range(2,1024) "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="sifive-x280" "target-features"="+64bit,+a,+c,+d,+f,+m,+relax,+v,+zicsr,+zifencei,+zmmul,+zve32f,+zve32x,+zve64d,+zve64f,+zve64x,+zvl128b,+zvl32b,+zvl64b,-b,-e,-experimental-smmpm,-experimental-smnpm,-experimental-ssnpm,-experimental-sspm,-experimental-ssqosid,-experimental-supm,-experimental-zacas,-experimental-zalasr,-experimental-zicfilp,-experimental-zicfiss,-h,-shcounterenw,-shgatpa,-shtvala,-shvsatpa,-shvstvala,-shvstvecd,-smaia,-smcdeleg,-smcsrind,-smepmp,-smstateen,-ssaia,-ssccfg,-ssccptr,-sscofpmf,-sscounterenw,-sscsrind,-ssstateen,-ssstrict,-sstc,-sstvala,-sstvecd,-ssu64xl,-svade,-svadu,-svbare,-svinval,-svnapot,-svpbmt,-xcvalu,-xcvbi,-xcvbitmanip,-xcvelw,-xcvmac,-xcvmem,-xcvsimd,-xsfcease,-xsfvcp,-xsfvfnrclipxfqf,-xsfvfwmaccqqq,-xsfvqmaccdod,-xsfvqmaccqoq,-xsifivecdiscarddlone,-xsifivecflushdlone,-xtheadba,-xtheadbb,-xtheadbs,-xtheadcmo,-xtheadcondmov,-xtheadfmemidx,-xtheadmac,-xtheadmemidx,-xtheadmempair,-xtheadsync,-xtheadvdot,-xventanacondops,-xwchc,-za128rs,-za64rs,-zaamo,-zabha,-zalrsc,-zama16b,-zawrs,-zba,-zbb,-zbc,-zbkb,-zbkc,-zbkx,-zbs,-zca,-zcb,-zcd,-zce,-zcf,-zcmop,-zcmp,-zcmt,-zdinx,-zfa,-zfbfmin,-zfh,-zfhmin,-zfinx,-zhinx,-zhinxmin,-zic64b,-zicbom,-zicbop,-zicboz,-ziccamoa,-ziccif,-zicclsm,-ziccrse,-zicntr,-zicond,-zihintntl,-zihintpause,-zihpm,-zimop,-zk,-zkn,-zknd,-zkne,-zknh,-zkr,-zks,-zksed,-zksh,-zkt,-ztso,-zvbb,-zvbc,-zvfbfmin,-zvfbfwma,-zvfh,-zvfhmin,-zvkb,-zvkg,-zvkn,-zvknc,-zvkned,-zvkng,-zvknha,-zvknhb,-zvks,-zvksc,-zvksed,-zvksg,-zvksh,-zvkt,-zvl1024b,-zvl16384b,-zvl2048b,-zvl256b,-zvl32768b,-zvl4096b,-zvl512b,-zvl65536b,-zvl8192b" }
attributes #1 = { mustprogress nocallback nofree nosync nounwind willreturn  }
attributes #2 = { mustprogress nocallback nofree nosync nounwind speculatable willreturn  }
attributes #3 = { mustprogress nocallback nofree nosync nounwind willreturn  }
attributes #4 = { nocallback nofree nosync nounwind speculatable willreturn  }

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
!9 = !{!"vlenb"}
