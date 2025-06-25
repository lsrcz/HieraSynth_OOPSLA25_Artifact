; RUN: converter %s | FileCheck %s

; Check vset instructions are converted correctly

; CHECK: def vset_i8m1_i8m4_0(dest: vec<ef8m4>, part: vec<ef8m1>) -> vec<ef8m4>:
define <vscale x 32 x i8> @vset_i8m1_i8m4_0(<vscale x 32 x i8> %dest, <vscale x 8 x i8> %part) {
  ; CHECK-NEXT: ([[RES:.*]]) = vset[part=ef8m1, dest=ef8m4, pd, idx=0](part, dest)
  %3 = tail call <vscale x 32 x i8> @llvm.vector.insert.nxv32i8.nxv8i8(<vscale x 32 x i8> %dest, <vscale x 8 x i8> %part, i64 0)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 32 x i8> %3
}

declare <vscale x 32 x i8> @llvm.vector.insert.nxv32i8.nxv8i8(<vscale x 32 x i8>, <vscale x 8 x i8>, i64 immarg) #1

; CHECK: def vset_i8m1_i8m4_1(dest: vec<ef8m4>, part: vec<ef8m1>) -> vec<ef8m4>:
define <vscale x 32 x i8> @vset_i8m1_i8m4_1(<vscale x 32 x i8> %dest, <vscale x 8 x i8> %part) {
  ; CHECK-NEXT: ([[RES:.*]]) = vset[part=ef8m1, dest=ef8m4, pd, idx=1](part, dest)
  %3 = tail call <vscale x 32 x i8> @llvm.vector.insert.nxv32i8.nxv8i8(<vscale x 32 x i8> %dest, <vscale x 8 x i8> %part, i64 8)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 32 x i8> %3
}

; CHECK: def vset_i16m2_i16m8_2(dest: vec<ef4m8>, part: vec<ef4m2>) -> vec<ef4m8>:
define <vscale x 32 x i16> @vset_i16m2_i16m8_2(<vscale x 32 x i16> %dest, <vscale x 8 x i16> %part) {
  ; CHECK-NEXT: ([[RES:.*]]) = vset[part=ef4m2, dest=ef4m8, pd, idx=2](part, dest)
  %3 = tail call <vscale x 32 x i16> @llvm.vector.insert.nxv32i16.nxv8i16(<vscale x 32 x i16> %dest, <vscale x 8 x i16> %part, i64 16)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 32 x i16> %3
}

declare <vscale x 32 x i16> @llvm.vector.insert.nxv32i16.nxv8i16(<vscale x 32 x i16>, <vscale x 8 x i16>, i64 immarg) #1

; CHECK: def vset_i16m1_i16m2_1(dest: vec<ef4m2>, part: vec<ef4m1>) -> vec<ef4m2>:
define <vscale x 8 x i16> @vset_i16m1_i16m2_1(<vscale x 8 x i16> %dest, <vscale x 4 x i16> %part) {
  ; CHECK-NEXT: ([[RES:.*]]) = vset[part=ef4m1, dest=ef4m2, pd, idx=1](part, dest)
  %3 = tail call <vscale x 8 x i16> @llvm.vector.insert.nxv8i16.nxv4i16(<vscale x 8 x i16> %dest, <vscale x 4 x i16> %part, i64 4)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i16> %3
}

declare <vscale x 8 x i16> @llvm.vector.insert.nxv8i16.nxv4i16(<vscale x 8 x i16>, <vscale x 4 x i16>, i64 immarg) #1

; CHECK: def vlmul_ext_i16mf2_i16m2(part: vec<ef4mf2>) -> vec<ef4m2>:
define <vscale x 8 x i16> @vlmul_ext_i16mf2_i16m2(<vscale x 2 x i16> %part) {
  ; CHECK-NEXT: ([[RES:.*]]) = vset[part=ef4mf2, dest=ef4m2, ud, idx=0](part)
  %2 = tail call <vscale x 8 x i16> @llvm.vector.insert.nxv8i16.nxv2i16(<vscale x 8 x i16> poison, <vscale x 2 x i16> %part, i64 0)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i16> %2
}

declare <vscale x 8 x i16> @llvm.vector.insert.nxv8i16.nxv2i16(<vscale x 8 x i16>, <vscale x 2 x i16>, i64 immarg) #1

; CHECK: def vset_b8_b2_3_non_rvv(dest: mask<mmul=32>, part: mask<mmul=8>) -> mask<mmul=32>:
define <vscale x 32 x i1> @vset_b8_b2_3_non_rvv(<vscale x 32 x i1> %dest, <vscale x 8 x i1> %part) {
  ; CHECK-NEXT: ([[RES:.*]]) = insert_mask[part=8, dest=32, pd, idx=3](part, dest)
  %3 = tail call <vscale x 32 x i1> @llvm.vector.insert.nxv32i1.nxv8i1(<vscale x 32 x i1> %dest, <vscale x 8 x i1> %part, i64 24)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 32 x i1> %3
}

declare <vscale x 32 x i1> @llvm.vector.insert.nxv32i1.nxv8i1(<vscale x 32 x i1>, <vscale x 8 x i1>, i64 immarg) #1
