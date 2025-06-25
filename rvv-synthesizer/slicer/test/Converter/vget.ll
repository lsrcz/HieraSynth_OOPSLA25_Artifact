; RUN: converter %s | FileCheck %s

; Check vget instructions are converted correctly

; CHECK: def vget_i8m4_i8m1_0(src: vec<ef8m4>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vget_i8m4_i8m1_0(<vscale x 32 x i8> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vget[part=ef8m1, src=ef8m4, idx=0](src)
  %2 = tail call <vscale x 8 x i8> @llvm.vector.extract.nxv8i8.nxv32i8(<vscale x 32 x i8> %src, i64 0)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %2
}

; CHECK: def vget_i8m4_i8m1_1(src: vec<ef8m4>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vget_i8m4_i8m1_1(<vscale x 32 x i8> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vget[part=ef8m1, src=ef8m4, idx=1](src)
  %2 = tail call <vscale x 8 x i8> @llvm.vector.extract.nxv8i8.nxv32i8(<vscale x 32 x i8> %src, i64 8)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %2
}

; CHECK: def vget_i16m8_i16m2_1(src: vec<ef4m8>) -> vec<ef4m2>:
define <vscale x 8 x i16> @vget_i16m8_i16m2_1(<vscale x 32 x i16> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vget[part=ef4m2, src=ef4m8, idx=1](src)
  %2 = tail call <vscale x 8 x i16> @llvm.vector.extract.nxv8i16.nxv32i16(<vscale x 32 x i16> %src, i64 8)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i16> %2
}

; CHECK: def vget_i16m8_i16m1_6(src: vec<ef4m8>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vget_i16m8_i16m1_6(<vscale x 32 x i16> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vget[part=ef4m1, src=ef4m8, idx=6](src)
  %2 = tail call <vscale x 4 x i16> @llvm.vector.extract.nxv4i16.nxv32i16(<vscale x 32 x i16> %src, i64 24)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %2
}

; CHECK: def vget_i16m2_i16m1_1(src: vec<ef4m2>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vget_i16m2_i16m1_1(<vscale x 8 x i16> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vget[part=ef4m1, src=ef4m2, idx=1](src)
  %2 = tail call <vscale x 4 x i16> @llvm.vector.extract.nxv4i16.nxv8i16(<vscale x 8 x i16> %src, i64 4)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %2
}

; CHECK: def vlmul_trunc_i16m2_i16mf2(src: vec<ef4m2>) -> vec<ef4mf2>:
define <vscale x 2 x i16> @vlmul_trunc_i16m2_i16mf2(<vscale x 8 x i16> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vget[part=ef4mf2, src=ef4m2, idx=0](src)
  %2 = tail call <vscale x 2 x i16> @llvm.vector.extract.nxv2i16.nxv8i16(<vscale x 8 x i16> %src, i64 0)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 2 x i16> %2
}

; CHECK: def vget_b2_b8_3_non_rvv(src: mask<mmul=32>) -> mask<mmul=8>:
define <vscale x 8 x i1> @vget_b2_b8_3_non_rvv(<vscale x 32 x i1> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = extract_mask[part=8, src=32, idx=3](src)
  %2 = tail call <vscale x 8 x i1> @llvm.vector.extract.nxv32i1.nxv8i1(<vscale x 32 x i1> %src, i64 24)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i1> %2
}

declare <vscale x 8 x i8> @llvm.vector.extract.nxv8i8.nxv32i8(<vscale x 32 x i8>, i64 immarg) #2

declare <vscale x 8 x i16> @llvm.vector.extract.nxv8i16.nxv32i16(<vscale x 32 x i16>, i64 immarg) #2

declare <vscale x 4 x i16> @llvm.vector.extract.nxv4i16.nxv32i16(<vscale x 32 x i16>, i64 immarg) #2

declare <vscale x 4 x i16> @llvm.vector.extract.nxv4i16.nxv8i16(<vscale x 8 x i16>, i64 immarg) #2

declare <vscale x 2 x i16> @llvm.vector.extract.nxv2i16.nxv8i16(<vscale x 8 x i16>, i64 immarg) #2

declare <vscale x 8 x i1> @llvm.vector.extract.nxv32i1.nxv8i1(<vscale x 32 x i1>, i64 immarg) #2