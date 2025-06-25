; RUN: converter %s | FileCheck %s

; Check vreinterpret instructions are converted correctly

; CHECK: def vreinterpret_i16m1_i8m1(src: vec<ef4m1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vreinterpret_i16m1_i8m1(<vscale x 4 x i16> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vec_to_vec[src=ef4m1, dest=ef8m1](src)
  %2 = bitcast <vscale x 4 x i16> %src to <vscale x 8 x i8>
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %2
}

; CHECK: def vreinterpret_i16m1_b1(src: vec<ef4m1>) -> mask<mmul=64>:
define <vscale x 64 x i1> @vreinterpret_i16m1_b1(<vscale x 4 x i16> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vec_to_mask[src=ef4m1, mmul=64](src)
  %2 = bitcast <vscale x 4 x i16> %src to <vscale x 64 x i1>
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 64 x i1> %2
}

; CHECK: def vreinterpret_i16m1_b2(src: vec<ef4m1>) -> mask<mmul=32>:
define <vscale x 32 x i1> @vreinterpret_i16m1_b2(<vscale x 4 x i16> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vec_to_mask[src=ef4m1, mmul=64](src)
  %2 = bitcast <vscale x 4 x i16> %src to <vscale x 64 x i1>
  ; CHECK-NEXT: ([[RES2:.*]]) = extract_mask[part=32, src=64, idx=0]([[RES]])
  %3 = tail call <vscale x 32 x i1> @llvm.vector.extract.nxv32i1.nxv64i1(<vscale x 64 x i1> %2, i64 0)
  ; CHECK-NEXT: return ([[RES2]])
  ret <vscale x 32 x i1> %3
}

; CHECK: def vreinterpret_b1_i16m1(src: mask<mmul=64>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vreinterpret_b1_i16m1(<vscale x 64 x i1> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = mask_to_vec[mmul=64, dest=ef4m1](src)
  %2 = bitcast <vscale x 64 x i1> %src to <vscale x 4 x i16>
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %2
}

; CHECK: def vreinterpret_b2_i16m1(src: mask<mmul=32>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vreinterpret_b2_i16m1(<vscale x 32 x i1> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = insert_mask[part=32, dest=64, ud, idx=0](src)
  %2 = tail call <vscale x 64 x i1> @llvm.vector.insert.nxv64i1.nxv32i1(<vscale x 64 x i1> poison, <vscale x 32 x i1> %src, i64 0)
  ; CHECK-NEXT: ([[RES2:.*]]) = mask_to_vec[mmul=64, dest=ef4m1]([[RES]])
  %3 = bitcast <vscale x 64 x i1> %2 to <vscale x 4 x i16>
  ; CHECK-NEXT: return ([[RES2]])
  ret <vscale x 4 x i16> %3
}

declare <vscale x 32 x i1> @llvm.vector.extract.nxv32i1.nxv64i1(<vscale x 64 x i1>, i64 immarg) #2

declare <vscale x 64 x i1> @llvm.vector.insert.nxv64i1.nxv64i1(<vscale x 64 x i1>, <vscale x 64 x i1>, i64 immarg) #2

declare <vscale x 64 x i1> @llvm.vector.insert.nxv64i1.nxv32i1(<vscale x 64 x i1>, <vscale x 32 x i1>, i64 immarg) #2
