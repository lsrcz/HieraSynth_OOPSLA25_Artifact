; RUN: converter %s | FileCheck %s

; check that vmerge is converted correctly

; CHECK: def vmerge_e8m1_e8m1(l: vec<ef8m1>, r: vec<ef8m1>, m: mask<mmul=8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmerge_e8m1_e8m1(<vscale x 8 x i8> %l, <vscale x 8 x i8> %r, <vscale x 8 x i1> %m, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmerge.vvm[ef8m1, ud]([[VL]], l, r, m)
  %1 = tail call <vscale x 8 x i8> @llvm.riscv.vmerge.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %l, <vscale x 8 x i8> %r, <vscale x 8 x i1> %m, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %1 
}

; CHECK: def vmerge_e8m1_e8m1_tu(dest: vec<ef8m1>, l: vec<ef8m1>, r: vec<ef8m1>, m: mask<mmul=8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmerge_e8m1_e8m1_tu(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %l, <vscale x 8 x i8> %r, <vscale x 8 x i1> %m, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmerge.vvm[ef8m1, pd]([[VL]], l, r, dest, m)
  %1 = tail call <vscale x 8 x i8> @llvm.riscv.vmerge.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %l, <vscale x 8 x i8> %r, <vscale x 8 x i1> %m, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %1 
}

; CHECK: def vmerge_e8m1_i8(l: vec<ef8m1>, r: scalar<xmul=f8>, m: mask<mmul=8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmerge_e8m1_i8(<vscale x 8 x i8> %l, i8 %r, <vscale x 8 x i1> %m, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmerge.vxm[ef8m1, ud]([[VL]], l, r, m)
  %1 = tail call <vscale x 8 x i8> @llvm.riscv.vmerge.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %l, i8 %r, <vscale x 8 x i1> %m, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %1 
}

; CHECK: def vmerge_e8m1_i8_tu(dest: vec<ef8m1>, l: vec<ef8m1>, r: scalar<xmul=f8>, m: mask<mmul=8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmerge_e8m1_i8_tu(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %l, i8 %r, <vscale x 8 x i1> %m, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmerge.vxm[ef8m1, pd]([[VL]], l, r, dest, m)
  %1 = tail call <vscale x 8 x i8> @llvm.riscv.vmerge.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %l, i8 %r, <vscale x 8 x i1> %m, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %1 
}

declare <vscale x 8 x i8> @llvm.riscv.vmerge.nxv8i8.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, <vscale x 8 x i8>, <vscale x 8 x i1>, i64)

declare <vscale x 8 x i8> @llvm.riscv.vmerge.nxv8i8.i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i8, <vscale x 8 x i1>, i64)
