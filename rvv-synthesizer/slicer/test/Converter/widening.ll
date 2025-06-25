; RUN: converter %s | FileCheck %s

; Check widening vector instructions are converted correctly

; CHECK: def vwaddu_wv_u32m2(lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_wv_u32m2(<vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.wv[wide=ef2m2, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.w.nxv4i32.nxv4i16.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %4
}

declare <vscale x 4 x i32> @llvm.riscv.vwaddu.w.nxv4i32.nxv4i16.i64(<vscale x 4 x i32>, <vscale x 4 x i32>, <vscale x 4 x i16>, i64) #1

; CHECK: def vwaddu_wv_u32m2_tu(dest: vec<ef2m2>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_wv_u32m2_tu(<vscale x 4 x i32> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.wv[wide=ef2m2, pd, fm]([[VL]], lhs, rhs, dest)
  %5 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.w.nxv4i32.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %5
}

; CHECK: def vwaddu_wv_u32m2_m(mask: mask<mmul=4>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_wv_u32m2_m(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.wv[wide=ef2m2, ud, pm]([[VL]], lhs, rhs, mask)
  %5 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.w.mask.nxv4i32.nxv4i16.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 3)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %5
}

declare <vscale x 4 x i32> @llvm.riscv.vwaddu.w.mask.nxv4i32.nxv4i16.i64(<vscale x 4 x i32>, <vscale x 4 x i32>, <vscale x 4 x i16>, <vscale x 4 x i1>, i64, i64 immarg) #1

; CHECK: def vwaddu_wv_u32m2_mu(mask: mask<mmul=4>, dest: vec<ef2m2>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_wv_u32m2_mu(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.wv[wide=ef2m2, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.w.mask.nxv4i32.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 1)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %6
}

; CHECK: def vwaddu_wv_u32m2_tum(mask: mask<mmul=4>, dest: vec<ef2m2>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_wv_u32m2_tum(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.wv[wide=ef2m2, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.w.mask.nxv4i32.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 2)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %6
}

; CHECK: def vwaddu_wv_u32m2_tumu(mask: mask<mmul=4>, dest: vec<ef2m2>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_wv_u32m2_tumu(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.wv[wide=ef2m2, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.w.mask.nxv4i32.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 0)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %6
}

; CHECK: def vwaddu_wx_u32m2(lhs: vec<ef2m2>, rhs: scalar<xmul=f4>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_wx_u32m2(<vscale x 4 x i32> %lhs, i16 noundef zeroext %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.wx[wide=ef2m2, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.w.nxv4i32.i16.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %lhs, i16 %rhs, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %4
}

declare <vscale x 4 x i32> @llvm.riscv.vwaddu.w.nxv4i32.i16.i64(<vscale x 4 x i32>, <vscale x 4 x i32>, i16, i64) #1

; CHECK: def vwaddu_wx_u32m2_m(mask: mask<mmul=4>, lhs: vec<ef2m2>, rhs: scalar<xmul=f4>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_wx_u32m2_m(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %lhs, i16 noundef zeroext %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.wx[wide=ef2m2, ud, pm]([[VL]], lhs, rhs, mask)
  %5 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.w.mask.nxv4i32.i16.i64(<vscale x 4 x i32> poison, <vscale x 4 x i32> %lhs, i16 %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 3)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %5
}

declare <vscale x 4 x i32> @llvm.riscv.vwaddu.w.mask.nxv4i32.i16.i64(<vscale x 4 x i32>, <vscale x 4 x i32>, i16, <vscale x 4 x i1>, i64, i64 immarg) #1

; CHECK: def vwaddu_vv_u32m2(lhs: vec<ef4m1>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_vv_u32m2(<vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.vv[wide=ef2m2, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32> poison, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %4
}

declare <vscale x 4 x i32> @llvm.riscv.vwaddu.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32>, <vscale x 4 x i16>, <vscale x 4 x i16>, i64) #1

; CHECK: def vwaddu_vv_u32m2_m(mask: mask<mmul=4>, lhs: vec<ef4m1>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_vv_u32m2_m(<vscale x 4 x i1> %mask, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.vv[wide=ef2m2, ud, pm]([[VL]], lhs, rhs, mask)
  %5 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.mask.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32> poison, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 3)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %5
}

declare <vscale x 4 x i32> @llvm.riscv.vwaddu.mask.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32>, <vscale x 4 x i16>, <vscale x 4 x i16>, <vscale x 4 x i1>, i64, i64 immarg) #1

; CHECK: def vwaddu_vx_u32m2(lhs: vec<ef4m1>, rhs: scalar<xmul=f4>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_vx_u32m2(<vscale x 4 x i16> %lhs, i16 noundef zeroext %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.vx[wide=ef2m2, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.nxv4i32.nxv4i16.i16.i64(<vscale x 4 x i32> poison, <vscale x 4 x i16> %lhs, i16 %rhs, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %4
}

declare <vscale x 4 x i32> @llvm.riscv.vwaddu.nxv4i32.nxv4i16.i16.i64(<vscale x 4 x i32>, <vscale x 4 x i16>, i16, i64) #1

; CHECK: def vwaddu_vx_u32m2_m(mask: mask<mmul=4>, lhs: vec<ef4m1>, rhs: scalar<xmul=f4>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwaddu_vx_u32m2_m(<vscale x 4 x i1> %mask, <vscale x 4 x i16> %lhs, i16 noundef zeroext %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwaddu.vx[wide=ef2m2, ud, pm]([[VL]], lhs, rhs, mask)
  %5 = tail call <vscale x 4 x i32> @llvm.riscv.vwaddu.mask.nxv4i32.nxv4i16.i16.i64(<vscale x 4 x i32> poison, <vscale x 4 x i16> %lhs, i16 %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 3)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %5
}

declare <vscale x 4 x i32> @llvm.riscv.vwaddu.mask.nxv4i32.nxv4i16.i16.i64(<vscale x 4 x i32>, <vscale x 4 x i16>, i16, <vscale x 4 x i1>, i64, i64 immarg) #1
