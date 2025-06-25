; RUN: converter %s | FileCheck %s

; Check narrowing shift right instructions are converted correctly

; CHECK: def vnsrl_wv_u16m1(vec: vec<ef2m2>, shift: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnsrl_wv_u16m1(<vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnsrl.wv[narrow_vtype=ef4m1, ud, fm]([[VL]], vec, shift)
  %4 = tail call <vscale x 4 x i16> @llvm.riscv.vnsrl.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %4
}

declare <vscale x 4 x i16> @llvm.riscv.vnsrl.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16>, <vscale x 4 x i32>, <vscale x 4 x i16>, i64) #1

; CHECK: def vnsrl_wv_u16m1_m(mask: mask<mmul=4>, vec: vec<ef2m2>, shift: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnsrl_wv_u16m1_m(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnsrl.wv[narrow_vtype=ef4m1, ud, pm]([[VL]], vec, shift, mask)
  %5 = tail call <vscale x 4 x i16> @llvm.riscv.vnsrl.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, <vscale x 4 x i1> %mask, i64 %avl, i64 3)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %5
}

declare <vscale x 4 x i16> @llvm.riscv.vnsrl.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16>, <vscale x 4 x i32>, <vscale x 4 x i16>, <vscale x 4 x i1>, i64, i64 immarg) #1

; CHECK: def vnsrl_wv_u16m1_mu(mask: mask<mmul=4>, dest: vec<ef4m1>, vec: vec<ef2m2>, shift: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnsrl_wv_u16m1_mu(<vscale x 4 x i1> %mask, <vscale x 4 x i16> %dest, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnsrl.wv[narrow_vtype=ef4m1, pd, pm]([[VL]], vec, shift, dest, mask)
  %6 = tail call <vscale x 4 x i16> @llvm.riscv.vnsrl.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, <vscale x 4 x i1> %mask, i64 %avl, i64 1)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %6
}

; CHECK: def vnsrl_wv_u16m1_tu(dest: vec<ef4m1>, vec: vec<ef2m2>, shift: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnsrl_wv_u16m1_tu(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnsrl.wv[narrow_vtype=ef4m1, pd, fm]([[VL]], vec, shift, dest)
  %5 = tail call <vscale x 4 x i16> @llvm.riscv.vnsrl.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %5
}

; CHECK: def vnsrl_wv_u16m1_tum(mask: mask<mmul=4>, dest: vec<ef4m1>, vec: vec<ef2m2>, shift: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnsrl_wv_u16m1_tum(<vscale x 4 x i1> %mask, <vscale x 4 x i16> %dest, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnsrl.wv[narrow_vtype=ef4m1, pd, pm]([[VL]], vec, shift, dest, mask)
  %6 = tail call <vscale x 4 x i16> @llvm.riscv.vnsrl.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, <vscale x 4 x i1> %mask, i64 %avl, i64 2)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %6
}

; CHECK: def vnsrl_wv_u16m1_tumu(mask: mask<mmul=4>, dest: vec<ef4m1>, vec: vec<ef2m2>, shift: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnsrl_wv_u16m1_tumu(<vscale x 4 x i1> %mask, <vscale x 4 x i16> %dest, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnsrl.wv[narrow_vtype=ef4m1, pd, pm]([[VL]], vec, shift, dest, mask)
  %6 = tail call <vscale x 4 x i16> @llvm.riscv.vnsrl.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %vec, <vscale x 4 x i16> %shift, <vscale x 4 x i1> %mask, i64 %avl, i64 0)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %6
}

; CHECK: def vnsrl_wx_u16m1(vec: vec<ef2m2>, shift: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnsrl_wx_u16m1(<vscale x 4 x i32> %vec, i64 noundef %shift, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnsrl.wx[narrow_vtype=ef4m1, ud, fm]([[VL]], vec, shift)
  %4 = tail call <vscale x 4 x i16> @llvm.riscv.vnsrl.nxv4i16.nxv4i32.i64.i64(<vscale x 4 x i16> poison, <vscale x 4 x i32> %vec, i64 %shift, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %4
}

declare <vscale x 4 x i16> @llvm.riscv.vnsrl.nxv4i16.nxv4i32.i64.i64(<vscale x 4 x i16>, <vscale x 4 x i32>, i64, i64) #1
