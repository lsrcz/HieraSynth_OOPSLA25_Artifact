; RUN: converter %s | FileCheck %s

; Check fixed point clip instructions are converted correctly

; CHECK: def vnclipu_wv_u16m1_rnu(lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclipu_wv_u16m1_rnu(<vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclipu.rnu.wv[narrow_vtype=ef4m1, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 4 x i16> @llvm.riscv.vnclipu.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 0, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %4
}

declare <vscale x 4 x i16> @llvm.riscv.vnclipu.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16>, <vscale x 4 x i32>, <vscale x 4 x i16>, i64 immarg, i64) #1

; CHECK: def vnclip_wv_u16m1_rne(lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclip_wv_u16m1_rne(<vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclip.rne.wv[narrow_vtype=ef4m1, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 4 x i16> @llvm.riscv.vnclip.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 1, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %4
}

declare <vscale x 4 x i16> @llvm.riscv.vnclip.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16>, <vscale x 4 x i32>, <vscale x 4 x i16>, i64 immarg, i64) #1

; CHECK: def vnclipu_wv_u16m1_rdn(lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclipu_wv_u16m1_rdn(<vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclipu.rdn.wv[narrow_vtype=ef4m1, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 4 x i16> @llvm.riscv.vnclipu.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 2, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %4
}

; CHECK: def vnclipu_wv_u16m1_rod(lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclipu_wv_u16m1_rod(<vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclipu.rod.wv[narrow_vtype=ef4m1, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 4 x i16> @llvm.riscv.vnclipu.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 3, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %4
}

; CHECK: def vnclipu_wv_u16m1_m(mask: mask<mmul=4>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclipu_wv_u16m1_m(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclipu.rnu.wv[narrow_vtype=ef4m1, ud, pm]([[VL]], lhs, rhs, mask)
  %5 = tail call <vscale x 4 x i16> @llvm.riscv.vnclipu.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> poison, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 0, i64 %avl, i64 3)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %5
}

declare <vscale x 4 x i16> @llvm.riscv.vnclipu.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16>, <vscale x 4 x i32>, <vscale x 4 x i16>, <vscale x 4 x i1>, i64 immarg, i64, i64 immarg) #1

; CHECK: def vnclipu_wv_u16m1_mu(mask: mask<mmul=4>, dest: vec<ef4m1>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclipu_wv_u16m1_mu(<vscale x 4 x i1> %mask, <vscale x 4 x i16> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclipu.rne.wv[narrow_vtype=ef4m1, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i16> @llvm.riscv.vnclipu.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 1, i64 %avl, i64 1)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %6
}

; CHECK: def vnclipu_wv_u16m1_tu(dest: vec<ef4m1>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclipu_wv_u16m1_tu(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclipu.rnu.wv[narrow_vtype=ef4m1, pd, fm]([[VL]], lhs, rhs, dest)
  %5 = tail call <vscale x 4 x i16> @llvm.riscv.vnclipu.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 0, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %5
}

; CHECK: def vnclipu_wv_u16m1_tum(mask: mask<mmul=4>, dest: vec<ef4m1>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclipu_wv_u16m1_tum(<vscale x 4 x i1> %mask, <vscale x 4 x i16> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclipu.rnu.wv[narrow_vtype=ef4m1, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i16> @llvm.riscv.vnclipu.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 0, i64 %avl, i64 2)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %6
}

; CHECK: def vnclipu_wv_u16m1_tumu(mask: mask<mmul=4>, dest: vec<ef4m1>, lhs: vec<ef2m2>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclipu_wv_u16m1_tumu(<vscale x 4 x i1> %mask, <vscale x 4 x i16> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclipu.rne.wv[narrow_vtype=ef4m1, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i16> @llvm.riscv.vnclipu.mask.nxv4i16.nxv4i32.nxv4i16.i64(<vscale x 4 x i16> %dest, <vscale x 4 x i32> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 1, i64 %avl, i64 0)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %6
}

; CHECK: def vnclipu_wx_u16m1(lhs: vec<ef2m2>, rhs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef4m1>:
define <vscale x 4 x i16> @vnclipu_wx_u16m1(<vscale x 4 x i32> %lhs, i64 noundef %rhs, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vnclipu.rnu.wx[narrow_vtype=ef4m1, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 4 x i16> @llvm.riscv.vnclipu.nxv4i16.nxv4i32.i64.i64(<vscale x 4 x i16> poison, <vscale x 4 x i32> %lhs, i64 %rhs, i64 0, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i16> %4
}

declare <vscale x 4 x i16> @llvm.riscv.vnclipu.nxv4i16.nxv4i32.i64.i64(<vscale x 4 x i16>, <vscale x 4 x i32>, i64, i64 immarg, i64) #1
