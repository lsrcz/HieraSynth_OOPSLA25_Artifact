; RUN: converter %s | FileCheck %s

; Check vmv instructions are converted correctly

; CHECK: def vmv_v_v_i8m1(src: vec<ef8m1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmv_v_v_i8m1(<vscale x 8 x i8> %src, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmv.v.v[ef8m1, ud]([[VL]], src)
  %3 = tail call <vscale x 8 x i8> @llvm.riscv.vmv.v.v.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %src, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %3
}

declare <vscale x 8 x i8> @llvm.riscv.vmv.v.v.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i64) #2

; CHECK: def vmv_v_v_i8m1_tu(dest: vec<ef8m1>, src: vec<ef8m1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmv_v_v_i8m1_tu(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmv.v.v[ef8m1, pd]([[VL]], src, dest)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vmv.v.v.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %4
}

; CHECK: def vmv_v_x_i8m1(src: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmv_v_x_i8m1(i8 noundef signext %src, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmv.v.x[ef8m1, ud]([[VL]], src)
  %3 = tail call <vscale x 8 x i8> @llvm.riscv.vmv.v.x.nxv8i8.i64(<vscale x 8 x i8> poison, i8 %src, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %3
}

declare <vscale x 8 x i8> @llvm.riscv.vmv.v.x.nxv8i8.i64(<vscale x 8 x i8>, i8, i64) #2

; CHECK: def vmv_v_x_i8m1_tu(dest: vec<ef8m1>, src: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmv_v_x_i8m1_tu(<vscale x 8 x i8> %dest, i8 noundef signext %src, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmv.v.x[ef8m1, pd]([[VL]], src, dest)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vmv.v.x.nxv8i8.i64(<vscale x 8 x i8> %dest, i8 %src, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %4
}

; CHECK: def vmv_s_x_i8m1(src: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmv_s_x_i8m1(i8 noundef signext %src, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmv.s.x[ef8m1, ud]([[VL]], src)
  %3 = tail call <vscale x 8 x i8> @llvm.riscv.vmv.s.x.nxv8i8.i64(<vscale x 8 x i8> poison, i8 %src, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %3
}

declare <vscale x 8 x i8> @llvm.riscv.vmv.s.x.nxv8i8.i64(<vscale x 8 x i8>, i8, i64) #2

; CHECK: def vmv_s_x_i8m1_tu(dest: vec<ef8m1>, src: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vmv_s_x_i8m1_tu(<vscale x 8 x i8> %dest, i8 noundef signext %src, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmv.s.x[ef8m1, pd]([[VL]], src, dest)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vmv.s.x.nxv8i8.i64(<vscale x 8 x i8> %dest, i8 %src, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %4
}

; CHECK: def vmv_x_s_i8m1_i8(src: vec<ef8m1>) -> scalar<xmul=f8>:
define signext i8 @vmv_x_s_i8m1_i8(<vscale x 8 x i8> %src) {
  ; CHECK-NEXT: ([[RES:.*]]) = vmv.x.s[ef8m1](src)
  %2 = tail call i8 @llvm.riscv.vmv.x.s.nxv8i8(<vscale x 8 x i8> %src)
  ; CHECK-NEXT: return ([[RES]])
  ret i8 %2
}

declare i8 @llvm.riscv.vmv.x.s.nxv8i8(<vscale x 8 x i8>) #2
