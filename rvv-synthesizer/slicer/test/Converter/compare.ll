; RUN: converter %s | FileCheck %s

; Check compare instructions are converted correctly

; CHECK: def vmseq_e8m1_e8m1(lhs: vec<ef8m1>, rhs: vec<ef8m1>, avl: scalar<xmul=1>) -> mask<mmul=8>:
define <vscale x 8 x i1> @vmseq_e8m1_e8m1(<vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmseq.vv[ef8m1, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i1> %4 
}

; CHECK: def vmseq_e8m1_e8m1_m(lhs: vec<ef8m1>, rhs: vec<ef8m1>, mask: mask<mmul=8>, avl: scalar<xmul=1>) -> mask<mmul=8>:
define <vscale x 8 x i1> @vmseq_e8m1_e8m1_m(<vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, <vscale x 8 x i1> %mask, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmseq.vv[ef8m1, ud, pm]([[VL]], lhs, rhs, mask)
  %1 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.mask.nxv8i8.nxv8i8.i64(<vscale x 8 x i1> poison, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, <vscale x 8 x i1> %mask, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i1> %1 
}

; CHECK: def vmseq_e8m1_e8m1_mu(dest: mask<mmul=8>, lhs: vec<ef8m1>, rhs: vec<ef8m1>, mask: mask<mmul=8>, avl: scalar<xmul=1>) -> mask<mmul=8>:
define <vscale x 8 x i1> @vmseq_e8m1_e8m1_mu(<vscale x 8 x i1> %dest, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, <vscale x 8 x i1> %mask, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmseq.vv[ef8m1, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %1 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.mask.nxv8i8.nxv8i8.i64(<vscale x 8 x i1> %dest, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, <vscale x 8 x i1> %mask, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i1> %1 
}

; CHECK: def vmseq_e8m1_i8(lhs: vec<ef8m1>, rhs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> mask<mmul=8>:
define <vscale x 8 x i1> @vmseq_e8m1_i8(<vscale x 8 x i8> %lhs, i8 %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmseq.vx[ef8m1, ud, fm]([[VL]], lhs, rhs)
  %4 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i8.i8.i64(<vscale x 8 x i8> %lhs, i8 %rhs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i1> %4 
}

; CHECK: def vmseq_e8m1_i8_m(lhs: vec<ef8m1>, rhs: scalar<xmul=f8>, mask: mask<mmul=8>, avl: scalar<xmul=1>) -> mask<mmul=8>:
define <vscale x 8 x i1> @vmseq_e8m1_i8_m(<vscale x 8 x i8> %lhs, i8 %rhs, <vscale x 8 x i1> %mask, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmseq.vx[ef8m1, ud, pm]([[VL]], lhs, rhs, mask)
  %1 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.mask.nxv8i8.i8.i64(<vscale x 8 x i1> poison, <vscale x 8 x i8> %lhs, i8 %rhs, <vscale x 8 x i1> %mask, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i1> %1 
}

; CHECK: def vmseq_e8m1_i8_mu(dest: mask<mmul=8>, lhs: vec<ef8m1>, rhs: scalar<xmul=f8>, mask: mask<mmul=8>, avl: scalar<xmul=1>) -> mask<mmul=8>:
define <vscale x 8 x i1> @vmseq_e8m1_i8_mu(<vscale x 8 x i1> %dest, <vscale x 8 x i8> %lhs, i8 %rhs, <vscale x 8 x i1> %mask, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmseq.vx[ef8m1, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %1 = tail call <vscale x 8 x i1> @llvm.riscv.vmseq.mask.nxv8i8.i8.i64(<vscale x 8 x i1> %dest, <vscale x 8 x i8> %lhs, i8 %rhs, <vscale x 8 x i1> %mask, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i1> %1 
}

declare <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i8.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i64)

declare <vscale x 8 x i1> @llvm.riscv.vmseq.nxv8i8.i8.i64(<vscale x 8 x i8>, i8, i64)

declare <vscale x 8 x i1> @llvm.riscv.vmseq.mask.nxv8i8.nxv8i8.i64(<vscale x 8 x i1>, <vscale x 8 x i8>, <vscale x 8 x i8>, <vscale x 8 x i1>, i64)

declare <vscale x 8 x i1> @llvm.riscv.vmseq.mask.nxv8i8.i8.i64(<vscale x 8 x i1>, <vscale x 8 x i8>, i8, <vscale x 8 x i1>, i64)
