; RUN: converter %s | FileCheck %s

; Check single width vector instructions are converted correctly

; CHECK: def vadd_e8m1_e8m1(lhs: vec<ef8m1>, rhs: vec<ef8m1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vadd_e8m1_e8m1(<vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vv[ef8m1, ud, fm]([[VL]], lhs, rhs)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vadd_e8m1_e8m1_tu(dest: vec<ef8m1>, lhs: vec<ef8m1>, rhs: vec<ef8m1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vadd_e8m1_e8m1_tu(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vv[ef8m1, pd, fm]([[VL]], lhs, rhs, dest)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vadd_e8m1_e8m1_m(mask: mask<mmul=8>, lhs: vec<ef8m1>, rhs: vec<ef8m1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vadd_e8m1_e8m1_m(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vv[ef8m1, ud, pm]([[VL]], lhs, rhs, mask)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vadd.mask.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, <vscale x 8 x i1> %mask, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vadd_e8m1_e8m1_tumu(dest: vec<ef8m1>, mask: mask<mmul=8>, lhs: vec<ef8m1>, rhs: vec<ef8m1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vadd_e8m1_e8m1_tumu(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vv[ef8m1, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vadd.mask.nxv8i8.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %lhs, <vscale x 8 x i8> %rhs, <vscale x 8 x i1> %mask, i64 %avl, i64 0) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vadd_e8m1_i8(lhs: vec<ef8m1>, rhs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vadd_e8m1_i8(<vscale x 8 x i8> %lhs, i8 %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vx[ef8m1, ud, fm]([[VL]], lhs, rhs)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %lhs, i8 %rhs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vadd_e8m1_i8_imm(lhs: vec<ef8m1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vadd_e8m1_i8_imm(<vscale x 8 x i8> %lhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[IMM:.*]]) = long_scalar[xmul=f8, imm=0x000000000000000a]()
  ; CHECK-NEXT: ([[RES0:.*]]) = vadd.vx[ef8m1, ud, fm]([[VL]], lhs, [[IMM]])
  %res0 = tail call <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %lhs, i8 10, i64 %avl) 
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vx[ef8m1, ud, fm]([[VL]], [[RES0]], [[IMM]])
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %res0, i8 10, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vadd_e8m1_i8_tu(dest: vec<ef8m1>, lhs: vec<ef8m1>, rhs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vadd_e8m1_i8_tu(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %lhs, i8 %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vx[ef8m1, pd, fm]([[VL]], lhs, rhs, dest)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %lhs, i8 %rhs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vadd_e8m1_i8_m(mask: mask<mmul=8>, lhs: vec<ef8m1>, rhs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vadd_e8m1_i8_m(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %lhs, i8 %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vx[ef8m1, ud, pm]([[VL]], lhs, rhs, mask)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vadd.mask.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %lhs, i8 %rhs, <vscale x 8 x i1> %mask, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vadd_e8m1_i8_tumu(dest: vec<ef8m1>, mask: mask<mmul=8>, lhs: vec<ef8m1>, rhs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vadd_e8m1_i8_tumu(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, <vscale x 8 x i8> %lhs, i8 %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vx[ef8m1, pd, pm]([[VL]], lhs, rhs, dest, mask)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vadd.mask.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %lhs, i8 %rhs, <vscale x 8 x i1> %mask, i64 %avl, i64 0) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vsrl_e8m1_i64(lhs: vec<ef8m1>, rhs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vsrl_e8m1_i64(<vscale x 8 x i8> %lhs, i64 %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vsrl.vx.full[ef8m1, ud, fm]([[VL]], lhs, rhs)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vsrl.nxv8i8.i64.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %lhs, i64 %rhs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

; CHECK: def vsrl_e8m1_i64_m(mask: mask<mmul=8>, lhs: vec<ef8m1>, rhs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vsrl_e8m1_i64_m(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %lhs, i64 %rhs, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vsrl.vx.full[ef8m1, ud, pm]([[VL]], lhs, rhs, mask)
  %res = tail call <vscale x 8 x i8> @llvm.riscv.vsrl.mask.nxv8i8.i64.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %lhs, i64 %rhs, <vscale x 8 x i1> %mask, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %res 
}

declare <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, <vscale x 8 x i8>, i64)

declare <vscale x 8 x i8> @llvm.riscv.vadd.mask.nxv8i8.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, <vscale x 8 x i8>, <vscale x 8 x i1>, i64, i64 immarg)

declare <vscale x 8 x i8> @llvm.riscv.vadd.mask.nxv8i8.i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i8, <vscale x 8 x i1>, i64, i64 immarg)

declare <vscale x 8 x i8> @llvm.riscv.vadd.nxv8i8.i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i8, i64)

declare <vscale x 8 x i8> @llvm.riscv.vsrl.mask.nxv8i8.i64.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i64, <vscale x 8 x i1>, i64, i64 immarg)

declare <vscale x 8 x i8> @llvm.riscv.vsrl.nxv8i8.i64.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i64, i64)
