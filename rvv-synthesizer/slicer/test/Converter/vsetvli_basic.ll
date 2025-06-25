; RUN: converter %s | FileCheck %s

; Tests for vsetvli with constant AVL values

; CHECK: def vsetvli_e8m1_constant() -> vl<mmul=8>:
define i64 @vsetvli_e8m1_constant() {
  ; CHECK-NEXT: ([[CONST:.*]]) = long_scalar[xmul=1, imm=0x000000000000000a]()
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama]([[CONST]])
  %1 = tail call i64 @llvm.riscv.vsetvli.i64(i64 10, i64 0, i64 0)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvli_minus_one_is_max() -> vl<mmul=8>:
define i64 @vsetvli_minus_one_is_max() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvli.i64(i64 -1, i64 0, i64 0)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

declare i64 @llvm.riscv.vsetvli.i64(i64, i64, i64) 
