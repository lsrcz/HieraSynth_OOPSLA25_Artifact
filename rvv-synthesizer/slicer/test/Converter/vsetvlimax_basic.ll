; RUN: converter %s | FileCheck %s

; Basic vsetvlimax tests with different LMUL values

; CHECK: def vsetvlimax_e8m1() -> vl<mmul=8>:
define i64 @vsetvlimax_e8m1() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvlimax_e8mf8() -> vl<mmul=1>:
define i64 @vsetvlimax_e8mf8() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=1, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 5)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvlimax_e8mf4() -> vl<mmul=2>:
define i64 @vsetvlimax_e8mf4() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=2, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 6)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvlimax_e8mf2() -> vl<mmul=4>:
define i64 @vsetvlimax_e8mf2() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=4, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 7)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvlimax_e8m2() -> vl<mmul=16>:
define i64 @vsetvlimax_e8m2() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=16, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 1)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvlimax_e8m4() -> vl<mmul=32>:
define i64 @vsetvlimax_e8m4() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=32, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 2)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvlimax_e8m8() -> vl<mmul=64>:
define i64 @vsetvlimax_e8m8() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=64, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 3)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvlimax_e16m8() -> vl<mmul=32>:
define i64 @vsetvlimax_e16m8() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=32, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 3)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvlimax_e32m8() -> vl<mmul=16>:
define i64 @vsetvlimax_e32m8() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=16, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 3)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

; CHECK: def vsetvlimax_e64m8() -> vl<mmul=8>:
define i64 @vsetvlimax_e64m8() {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 3, i64 3)
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %1
}

declare i64 @llvm.riscv.vsetvlimax.i64(i64, i64) 
