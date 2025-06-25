; RUN: converter %s | FileCheck %s

; Check that when ratio is unchanged, the VL instructions should be removed

; CHECK: def vsetvlimax_ratio_unchanged() -> vl<mmul=8>:
define i64 @vsetvlimax_ratio_unchanged() { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0) 
  %2 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 1) 
  %3 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 2) 
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %3 
}

; CHECK: def vsetvlimax_ratio_changed() -> vl<mmul=8>:
define i64 @vsetvlimax_ratio_changed() { 
  ; CHECK-NEXT: ([[VL1:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0) 
  ; CHECK-NEXT: ([[VL2:.*]]) = vsetvlmax[mmul=4, tama]()
  %2 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 1, i64 0) 
  ; CHECK-NEXT: ([[VL3:.*]]) = vsetvlmax[mmul=8, tama]()
  %3 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 2, i64 2) 
  ; CHECK-NEXT: return ([[VL3]])
  ret i64 %3 
}

; CHECK: def vsetvli_e8m1_relayed(avl: scalar<xmul=1>) -> vl<mmul=16>:
define i64 @vsetvli_e8m1_relayed(i64 %avl) { 
  ; CHECK-NEXT: ([[VL1:.*]]) = vsetvl[mmul=16, tama](avl)
  %1 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %avl, i64 0, i64 1) 
  ; CHECK-NEXT: ([[VL2:.*]]) = vsetvlrelay[mmul=8, src_mmul=16, tama]([[VL1]])
  %2 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %1, i64 0, i64 0) 
  ; CHECK-NEXT: ([[VL3:.*]]) = vsetvlrelay[mmul=16, src_mmul=8, tama]([[VL2]])
  %3 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %2, i64 0, i64 1) 
  ; CHECK-NEXT: return ([[VL3]])
  ret i64 %3 
}

; CHECK: def vsetvli_unchanged_vsetvlmax() -> vl<mmul=8>:
define i64 @vsetvli_unchanged_vsetvlmax() { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0) 
  %2 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %1, i64 0, i64 0) 
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %2 
}

; CHECK: def vsetvli_unchanged(avl: scalar<xmul=1>) -> vl<mmul=8>:
define i64 @vsetvli_unchanged(i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  %1 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %avl, i64 0, i64 0) 
  %2 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %1, i64 0, i64 0) 
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %2 
}

; CHECK: def vsetvli_use_same(avl: scalar<xmul=1>) -> vl<mmul=8>:
define i64 @vsetvli_use_same(i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  %1 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %avl, i64 0, i64 0) 
  %2 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %avl, i64 0, i64 0) 
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %2 
}

; CHECK: def vsetvli_unchanged_2(avl: scalar<xmul=1>) -> vl<mmul=8>:
define i64 @vsetvli_unchanged_2(i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  %1 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %avl, i64 0, i64 0) 
  %2 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %1, i64 0, i64 0) 
  %3 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %avl, i64 0, i64 0) 
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %3 
}

; CHECK: def vsetvli_disable_max(avl: scalar<xmul=1>) -> vl<mmul=8>:
define i64 @vsetvli_disable_max(i64 %avl) { 
  ; CHECK-NEXT: ([[VL1:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0) 
  ; CHECK-NEXT: ([[VL2:.*]]) = vsetvl[mmul=8, tama](avl)
  %2 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %avl, i64 0, i64 0) 
  ; CHECK-NEXT: ([[VL3:.*]]) = vsetvlmax[mmul=8, tama]()
  %3 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0) 
  ; CHECK-NEXT: return ([[VL3]])
  ret i64 %3 
}

; CHECK: def vsetvli_relay_dont_disable_max() -> vl<mmul=8>:
define i64 @vsetvli_relay_dont_disable_max() { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0) 
  %2 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %1, i64 0, i64 0) 
  %3 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0) 
  ; CHECK-NEXT: return ([[VL]])
  ret i64 %3 
}

; CHECK: def vsetvli_only_change_policy(vec: vec<ef8m1>, avl: scalar<xmul=1>) -> vl<mmul=8>:
define i64 @vsetvli_only_change_policy(<vscale x 8 x i8> %vec, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[ID:.*]]) = vid.v[ef8m1, pd, fm]([[VL]], vec)
  %3 = tail call <vscale x 8 x i8> @llvm.riscv.vid.nxv8i8.i64(<vscale x 8 x i8> %vec, i64 %avl) 
  ; CHECK-NEXT: ([[VL2:.*]]) = vsetvl[mmul=8, tama](avl)
  %4 = tail call i64 @llvm.riscv.vsetvli.i64(i64 %avl, i64 0, i64 0) 
  ; CHECK-NEXT: return ([[VL2]])
  ret i64 %4 
}

; CHECK: def vl_used_as_scalar(a: vec<e1m1>) -> vec<e1m1>:
define <vscale x 1 x i64> @vl_used_as_scalar(<vscale x 1 x i64> %a) { 
  ; CHECK-NEXT: ([[VL1:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0) 
  ; CHECK-NEXT: ([[CONST:.*]]) = long_scalar[xmul=1, imm=0x000000000000000a]()
  ; CHECK-NEXT: ([[VL2:.*]]) = vsetvl[mmul=1, tama]([[CONST]])
  ; CHECK-NEXT: ([[SCALAR:.*]]) = vl_to_scalar[mmul=8]([[VL1]])
  ; CHECK-NEXT: ([[RES:.*]]) = vadd.vx[e1m1, ud, fm]([[VL2]], a, [[SCALAR]])
  %res = tail call <vscale x 1 x i64> @llvm.riscv.vadd.nxv1i64.i64.i64(<vscale x 1 x i64> poison, <vscale x 1 x i64> %a, i64 %1, i64 10) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 1 x i64> %res 
}

declare <vscale x 1 x i64> @llvm.riscv.vadd.nxv1i64.i64.i64(<vscale x 1 x i64>, <vscale x 1 x i64>, i64, i64) #2

declare <vscale x 8 x i8> @llvm.riscv.vid.nxv8i8.i64(<vscale x 8 x i8>, i64)

declare i64 @llvm.riscv.vsetvli.i64(i64, i64, i64) 

declare i64 @llvm.riscv.vsetvlimax.i64(i64, i64) 
