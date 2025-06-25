; RUN: converter %s | FileCheck %s

; Check miscellaneous masked instructions are converted correctly

; CHECK: def vcpop_m_b4(input: mask<mmul=16>, avl: scalar<xmul=1>) -> scalar<xmul=1>:
define i64 @vcpop_m_b4(<vscale x 16 x i1> %input, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=16, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vcpop.m[mmul=16, fm]([[VL]], input)
  %3 = tail call i64 @llvm.riscv.vcpop.nxv16i1.i64(<vscale x 16 x i1> %input, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret i64 %3
}

declare i64 @llvm.riscv.vcpop.nxv16i1.i64(<vscale x 16 x i1>, i64) #2

; CHECK: def vcpop_m_b4_m(mask: mask<mmul=16>, input: mask<mmul=16>, avl: scalar<xmul=1>) -> scalar<xmul=1>:
define i64 @vcpop_m_b4_m(<vscale x 16 x i1> %mask, <vscale x 16 x i1> %input, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=16, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vcpop.m[mmul=16, pm]([[VL]], input, mask)
  %4 = tail call i64 @llvm.riscv.vcpop.mask.nxv16i1.i64(<vscale x 16 x i1> %input, <vscale x 16 x i1> %mask, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret i64 %4
}

declare i64 @llvm.riscv.vcpop.mask.nxv16i1.i64(<vscale x 16 x i1>, <vscale x 16 x i1>, i64) #2

; CHECK: def vfirst_m_b4(input: mask<mmul=16>, avl: scalar<xmul=1>) -> scalar<xmul=1>:
define i64 @vfirst_m_b4(<vscale x 16 x i1> %input, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=16, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vfirst.m[mmul=16, fm]([[VL]], input)
  %3 = tail call i64 @llvm.riscv.vfirst.nxv16i1.i64(<vscale x 16 x i1> %input, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret i64 %3
}

declare i64 @llvm.riscv.vfirst.nxv16i1.i64(<vscale x 16 x i1>, i64) #2

; CHECK: def vfirst_m_b4_m(mask: mask<mmul=16>, input: mask<mmul=16>, avl: scalar<xmul=1>) -> scalar<xmul=1>:
define i64 @vfirst_m_b4_m(<vscale x 16 x i1> %mask, <vscale x 16 x i1> %input, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=16, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vfirst.m[mmul=16, pm]([[VL]], input, mask)
  %4 = tail call i64 @llvm.riscv.vfirst.mask.nxv16i1.i64(<vscale x 16 x i1> %input, <vscale x 16 x i1> %mask, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret i64 %4
}

declare i64 @llvm.riscv.vfirst.mask.nxv16i1.i64(<vscale x 16 x i1>, <vscale x 16 x i1>, i64) #2

; CHECK: def vmsbf_m_b16(input: mask<mmul=4>, avl: scalar<xmul=1>) -> mask<mmul=4>:
define <vscale x 4 x i1> @vmsbf_m_b16(<vscale x 4 x i1> %input, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmsbf.m[mmul=4, ud, fm]([[VL]], input)
  %3 = tail call <vscale x 4 x i1> @llvm.riscv.vmsbf.nxv4i1.i64(<vscale x 4 x i1> %input, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i1> %3
}

declare <vscale x 4 x i1> @llvm.riscv.vmsbf.nxv4i1.i64(<vscale x 4 x i1>, i64) #2

; CHECK: def vmsbf_m_b16_m(mask: mask<mmul=4>, input: mask<mmul=4>, avl: scalar<xmul=1>) -> mask<mmul=4>:
define <vscale x 4 x i1> @vmsbf_m_b16_m(<vscale x 4 x i1> %mask, <vscale x 4 x i1> %input, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmsbf.m[mmul=4, ud, pm]([[VL]], input, mask)
  %4 = tail call <vscale x 4 x i1> @llvm.riscv.vmsbf.mask.nxv4i1.i64(<vscale x 4 x i1> poison, <vscale x 4 x i1> %input, <vscale x 4 x i1> %mask, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i1> %4
}

declare <vscale x 4 x i1> @llvm.riscv.vmsbf.mask.nxv4i1.i64(<vscale x 4 x i1>, <vscale x 4 x i1>, <vscale x 4 x i1>, i64) #2

; CHECK: def vmsbf_m_b16_mu(mask: mask<mmul=4>, dest: mask<mmul=4>, input: mask<mmul=4>, avl: scalar<xmul=1>) -> mask<mmul=4>:
define <vscale x 4 x i1> @vmsbf_m_b16_mu(<vscale x 4 x i1> %mask, <vscale x 4 x i1> %dest, <vscale x 4 x i1> %input, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmsbf.m[mmul=4, pd, pm]([[VL]], input, dest, mask)
  %5 = tail call <vscale x 4 x i1> @llvm.riscv.vmsbf.mask.nxv4i1.i64(<vscale x 4 x i1> %dest, <vscale x 4 x i1> %input, <vscale x 4 x i1> %mask, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i1> %5
}

; CHECK: def vmsif_m_b16(input: mask<mmul=4>, avl: scalar<xmul=1>) -> mask<mmul=4>:
define <vscale x 4 x i1> @vmsif_m_b16(<vscale x 4 x i1> %input, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmsif.m[mmul=4, ud, fm]([[VL]], input)
  %3 = tail call <vscale x 4 x i1> @llvm.riscv.vmsif.nxv4i1.i64(<vscale x 4 x i1> %input, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i1> %3
}

declare <vscale x 4 x i1> @llvm.riscv.vmsif.nxv4i1.i64(<vscale x 4 x i1>, i64) #2

; CHECK: def vmsof_m_b16(input: mask<mmul=4>, avl: scalar<xmul=1>) -> mask<mmul=4>:
define <vscale x 4 x i1> @vmsof_m_b16(<vscale x 4 x i1> %input, i64 noundef %avl) {
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vmsof.m[mmul=4, ud, fm]([[VL]], input)
  %3 = tail call <vscale x 4 x i1> @llvm.riscv.vmsof.nxv4i1.i64(<vscale x 4 x i1> %input, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i1> %3
}

declare <vscale x 4 x i1> @llvm.riscv.vmsof.nxv4i1.i64(<vscale x 4 x i1>, i64) #2
