; RUN: converter %s | FileCheck %s

; Check that the sliding instructions are converted correctly

; CHECK: def vslideup_e8m1_i64(dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslideup_e8m1_i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslideup.vx[ef8m1, pd, fm]([[VL]], src, rs, dest)
  %5 = tail call <vscale x 8 x i8> @llvm.riscv.vslideup.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %5 
}

; CHECK: def vslideup_e8m1_i64_tu(dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslideup_e8m1_i64_tu(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslideup.vx[ef8m1, pd, fm]([[VL]], src, rs, dest)
  %5 = tail call <vscale x 8 x i8> @llvm.riscv.vslideup.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, i64 %avl, i64 2) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %5 
}

; CHECK: def vslideup_e8m1_i64_m(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslideup_e8m1_i64_m(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslideup.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslideup.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslideup_e8m1_i64_tum(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslideup_e8m1_i64_tum(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslideup.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslideup.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 2) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslideup_e8m1_i64_mu(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslideup_e8m1_i64_mu(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslideup.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslideup.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 1) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslideup_e8m1_i64_tumu(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslideup_e8m1_i64_tumu(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslideup.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslideup.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 0) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslidedown_e8m1_i64(src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslidedown_e8m1_i64(<vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslidedown.vx[ef8m1, ud, fm]([[VL]], src, rs)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vslidedown.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %src, i64 %rs, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %4 
}

; CHECK: def vslidedown_e8m1_i64_tu(dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslidedown_e8m1_i64_tu(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslidedown.vx[ef8m1, pd, fm]([[VL]], src, rs, dest)
  %5 = tail call <vscale x 8 x i8> @llvm.riscv.vslidedown.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, i64 %avl, i64 2) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %5 
}

; CHECK: def vslidedown_e8m1_i64_m(mask: mask<mmul=8>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslidedown_e8m1_i64_m(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslidedown.vx[ef8m1, ud, pm]([[VL]], src, rs, mask)
  %5 = tail call <vscale x 8 x i8> @llvm.riscv.vslidedown.mask.nxv8i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %src, i64 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %5 
}

; CHECK: def vslidedown_e8m1_i64_tum(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslidedown_e8m1_i64_tum(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslidedown.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslidedown.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 2) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslidedown_e8m1_i64_mu(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslidedown_e8m1_i64_mu(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslidedown.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslidedown.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 1) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslidedown_e8m1_i64_tumu(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslidedown_e8m1_i64_tumu(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 noundef %rs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslidedown.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslidedown.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i64 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 0) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslide1up_e8m1_i8(src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1up_e8m1_i8(<vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) {
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1up.vx[ef8m1, ud, fm]([[VL]], src, rs)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1up.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %src, i8 %rs, i64 %avl)
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %4
}

; CHECK: def vslide1up_e8m1_i8_tu(dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1up_e8m1_i8_tu(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1up.vx[ef8m1, pd, fm]([[VL]], src, rs, dest)
  %5 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1up.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 %rs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %5 
}

; CHECK: def vslide1up_e8m1_i8_m(mask: mask<mmul=8>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1up_e8m1_i8_m(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1up.vx[ef8m1, ud, pm]([[VL]], src, rs, mask)
  %5 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1up.mask.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %src, i8 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %5 
}

; CHECK: def vslide1up_e8m1_i8_tum(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1up_e8m1_i8_tum(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1up.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1up.mask.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 2) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslide1up_e8m1_i8_mu(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1up_e8m1_i8_mu(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1up.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1up.mask.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 1) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslide1up_e8m1_i8_tumu(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1up_e8m1_i8_tumu(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1up.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1up.mask.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 0) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslide1down_e8m1_i8(src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1down_e8m1_i8(<vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1down.vx[ef8m1, ud, fm]([[VL]], src, rs)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1down.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %src, i8 %rs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %4 
}

; CHECK: def vslide1down_e8m1_i8_tu(dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1down_e8m1_i8_tu(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1down.vx[ef8m1, pd, fm]([[VL]], src, rs, dest)
  %5 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1down.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 %rs, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %5 
}

; CHECK: def vslide1down_e8m1_i8_m(mask: mask<mmul=8>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1down_e8m1_i8_m(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1down.vx[ef8m1, ud, pm]([[VL]], src, rs, mask)
  %5 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1down.mask.nxv8i8.i8.i64(<vscale x 8 x i8> poison, <vscale x 8 x i8> %src, i8 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %5 
}

; CHECK: def vslide1down_e8m1_i8_tum(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1down_e8m1_i8_tum(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1down.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1down.mask.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 2) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslide1down_e8m1_i8_mu(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1down_e8m1_i8_mu(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1down.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1down.mask.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 1) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

; CHECK: def vslide1down_e8m1_i8_tumu(mask: mask<mmul=8>, dest: vec<ef8m1>, src: vec<ef8m1>, rs: scalar<xmul=f8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vslide1down_e8m1_i8_tumu(<vscale x 8 x i1> %mask, <vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 noundef signext %rs, i64 noundef %avl) { 
  ; CHECK: ([[VL:.*]]) = vsetvl[mmul=8, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vslide1down.vx[ef8m1, pd, pm]([[VL]], src, rs, dest, mask)
  %6 = tail call <vscale x 8 x i8> @llvm.riscv.vslide1down.mask.nxv8i8.i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i8> %src, i8 %rs, <vscale x 8 x i1> %mask, i64 %avl, i64 0) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %6 
}

declare <vscale x 8 x i8> @llvm.riscv.vslideup.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i64, i64, i64 immarg) #3

declare <vscale x 8 x i8> @llvm.riscv.vslideup.mask.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i64, <vscale x 8 x i1>, i64, i64 immarg) #3

declare <vscale x 8 x i8> @llvm.riscv.vslidedown.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i64, i64, i64 immarg) #3

declare <vscale x 8 x i8> @llvm.riscv.vslidedown.mask.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i64, <vscale x 8 x i1>, i64, i64 immarg) #3

declare <vscale x 8 x i8> @llvm.riscv.vslide1up.nxv8i8.i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i8, i64) #3

declare <vscale x 8 x i8> @llvm.riscv.vslide1up.mask.nxv8i8.i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i8, <vscale x 8 x i1>, i64, i64 immarg) #3

declare <vscale x 8 x i8> @llvm.riscv.vslide1down.nxv8i8.i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i8, i64) #3

declare <vscale x 8 x i8> @llvm.riscv.vslide1down.mask.nxv8i8.i8.i64(<vscale x 8 x i8>, <vscale x 8 x i8>, i8, <vscale x 8 x i1>, i64, i64 immarg) #3
