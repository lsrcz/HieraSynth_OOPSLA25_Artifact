; RUN: converter %s | FileCheck %s

; Check that vid instructions are converted correctly

; CHECK: def vid_e8m1_vsetvlimax() -> vec<ef8m1>:
define <vscale x 8 x i8> @vid_e8m1_vsetvlimax() { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvlmax[mmul=8, tama]()
  %1 = tail call i64 @llvm.riscv.vsetvlimax.i64(i64 0, i64 0) 
  ; CHECK-NEXT: ([[RES:.*]]) = vid.v[ef8m1, ud, fm]([[VL]])
  %2 = tail call <vscale x 8 x i8> @llvm.riscv.vid.nxv8i8.i64(<vscale x 8 x i8> poison, i64 %1) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %2 
}

; CHECK: def vid_e8m1_tu(dest: vec<ef8m1>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vid_e8m1_tu(<vscale x 8 x i8> %dest, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vid.v[ef8m1, pd, fm]([[VL]], dest)
  %3 = tail call <vscale x 8 x i8> @llvm.riscv.vid.nxv8i8.i64(<vscale x 8 x i8> %dest, i64 %avl) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %3 
}

; CHECK: def vid_e8m1_m(dest: vec<ef8m1>, mask: mask<mmul=8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vid_e8m1_m(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vid.v[ef8m1, pd, pm]([[VL]], dest, mask)
  %3 = tail call <vscale x 8 x i8> @llvm.riscv.vid.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %3 
}

; CHECK: def vid_e8m1_tum(dest: vec<ef8m1>, mask: mask<mmul=8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vid_e8m1_tum(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vid.v[ef8m1, pd, pm]([[VL]], dest, mask)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vid.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, i64 %avl, i64 2) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %4 
}

; CHECK: def vid_e8m1_tumu(dest: vec<ef8m1>, mask: mask<mmul=8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vid_e8m1_tumu(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vid.v[ef8m1, pd, pm]([[VL]], dest, mask)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vid.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, i64 %avl, i64 0) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %4 
}

; CHECK: def vid_e8m1_mu(dest: vec<ef8m1>, mask: mask<mmul=8>, avl: scalar<xmul=1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @vid_e8m1_mu(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, i64 %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=8, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vid.v[ef8m1, pd, pm]([[VL]], dest, mask)
  %4 = tail call <vscale x 8 x i8> @llvm.riscv.vid.mask.nxv8i8.i64(<vscale x 8 x i8> %dest, <vscale x 8 x i1> %mask, i64 %avl, i64 1) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 8 x i8> %4 
}

declare i64 @llvm.riscv.vsetvlimax.i64(i64 immarg, i64 immarg)

declare <vscale x 8 x i8> @llvm.riscv.vid.mask.nxv8i8.i64(<vscale x 8 x i8>, <vscale x 8 x i1>, i64, i64 immarg)

declare <vscale x 8 x i8> @llvm.riscv.vid.nxv8i8.i64(<vscale x 8 x i8>, i64)
