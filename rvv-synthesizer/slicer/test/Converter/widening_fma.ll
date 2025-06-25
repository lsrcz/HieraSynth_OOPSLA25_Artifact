; RUN: converter %s | FileCheck %s

; Check that the widening fma instructions are converted correctly

; CHECK: def vwmaccu_vv_u32m2(dest: vec<ef2m2>, lhs: vec<ef4m1>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwmaccu_vv_u32m2(<vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwmaccu.vv[wide_vtype=ef2m2, fm]([[VL]], lhs, rhs, dest)
  %5 = tail call <vscale x 4 x i32> @llvm.riscv.vwmaccu.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %5 
}

declare <vscale x 4 x i32> @llvm.riscv.vwmaccu.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32>, <vscale x 4 x i16>, <vscale x 4 x i16>, i64, i64 immarg) #1

declare <vscale x 4 x i32> @llvm.riscv.vwmacc.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32>, <vscale x 4 x i16>, <vscale x 4 x i16>, i64, i64 immarg) #1

; CHECK: def vwmacc_vv_u32m2_tu(dest: vec<ef2m2>, lhs: vec<ef4m1>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwmacc_vv_u32m2_tu(<vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwmacc.vv[wide_vtype=ef2m2, fm]([[VL]], lhs, rhs, dest)
  %5 = tail call <vscale x 4 x i32> @llvm.riscv.vwmacc.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 %avl, i64 2) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %5 
}

; CHECK: def vwmacc_vv_u32m2_m(mask: mask<mmul=4>, dest: vec<ef2m2>, lhs: vec<ef4m1>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwmacc_vv_u32m2_m(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwmacc.vv[wide_vtype=ef2m2, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i32> @llvm.riscv.vwmacc.mask.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %6 
}

declare <vscale x 4 x i32> @llvm.riscv.vwmaccus.mask.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32>, <vscale x 4 x i16>, <vscale x 4 x i16>, <vscale x 4 x i1>, i64, i64 immarg) #1

; CHECK: def vwmaccus_vv_u32m2_mu(mask: mask<mmul=4>, dest: vec<ef2m2>, lhs: vec<ef4m1>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwmaccus_vv_u32m2_mu(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tamu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwmaccus.vv[wide_vtype=ef2m2, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i32> @llvm.riscv.vwmaccus.mask.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 1) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %6 
}

; CHECK: def vwmaccus_vv_u32m2_tum(mask: mask<mmul=4>, dest: vec<ef2m2>, lhs: vec<ef4m1>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwmaccus_vv_u32m2_tum(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tuma](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwmaccus.vv[wide_vtype=ef2m2, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i32> @llvm.riscv.vwmaccus.mask.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 2) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %6 
}

; CHECK: def vwmaccus_vv_u32m2_tumu(mask: mask<mmul=4>, dest: vec<ef2m2>, lhs: vec<ef4m1>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwmaccus_vv_u32m2_tumu(<vscale x 4 x i1> %mask, <vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tumu](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwmaccus.vv[wide_vtype=ef2m2, pm]([[VL]], lhs, rhs, dest, mask)
  %6 = tail call <vscale x 4 x i32> @llvm.riscv.vwmaccus.mask.nxv4i32.nxv4i16.nxv4i16.i64(<vscale x 4 x i32> %dest, <vscale x 4 x i16> %lhs, <vscale x 4 x i16> %rhs, <vscale x 4 x i1> %mask, i64 %avl, i64 0) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %6 
}

; CHECK: def vwmaccsu_vx_u32m2(dest: vec<ef2m2>, lhs: scalar<xmul=f4>, rhs: vec<ef4m1>, avl: scalar<xmul=1>) -> vec<ef2m2>:
define <vscale x 4 x i32> @vwmaccsu_vx_u32m2(<vscale x 4 x i32> %dest, i16 noundef zeroext %lhs, <vscale x 4 x i16> %rhs, i64 noundef %avl) { 
  ; CHECK-NEXT: ([[VL:.*]]) = vsetvl[mmul=4, tama](avl)
  ; CHECK-NEXT: ([[RES:.*]]) = vwmaccsu.vx[wide_vtype=ef2m2, fm]([[VL]], lhs, rhs, dest)
  %5 = tail call <vscale x 4 x i32> @llvm.riscv.vwmaccsu.nxv4i32.i16.nxv4i16.i64(<vscale x 4 x i32> %dest, i16 %lhs, <vscale x 4 x i16> %rhs, i64 %avl, i64 3) 
  ; CHECK-NEXT: return ([[RES]])
  ret <vscale x 4 x i32> %5 
}

declare <vscale x 4 x i32> @llvm.riscv.vwmaccsu.nxv4i32.i16.nxv4i16.i64(<vscale x 4 x i32>, i16, <vscale x 4 x i16>, i64, i64 immarg) #1
