; RUN: converter %s | FileCheck %s

; Check the same constant is merged into the same reference

; CHECK: def same_constant(a: scalar<xmul=f2>) -> scalar<xmul=f2>:
define i32 @same_constant(i32 %a) { 
  ; CHECK-NEXT: ([[CONST:.*]]) = long_scalar[xmul=f2, imm=0x0000000000000001]()
  ; CHECK-NEXT: ([[ADD:.*]]) = add.xx[width=f2](a, [[CONST]])
  %1 = add i32 %a, 1 
  ; CHECK-NEXT: ([[SUB:.*]]) = sub.xx[width=f2]([[ADD]], [[CONST]])
  %2 = sub i32 %1, 1 
  %3 = add i32 %1, %2 
  ret i32 %3 
}
