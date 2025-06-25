; RUN: converter %s | FileCheck %s

; CHECK: def name_collison(v0: scalar<xmul=f2>, v1: scalar<xmul=f2>) -> scalar<xmul=f2>:
define i32 @name_collison(i32 %0, i32 %v0) { 
  ; CHECK-NEXT: (v2) = add.xx[width=f2](v0, v1)
  %v1 = add i32 %0, %v0 
  ; CHECK-NEXT: return (v2)
  ret i32 %v1 
}
