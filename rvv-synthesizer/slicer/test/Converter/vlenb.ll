; RUN: converter %s | FileCheck %s

; Check that vlenb is converted correctly

; CHECK: def vlenb() -> scalar<xmul=1>:
define i64 @vlenb() {
  ; CHECK-NEXT: ([[RES:.*]]) = vlenb()
  %1 = tail call i64 @llvm.read_register.i64(metadata !0)
  ; CHECK-NEXT: return ([[RES]])
  ret i64 %1
}

declare i64 @llvm.read_register.i64(metadata)

!0 = !{!"vlenb"}
