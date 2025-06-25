; RUN: converter %s | FileCheck %s

; Check scalar binary operations are converted correctly

; CHECK: def add(a: scalar<xmul=f2>, b: scalar<xmul=f2>) -> scalar<xmul=f2>:
define dso_local noundef signext i32 @add(i32 noundef signext %a, i32 noundef signext %b) local_unnamed_addr {
  ; CHECK-NEXT: ([[RES:.*]]) = add.xx[width=f2](b, a)
  %add = add nsw i32 %b, %a
  ; CHECK-NEXT: return ([[RES]])
  ret i32 %add
}

; CHECK: def sub(a: scalar<xmul=f2>, b: scalar<xmul=f2>) -> scalar<xmul=f2>:
define dso_local noundef signext i32 @sub(i32 noundef signext %a, i32 noundef signext %b) local_unnamed_addr {
  ; CHECK-NEXT: ([[RES:.*]]) = sub.xx[width=f2](a, b)
  %sub = sub nsw i32 %a, %b
  ; CHECK-NEXT: return ([[RES]])
  ret i32 %sub
}

; CHECK: def band(a: scalar<xmul=f2>, b: scalar<xmul=f2>) -> scalar<xmul=f2>:
define dso_local noundef signext i32 @band(i32 noundef signext %a, i32 noundef signext %b) local_unnamed_addr {
  ; CHECK-NEXT: ([[RES:.*]]) = and.xx[width=f2](b, a)
  %and = and i32 %b, %a
  ; CHECK-NEXT: return ([[RES]])
  ret i32 %and
}

; CHECK: def shl(a: scalar<xmul=f2>, b: scalar<xmul=f2>) -> scalar<xmul=f2>:
define dso_local noundef signext i32 @shl(i32 noundef signext %a, i32 noundef signext %b) local_unnamed_addr {
  ; CHECK-NEXT: ([[RES:.*]]) = sll.xx[width=f2](a, b)
  %shl = shl i32 %a, %b
  ; CHECK-NEXT: return ([[RES]])
  ret i32 %shl
}

; CHECK: def lshr(a: scalar<xmul=f2>, b: scalar<xmul=f2>) -> scalar<xmul=f2>:
define dso_local noundef signext i32 @lshr(i32 noundef signext %a, i32 noundef signext %b) local_unnamed_addr {
  ; CHECK-NEXT: ([[RES:.*]]) = srl.xx[width=f2](a, b)
  %shr = lshr i32 %a, %b
  ; CHECK-NEXT: return ([[RES]])
  ret i32 %shr
}

; CHECK: def ashr(a: scalar<xmul=f2>, b: scalar<xmul=f2>) -> scalar<xmul=f2>:
define dso_local noundef signext i32 @ashr(i32 noundef signext %a, i32 noundef signext %b) local_unnamed_addr {
  ; CHECK-NEXT: ([[RES:.*]]) = sra.xx[width=f2](a, b)
  %shr = ashr i32 %a, %b
  ; CHECK-NEXT: return ([[RES]])
  ret i32 %shr
}

; CHECK: def umin(a: scalar<xmul=f2>, b: scalar<xmul=f2>) -> scalar<xmul=f2>:
define dso_local noundef signext i32 @umin(i32 noundef signext %a, i32 noundef signext %b) local_unnamed_addr {
  ; CHECK-NEXT: ([[RES:.*]]) = minu.xx[width=f2](b, a)
  %umin = tail call i32 @llvm.umin.i32(i32 %b, i32 %a)
  ; CHECK-NEXT: return ([[RES]])
  ret i32 %umin
}

declare i32 @llvm.umin.i32(i32, i32) #3

; CHECK: def trunc(v: scalar<xmul=1>) -> scalar<xmul=f2>:
define dso_local noundef signext i32 @trunc(i64 noundef %v) {
  ; CHECK-NEXT: ([[RES:.*]]) = trunc[src=1, dest=f2](v)
  %conv = trunc i64 %v to i32
  ; CHECK-NEXT: return ([[RES]])
  ret i32 %conv
}
