; RUN: converter %s | FileCheck %s

; Check empty functions are converted correctly

; CHECK: def empty_func(arg: vec<ef8m1>) -> vec<ef8m1>:
define <vscale x 8 x i8> @empty_func(<vscale x 8 x i8> %arg) { 
  ; CHECK-NEXT: return (arg)
  ret <vscale x 8 x i8> %arg 
}
