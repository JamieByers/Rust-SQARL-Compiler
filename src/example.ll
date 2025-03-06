; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %array = alloca <3 x i32>, align 16
  store <3 x i32> <i32 1, i32 2, i32 3>, ptr %array, align 16
  ret i32 0
}
