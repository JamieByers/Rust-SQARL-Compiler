; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, i1 true)
  ret i32 0
}
