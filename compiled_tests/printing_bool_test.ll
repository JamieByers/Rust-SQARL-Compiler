; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@format_str.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  %printf = call i32 (ptr, ...) @printf(ptr @format_str, i32 1)
  %printf1 = call i32 (ptr, ...) @printf(ptr @format_str.1, i32 0)
  ret i32 0
}
