; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  %display_str_temp1 = alloca [32 x i8], align 1
  store [32 x i8] c"<3 x i32> <i32 1, i32 2, i32 3>\00", ptr %display_str_temp1, align 1
  %printf = call i32 (ptr, ...) @printf(ptr @format_str, ptr %display_str_temp1)
  ret i32 0
}
