; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %function_result1 = call i32 @func(i32 123)
  ret i32 0
}

define i32 @func(i32 %0) {
entry:
  %param1 = alloca i32, align 4
  store i32 %0, ptr %param1, align 4
  %var_value = load i32, ptr %param1, align 4
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, i32 %var_value)
  ret i32 1
}
