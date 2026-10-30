; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  %function_result2 = call i1 @func(i1 true)
  ret i32 0
}

define i1 @func(i1 %0) {
entry:
  %param1 = alloca i1, align 1
  store i1 %0, ptr %param1, align 1
  %var_value = load i1, ptr %param1, align 1
  %printf = call i32 (ptr, ...) @printf(ptr @format_str, i1 %var_value)
  %temp1_load = load i1, ptr %param1, align 1
  ret i1 %temp1_load
}
