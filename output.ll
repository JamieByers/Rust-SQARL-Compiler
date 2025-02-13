; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %sum = alloca i32, align 4
  store i32 3, ptr %sum, align 4
  %temp2_load = load i32, ptr %sum, align 4
  %temp3 = add i32 %temp2_load, 3
  %new_sum = alloca i32, align 4
  store i32 %temp3, ptr %new_sum, align 4
  %var_value = load i32, ptr %new_sum, align 4
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, i32 %var_value)
  ret i32 0
}
