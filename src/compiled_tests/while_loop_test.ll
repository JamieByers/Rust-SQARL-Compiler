; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %condition = alloca i32, align 4
  store i32 0, ptr %condition, align 4
  br label %loop1

loop1:                                            ; preds = %loop_body3, %entry
  %temp1_load = load i32, ptr %condition, align 4
  %temp2 = icmp sle i32 %temp1_load, 5
  br i1 %temp2, label %loop_body3, label %merge2

merge2:                                           ; preds = %loop1
  ret i32 0

loop_body3:                                       ; preds = %loop1
  %var_value = load i32, ptr %condition, align 4
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, i32 %var_value)
  %temp3_load = load i32, ptr %condition, align 4
  %temp4 = add i32 %temp3_load, 1
  store i32 %temp4, ptr %condition, align 4
  br label %loop1
}
