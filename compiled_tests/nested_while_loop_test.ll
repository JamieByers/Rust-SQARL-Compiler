; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %outer_condition = alloca i32, align 4
  store i32 0, ptr %outer_condition, align 4
  br label %loop1

loop1:                                            ; preds = %merge5, %entry
  %temp1_load = load i32, ptr %outer_condition, align 4
  %temp2 = icmp slt i32 %temp1_load, 3
  br i1 %temp2, label %loop_body3, label %merge2

merge2:                                           ; preds = %loop1
  ret i32 0

loop_body3:                                       ; preds = %loop1
  %temp3_load = load i32, ptr %outer_condition, align 4
  %temp4 = add i32 %temp3_load, 1
  store i32 %temp4, ptr %outer_condition, align 4
  %inner_condition = alloca i32, align 4
  store i32 0, ptr %inner_condition, align 4
  br label %loop4

loop4:                                            ; preds = %loop_body6, %loop_body3
  %temp5_load = load i32, ptr %inner_condition, align 4
  %temp6 = icmp slt i32 %temp5_load, 3
  br i1 %temp6, label %loop_body6, label %merge5

merge5:                                           ; preds = %loop4
  br label %loop1

loop_body6:                                       ; preds = %loop4
  %var_value = load i32, ptr %inner_condition, align 4
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, i32 %var_value)
  br label %loop4
}
