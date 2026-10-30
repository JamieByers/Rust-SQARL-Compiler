; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  %condition = alloca i1, align 1
  store i1 true, ptr %condition, align 1
  %temp1_load = load i1, ptr %condition, align 1
  %temp2 = icmp eq i1 %temp1_load, true
  br i1 %temp2, label %if_then2, label %merge1

merge1:                                           ; preds = %if_then2, %entry
  ret i32 0

if_then2:                                         ; preds = %entry
  %display_str_temp3 = alloca [5 x i8], align 1
  store [5 x i8] c"true\00", ptr %display_str_temp3, align 1
  %printf = call i32 (ptr, ...) @printf(ptr @format_str, ptr %display_str_temp3)
  br label %merge1
}
