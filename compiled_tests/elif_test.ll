; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  %condition = alloca i1, align 1
  store i1 false, ptr %condition, align 1
  %temp1_load = load i1, ptr %condition, align 1
  %temp2 = icmp eq i1 %temp1_load, true
  br i1 %temp2, label %if_then2, label %elif_cond0

merge1:                                           ; preds = %elif_body0, %elif_cond0, %if_then2
  ret i32 0

if_then2:                                         ; preds = %entry
  %display_str_temp3 = alloca [5 x i8], align 1
  store [5 x i8] c"true\00", ptr %display_str_temp3, align 1
  %printf = call i32 (ptr, ...) @printf(ptr @format_str, ptr %display_str_temp3)
  br label %merge1

elif_cond0:                                       ; preds = %entry
  %temp4_load = load i1, ptr %condition, align 1
  %temp5 = icmp eq i1 %temp4_load, false
  br i1 %temp5, label %elif_body0, label %merge1

elif_body0:                                       ; preds = %elif_cond0
  %display_str_temp6 = alloca [6 x i8], align 1
  store [6 x i8] c"false\00", ptr %display_str_temp6, align 1
  %printf1 = call i32 (ptr, ...) @printf(ptr @format_str.1, ptr %display_str_temp6)
  br label %merge1
}
