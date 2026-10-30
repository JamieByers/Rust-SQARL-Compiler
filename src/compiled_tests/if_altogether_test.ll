; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.4 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.5 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.6 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.7 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.8 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  %condition = alloca i1, align 1
  store i1 true, ptr %condition, align 1
  %temp1_load = load i1, ptr %condition, align 1
  %temp2 = icmp eq i1 %temp1_load, true
  br i1 %temp2, label %if_then2, label %elif_cond0

merge1:                                           ; preds = %else3, %elif_body0, %if_then2
  %second_condition = alloca i1, align 1
  store i1 false, ptr %second_condition, align 1
  %temp8_load = load i1, ptr %second_condition, align 1
  %temp9 = icmp eq i1 %temp8_load, true
  br i1 %temp9, label %if_then5, label %elif_cond03

if_then2:                                         ; preds = %entry
  %display_str_temp3 = alloca [5 x i8], align 1
  store [5 x i8] c"true\00", ptr %display_str_temp3, align 1
  %printf = call i32 (ptr, ...) @printf(ptr @format_str, ptr %display_str_temp3)
  br label %merge1

elif_cond0:                                       ; preds = %entry
  %temp4_load = load i1, ptr %condition, align 1
  %temp5 = icmp eq i1 %temp4_load, false
  br i1 %temp5, label %elif_body0, label %else3

elif_body0:                                       ; preds = %elif_cond0
  %display_str_temp6 = alloca [6 x i8], align 1
  store [6 x i8] c"false\00", ptr %display_str_temp6, align 1
  %printf1 = call i32 (ptr, ...) @printf(ptr @format_str.1, ptr %display_str_temp6)
  br label %merge1

else3:                                            ; preds = %elif_cond0
  %display_str_temp7 = alloca [6 x i8], align 1
  store [6 x i8] c"other\00", ptr %display_str_temp7, align 1
  %printf2 = call i32 (ptr, ...) @printf(ptr @format_str.2, ptr %display_str_temp7)
  br label %merge1

merge4:                                           ; preds = %else6, %elif_body04, %if_then5
  %third_condition = alloca i32, align 4
  store i32 17, ptr %third_condition, align 4
  %temp15_load = load i32, ptr %third_condition, align 4
  %temp16 = icmp eq i32 %temp15_load, i1 true
  br i1 %temp16, label %if_then8, label %elif_cond08

if_then5:                                         ; preds = %merge1
  %display_str_temp10 = alloca [5 x i8], align 1
  store [5 x i8] c"true\00", ptr %display_str_temp10, align 1
  %printf5 = call i32 (ptr, ...) @printf(ptr @format_str.3, ptr %display_str_temp10)
  br label %merge4

elif_cond03:                                      ; preds = %merge1
  %temp11_load = load i1, ptr %second_condition, align 1
  %temp12 = icmp eq i1 %temp11_load, false
  br i1 %temp12, label %elif_body04, label %else6

elif_body04:                                      ; preds = %elif_cond03
  %display_str_temp13 = alloca [6 x i8], align 1
  store [6 x i8] c"false\00", ptr %display_str_temp13, align 1
  %printf6 = call i32 (ptr, ...) @printf(ptr @format_str.4, ptr %display_str_temp13)
  br label %merge4

else6:                                            ; preds = %elif_cond03
  %display_str_temp14 = alloca [6 x i8], align 1
  store [6 x i8] c"other\00", ptr %display_str_temp14, align 1
  %printf7 = call i32 (ptr, ...) @printf(ptr @format_str.5, ptr %display_str_temp14)
  br label %merge4

merge7:                                           ; preds = %else9, %elif_body09, %if_then8
  ret i32 0

if_then8:                                         ; preds = %merge4
  %display_str_temp17 = alloca [5 x i8], align 1
  store [5 x i8] c"true\00", ptr %display_str_temp17, align 1
  %printf10 = call i32 (ptr, ...) @printf(ptr @format_str.6, ptr %display_str_temp17)
  br label %merge7

elif_cond08:                                      ; preds = %merge4
  %temp18_load = load i32, ptr %third_condition, align 4
  %temp19 = icmp eq i32 %temp18_load, i1 false
  br i1 %temp19, label %elif_body09, label %else9

elif_body09:                                      ; preds = %elif_cond08
  %display_str_temp20 = alloca [6 x i8], align 1
  store [6 x i8] c"false\00", ptr %display_str_temp20, align 1
  %printf11 = call i32 (ptr, ...) @printf(ptr @format_str.7, ptr %display_str_temp20)
  br label %merge7

else9:                                            ; preds = %elif_cond08
  %display_str_temp21 = alloca [6 x i8], align 1
  store [6 x i8] c"other\00", ptr %display_str_temp21, align 1
  %printf12 = call i32 (ptr, ...) @printf(ptr @format_str.8, ptr %display_str_temp21)
  br label %merge7
}
