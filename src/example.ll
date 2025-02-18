; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  br i1 true, label %if_then2, label %else3

merge1:                                           ; preds = %else3, %merge4
  ret i32 0

if_then2:                                         ; preds = %entry
  br i1 true, label %if_then5, label %elif_cond0

else3:                                            ; preds = %entry
  %display_str_temp7 = alloca [18 x i8], align 1
  store [18 x i8] c"SUPER DUPER FALSE\00", ptr %display_str_temp7, align 1
  %printf3 = call ptr (ptr, ...) @printf(ptr @format_str.3, ptr %display_str_temp7)
  br label %merge1

merge4:                                           ; preds = %else6, %elif_body0, %if_then5
  br label %merge1

if_then5:                                         ; preds = %if_then2
  %display_str_temp3 = alloca [5 x i8], align 1
  store [5 x i8] c"TRUE\00", ptr %display_str_temp3, align 1
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, ptr %display_str_temp3)
  br label %merge4

elif_cond0:                                       ; preds = %if_then2
  br i1 false, label %elif_body0, label %else6

elif_body0:                                       ; preds = %elif_cond0
  %display_str_temp5 = alloca [14 x i8], align 1
  store [14 x i8] c"SHOULD RETURN\00", ptr %display_str_temp5, align 1
  %printf1 = call ptr (ptr, ...) @printf(ptr @format_str.1, ptr %display_str_temp5)
  br label %merge4

else6:                                            ; preds = %elif_cond0
  %display_str_temp6 = alloca [6 x i8], align 1
  store [6 x i8] c"FALSE\00", ptr %display_str_temp6, align 1
  %printf2 = call ptr (ptr, ...) @printf(ptr @format_str.2, ptr %display_str_temp6)
  br label %merge4
}
