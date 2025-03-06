; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@format_str.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@format_str.4 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  %display_str_temp1 = alloca [2 x i8], align 1
  store [2 x i8] c"c\00", ptr %display_str_temp1, align 1
  %printf = call i32 (ptr, ...) @printf(ptr @format_str, ptr %display_str_temp1)
  %display_str_temp2 = alloca [13 x i8], align 1
  store [13 x i8] c"Hello world!\00", ptr %display_str_temp2, align 1
  %printf1 = call i32 (ptr, ...) @printf(ptr @format_str.1, ptr %display_str_temp2)
  %printf2 = call i32 (ptr, ...) @printf(ptr @format_str.2, i32 1)
  %printf3 = call i32 (ptr, ...) @printf(ptr @format_str.3, double 1.000000e+00)
  %printf4 = call i32 (ptr, ...) @printf(ptr @format_str.4, i32 2)
  ret i32 0
}
