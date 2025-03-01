; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@concat_format = global [4 x i8] c"%s%s"
@format_str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %func_result2 = call [256 x i8] @func([13 x i8] c"Hello world!\00")
  %function_string = alloca [256 x i8], align 1
  store [256 x i8] %func_result2, ptr %function_string, align 1
  %temp3_load = load [256 x i8], ptr %function_string, align 1
  %buffer = alloca [512 x i8], align 1
  %lhs_array = alloca [18 x i8], align 1
  store [18 x i8] c"function string: \00", ptr %lhs_array, align 1
  %rhs_array = alloca [256 x i8], align 1
  store [256 x i8] %temp3_load, ptr %rhs_array, align 1
  %lhs_ptr = getelementptr [18 x i8], ptr %lhs_array, i32 0, i32 0
  %rhs_ptr = getelementptr [256 x i8], ptr %rhs_array, i32 0, i32 0
  %sprintf_call = call i32 (ptr, ptr, ...) @sprintf(ptr %buffer, ptr @concat_format, ptr %lhs_ptr, ptr %rhs_ptr)
  %loaded_buffer = load [512 x i8], ptr %buffer, align 1
  %temp4 = alloca [512 x i8], align 1
  store [512 x i8] %loaded_buffer, ptr %temp4, align 1
  %printf = call ptr (ptr, ...) @printf(ptr @format_str.1, ptr %temp4)
  ret i32 0
}

define [256 x i8] @func([256 x i8] %0) {
entry:
  %param1 = alloca [256 x i8], align 1
  store [256 x i8] %0, ptr %param1, align 1
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, ptr %param1)
  %temp1_load = load [256 x i8], ptr %param1, align 1
  ret [256 x i8] %temp1_load
}

declare i32 @sprintf(ptr, ptr, ...)
