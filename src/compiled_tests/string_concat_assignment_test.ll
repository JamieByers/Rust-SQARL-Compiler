; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@concat_format = global [5 x i8] c"%s%s\00"
@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %example = alloca [15 x i8], align 1
  store [15 x i8] c"example string\00", ptr %example, align 1
  %buffer = alloca [13 x i8], align 1
  %lhs_array = alloca [7 x i8], align 1
  store [7 x i8] c"Hello \00", ptr %lhs_array, align 1
  %rhs_array = alloca [7 x i8], align 1
  store [7 x i8] c"world!\00", ptr %rhs_array, align 1
  %lhs_ptr = getelementptr [7 x i8], ptr %lhs_array, i32 0, i32 0
  %rhs_ptr = getelementptr [7 x i8], ptr %rhs_array, i32 0, i32 0
  %sprintf_call = call i32 (ptr, ptr, ...) @sprintf(ptr %buffer, ptr @concat_format, ptr %lhs_ptr, ptr %rhs_ptr)
  %loaded_buffer = load [13 x i8], ptr %buffer, align 1
  %example1 = alloca [13 x i8], align 1
  store [13 x i8] %loaded_buffer, ptr %example1, align 1
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, ptr %example1)
  ret i32 0
}

declare i32 @sprintf(ptr, ptr, ...)
