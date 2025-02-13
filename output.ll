; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %message = alloca [13 x i8], align 1
  store [13 x i8] c"Hello World!\00", ptr %message, align 1
  %var_value = load [13 x i8], ptr %message, align 1
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, [13 x i8] %var_value)
  ret i32 0
}
