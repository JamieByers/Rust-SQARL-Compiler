; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  ret i32 0
}

define i32 @func(i32 %0) {
entry:
  %param1 = alloca i32, align 4
  store i32 %0, ptr %param1, align 4
  %display_str_temp1 = alloca [7 x i8], align 1
  store [7 x i8] c"Tesing\00", ptr %display_str_temp1, align 1
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, ptr %display_str_temp1)
  ret i32 1
}
