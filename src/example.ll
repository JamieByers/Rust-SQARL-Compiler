; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %function_result1 = call i32 @func([13 x i8] c"Hello world!\00")
  ret i32 0
}

define i32 @func(ptr %0) {
entry:
  %param1 = alloca ptr, align 8
  store ptr %0, ptr %param1, align 8
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, ptr %param1)
  ret i32 1
}
