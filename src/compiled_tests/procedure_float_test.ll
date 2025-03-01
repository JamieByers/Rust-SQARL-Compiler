; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

@format_str = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %function_result1 = call i32 @func(double 1.231230e+02)
  ret i32 0
}

define i32 @func(double %0) {
entry:
  %param1 = alloca double, align 8
  store double %0, ptr %param1, align 8
  %var_value = load double, ptr %param1, align 8
  %printf = call ptr (ptr, ...) @printf(ptr @format_str, double %var_value)
  ret i32 1
}
