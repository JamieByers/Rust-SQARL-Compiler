; ModuleID = 'SQARL Compiler'
source_filename = "SQARL Compiler"

declare ptr @printf(ptr, ...)

define i32 @main() {
entry:
  %message = alloca [13 x i8], align 1
  store [13 x i8] c"Hello World!\00", ptr %message, align 1
  %temp1_load = load [13 x i8], ptr %message, align 1
  %printf = call ptr (ptr, ...) @printf([13 x i8] %temp1_load)
  ret i32 0
}
