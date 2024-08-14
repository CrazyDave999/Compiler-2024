; ModuleID = 'builtin.c'
source_filename = "builtin.c"
target datalayout = "e-m:e-p:32:32-i64:64-n32-S128"
target triple = "riscv32-unknown-unknown-elf"

@.str = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.2 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.3 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

; Function Attrs: noinline nounwind optnone
define dso_local void @print(ptr noundef %0) #0 {
  %2 = alloca ptr, align 4
  store ptr %0, ptr %2, align 4
  %3 = load ptr, ptr %2, align 4
  %4 = call i32 (ptr, ...) @printf(ptr noundef @.str, ptr noundef %3) #2
  ret void
}

declare dso_local i32 @printf(ptr noundef, ...) #1

; Function Attrs: noinline nounwind optnone
define dso_local void @println(ptr noundef %0) #0 {
  %2 = alloca ptr, align 4
  store ptr %0, ptr %2, align 4
  %3 = load ptr, ptr %2, align 4
  %4 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, ptr noundef %3) #2
  ret void
}

; Function Attrs: noinline nounwind optnone
define dso_local void @printInt(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  %4 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %3) #2
  ret void
}

; Function Attrs: noinline nounwind optnone
define dso_local void @printlnInt(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  %4 = call i32 (ptr, ...) @printf(ptr noundef @.str.3, i32 noundef %3) #2
  ret void
}

; Function Attrs: noinline nounwind optnone
define dso_local ptr @getString() #0 {
  %1 = alloca ptr, align 4
  %2 = call ptr @malloc(i32 noundef 4096) #2
  store ptr %2, ptr %1, align 4
  %3 = load ptr, ptr %1, align 4
  %4 = call i32 (ptr, ...) @scanf(ptr noundef @.str, ptr noundef %3) #3
  %5 = load ptr, ptr %1, align 4
  ret ptr %5
}

declare dso_local ptr @malloc(i32 noundef) #1

declare dso_local i32 @scanf(ptr noundef, ...) #1

; Function Attrs: noinline nounwind optnone
define dso_local i32 @getInt() #0 {
  %1 = alloca i32, align 4
  %2 = call i32 (ptr, ...) @scanf(ptr noundef @.str.2, ptr noundef %1) #3
  %3 = load i32, ptr %1, align 4
  ret i32 %3
}

; Function Attrs: noinline nounwind optnone
define dso_local ptr @toString(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  %3 = alloca ptr, align 4
  store i32 %0, ptr %2, align 4
  %4 = call ptr @malloc(i32 noundef 16) #2
  store ptr %4, ptr %3, align 4
  %5 = load ptr, ptr %3, align 4
  %6 = load i32, ptr %2, align 4
  %7 = call i32 (ptr, ptr, ...) @sprintf(ptr noundef %5, ptr noundef @.str.2, i32 noundef %6) #3
  %8 = load ptr, ptr %3, align 4
  ret ptr %8
}

declare dso_local i32 @sprintf(ptr noundef, ptr noundef, ...) #1

; Function Attrs: noinline nounwind optnone
define dso_local ptr @string.string() #0 {
  %1 = alloca ptr, align 4
  %2 = call ptr @malloc(i32 noundef 1) #2
  store ptr %2, ptr %1, align 4
  %3 = load ptr, ptr %1, align 4
  %4 = getelementptr inbounds i8, ptr %3, i32 0
  store i8 0, ptr %4, align 1
  %5 = load ptr, ptr %1, align 4
  ret ptr %5
}

; Function Attrs: noinline nounwind optnone
define dso_local i32 @string.length(ptr noundef %0) #0 {
  %2 = alloca ptr, align 4
  store ptr %0, ptr %2, align 4
  %3 = load ptr, ptr %2, align 4
  %4 = call i32 @strlen(ptr noundef %3) #2
  ret i32 %4
}

declare dso_local i32 @strlen(ptr noundef) #1

; Function Attrs: noinline nounwind optnone
define dso_local ptr @string.substring(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 {
  %4 = alloca ptr, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca ptr, align 4
  store ptr %0, ptr %4, align 4
  store i32 %1, ptr %5, align 4
  store i32 %2, ptr %6, align 4
  %9 = load i32, ptr %6, align 4
  %10 = load i32, ptr %5, align 4
  %11 = sub nsw i32 %9, %10
  store i32 %11, ptr %7, align 4
  %12 = load i32, ptr %7, align 4
  %13 = add nsw i32 %12, 1
  %14 = call ptr @malloc(i32 noundef %13) #2
  store ptr %14, ptr %8, align 4
  %15 = load ptr, ptr %8, align 4
  %16 = load ptr, ptr %4, align 4
  %17 = load i32, ptr %5, align 4
  %18 = getelementptr inbounds i8, ptr %16, i32 %17
  %19 = load i32, ptr %7, align 4
  %20 = call ptr @memcpy(ptr noundef %15, ptr noundef %18, i32 noundef %19) #2
  %21 = load ptr, ptr %8, align 4
  %22 = load i32, ptr %7, align 4
  %23 = getelementptr inbounds i8, ptr %21, i32 %22
  store i8 0, ptr %23, align 1
  %24 = load ptr, ptr %8, align 4
  ret ptr %24
}

declare dso_local ptr @memcpy(ptr noundef, ptr noundef, i32 noundef) #1

; Function Attrs: noinline nounwind optnone
define dso_local i32 @string.parseInt(ptr noundef %0) #0 {
  %2 = alloca ptr, align 4
  %3 = alloca i32, align 4
  store ptr %0, ptr %2, align 4
  %4 = load ptr, ptr %2, align 4
  %5 = call i32 (ptr, ptr, ...) @sscanf(ptr noundef %4, ptr noundef @.str.2, ptr noundef %3) #3
  %6 = load i32, ptr %3, align 4
  ret i32 %6
}

declare dso_local i32 @sscanf(ptr noundef, ptr noundef, ...) #1
