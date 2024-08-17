
target triple = "riscv32-unknown-unknown-elf"

@.str = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.2 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.3 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.4 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.5 = private unnamed_addr constant [6 x i8] c"false\00", align 1

define dso_local void @print(ptr noundef %0) local_unnamed_addr #0  {
  
  %2 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull @.str, ptr noundef %0) #11
  ret void
}

declare  dso_local i32 @printf(ptr noundef, ...) local_unnamed_addr #1

define dso_local void @println(ptr noundef %0) local_unnamed_addr #0  {
  
  %2 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull @.str.1, ptr noundef %0) #11
  ret void
}

define dso_local void @printInt(i32 noundef %0) local_unnamed_addr #0  {
  
  %2 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull @.str.2, i32 noundef %0) #11
  ret void
}

define dso_local void @printlnInt(i32 noundef %0) local_unnamed_addr #0  {
  
  %2 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull @.str.3, i32 noundef %0) #11
  ret void
}

define dso_local ptr @getString() local_unnamed_addr #2  {
  %1 = tail call dereferenceable_or_null(4096) ptr @malloc(i32 noundef 4096) #12
  
  %2 = tail call i32 (ptr, ...) @scanf(ptr noundef nonnull @.str, ptr noundef %1) #13
  ret ptr %1
}

declare dso_local noalias noundef ptr @malloc(i32 noundef) local_unnamed_addr #4

declare dso_local noundef i32 @scanf(ptr nocapture noundef readonly, ...) local_unnamed_addr #5

define dso_local i32 @getInt() local_unnamed_addr #2  {
  %1 = alloca i32, align 4
  
  %2 = call i32 (ptr, ...) @scanf(ptr noundef nonnull @.str.2, ptr noundef nonnull %1) #13
  %3 = load i32, ptr %1, align 4

  ret i32 %3
}

define dso_local noalias ptr @toString(i32 noundef %0) local_unnamed_addr #2  {
  
  %2 = tail call dereferenceable_or_null(64) ptr @malloc(i32 noundef 64) #12
  
  %3 = tail call i32 (ptr, ptr, ...) @sprintf(ptr noundef nonnull dereferenceable(1) %2, ptr noundef nonnull @.str.2, i32 noundef %0) #13
  ret ptr %2
}

define dso_local noundef nonnull ptr @CrazyDave..boolToString(i1 noundef zeroext %0) local_unnamed_addr {
  %2 = select i1 %0, ptr @.str.4, ptr @.str.5
  ret ptr %2
}

declare dso_local noundef i32 @sprintf(ptr noalias nocapture noundef writeonly, ptr nocapture noundef readonly, ...) local_unnamed_addr #5

define dso_local noalias nonnull ptr @CrazyDave..AllocArray(i32 noundef %0) local_unnamed_addr #6  {
  
  %2 = shl i32 %0, 2
  %3 = add nsw i32 %2, 4
  %4 = tail call ptr @malloc(i32 noundef %3) #12
  
  store i32 %0, ptr %4, align 4
  %5 = getelementptr inbounds i32, ptr %4, i32 1
  ret ptr %5
}

define dso_local i32 @CrazyDave..GetArraySize(ptr nocapture noundef readonly %0) local_unnamed_addr #7  {
  
  %2 = getelementptr inbounds i32, ptr %0, i32 -1
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}

define dso_local i32 @string.length(ptr nocapture noundef readonly %0) local_unnamed_addr #8  {
  
  %2 = tail call i32 @strlen(ptr noundef nonnull dereferenceable(1) %0) #13
  ret i32 %2
}

declare dso_local i32 @strlen(ptr nocapture noundef) local_unnamed_addr #9

define dso_local ptr @string.substring(ptr noundef %0, i32 noundef %1, i32 noundef %2) local_unnamed_addr #0  {
  
  
  
  %4 = sub nsw i32 %2, %1
  %5 = add nsw i32 %4, 2
  %6 = tail call ptr @malloc(i32 noundef %5) #12
  
  %7 = getelementptr inbounds i8, ptr %0, i32 %1
  %8 = tail call ptr @memcpy(ptr noundef %6, ptr noundef %7, i32 noundef %4) #11
  %9 = getelementptr inbounds i8, ptr %6, i32 %4
  store i8 0, ptr %9, align 1
  ret ptr %6
}

declare  dso_local ptr @memcpy(ptr noundef, ptr noundef, i32 noundef) local_unnamed_addr #1

define dso_local i32 @string.parseInt(ptr nocapture noundef readonly %0) local_unnamed_addr #2  {
  %2 = alloca i32, align 4
  
  
  
  %3 = call i32 (ptr, ptr, ...) @sscanf(ptr noundef %0, ptr noundef nonnull @.str.2, ptr noundef nonnull %2) #13
  %4 = load i32, ptr %2, align 4
  
  
  ret i32 %4
}

declare dso_local noundef i32 @sscanf(ptr nocapture noundef readonly, ptr nocapture noundef readonly, ...) local_unnamed_addr #5

define dso_local i32 @string.ord(ptr nocapture noundef readonly %0, i32 noundef %1) local_unnamed_addr #7  {
  
  
  %3 = getelementptr inbounds i8, ptr %0, i32 %1
  %4 = load i8, ptr %3, align 1
  %5 = zext i8 %4 to i32
  ret i32 %5
}

define dso_local ptr @string.add(ptr noundef %0, ptr noundef %1) local_unnamed_addr #0  {
  
  
  %3 = tail call i32 @strlen(ptr noundef nonnull dereferenceable(1) %0) #13
  
  %4 = tail call i32 @strlen(ptr noundef nonnull dereferenceable(1) %1) #13
  
  %5 = add nsw i32 %4, %3
  %6 = add nsw i32 %5, 1
  %7 = tail call ptr @malloc(i32 noundef %6) #12
  
  %8 = tail call ptr @memcpy(ptr noundef %7, ptr noundef %0, i32 noundef %3) #11
  %9 = getelementptr inbounds i8, ptr %7, i32 %3
  %10 = add nsw i32 %4, 1
  %11 = tail call ptr @memcpy(ptr noundef %9, ptr noundef %1, i32 noundef %10) #11
  %12 = getelementptr inbounds i8, ptr %7, i32 %5
  store i8 0, ptr %12, align 1
  ret ptr %7
}

define dso_local zeroext i1 @string.eq(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1) local_unnamed_addr #8  {
  
  
  %3 = tail call i32 @strcmp(ptr noundef nonnull dereferenceable(1) %0, ptr noundef nonnull dereferenceable(1) %1) #13
  %4 = icmp eq i32 %3, 0
  ret i1 %4
}

declare dso_local i32 @strcmp(ptr nocapture noundef, ptr nocapture noundef) local_unnamed_addr #9

define dso_local zeroext i1 @string.ne(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1) local_unnamed_addr #8  {
  
  
  %3 = tail call i32 @strcmp(ptr noundef nonnull dereferenceable(1) %0, ptr noundef nonnull dereferenceable(1) %1) #13
  %4 = icmp ne i32 %3, 0
  ret i1 %4
}

define dso_local zeroext i1 @string.lt(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1) local_unnamed_addr #8  {
  
  
  %3 = tail call i32 @strcmp(ptr noundef nonnull dereferenceable(1) %0, ptr noundef nonnull dereferenceable(1) %1) #13
  %4 = icmp slt i32 %3, 0
  ret i1 %4
}

define dso_local zeroext i1 @string.le(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1) local_unnamed_addr #8  {
  
  
  %3 = tail call i32 @strcmp(ptr noundef nonnull dereferenceable(1) %0, ptr noundef nonnull dereferenceable(1) %1) #13
  %4 = icmp slt i32 %3, 1
  ret i1 %4
}

define dso_local zeroext i1 @string.gt(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1) local_unnamed_addr #8  {
  
  
  %3 = tail call i32 @strcmp(ptr noundef nonnull dereferenceable(1) %0, ptr noundef nonnull dereferenceable(1) %1) #13
  %4 = icmp sgt i32 %3, 0
  ret i1 %4
}

define dso_local zeroext i1 @string.ge(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1) local_unnamed_addr #8  {
  
  
  %3 = tail call i32 @strcmp(ptr noundef nonnull dereferenceable(1) %0, ptr noundef nonnull dereferenceable(1) %1) #13
  %4 = icmp sgt i32 %3, -1
  ret i1 %4
}


attributes #0 = { nounwind "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtin-memcpy" "no-builtin-printf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+a,+c,+d,+f,+m,+relax,-save-restore" }
attributes #1 = { "frame-pointer"="none" "no-builtin-memcpy" "no-builtin-printf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+a,+c,+d,+f,+m,+relax,-save-restore" }
attributes #2 = { nofree nounwind "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtin-memcpy" "no-builtin-printf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+a,+c,+d,+f,+m,+relax,-save-restore" }
attributes #3 = { argmemonly mustprogress nocallback nofree nosync nounwind willreturn }
attributes #4 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" "frame-pointer"="none" "no-builtin-memcpy" "no-builtin-printf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+a,+c,+d,+f,+m,+relax,-save-restore" }
attributes #5 = { nofree nounwind "frame-pointer"="none" "no-builtin-memcpy" "no-builtin-printf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+a,+c,+d,+f,+m,+relax,-save-restore" }
attributes #6 = { mustprogress nofree nounwind willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtin-memcpy" "no-builtin-printf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+a,+c,+d,+f,+m,+relax,-save-restore" }
attributes #7 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtin-memcpy" "no-builtin-printf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+a,+c,+d,+f,+m,+relax,-save-restore" }
attributes #8 = { argmemonly mustprogress nofree nounwind readonly willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtin-memcpy" "no-builtin-printf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+a,+c,+d,+f,+m,+relax,-save-restore" }
attributes #9 = { argmemonly mustprogress nofree nounwind readonly willreturn "frame-pointer"="none" "no-builtin-memcpy" "no-builtin-printf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+a,+c,+d,+f,+m,+relax,-save-restore" }
attributes #10 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #11 = { nobuiltin nounwind "no-builtin-memcpy" "no-builtin-printf" }
attributes #12 = { allocsize(0) "no-builtin-memcpy" "no-builtin-printf" }
attributes #13 = { "no-builtin-memcpy" "no-builtin-printf" }
attributes #14 = { nounwind }