	.text
	.globl	print
	.p2align	1
print:
	mv	a1, a0
	lui	a0, %hi(.L.str)
	addi	a0, a0, %lo(.L.str)
	tail	printf
.Lfunc_end0:
	.globl	println
	.p2align	1
println:
	mv	a1, a0
	lui	a0, %hi(.L.str.1)
	addi	a0, a0, %lo(.L.str.1)
	tail	printf
.Lfunc_end1:
	.globl	printInt
	.p2align	1
printInt:
	mv	a1, a0
	lui	a0, %hi(.L.str.2)
	addi	a0, a0, %lo(.L.str.2)
	tail	printf
.Lfunc_end2:
	.globl	printlnInt
	.p2align	1
printlnInt:
	mv	a1, a0
	lui	a0, %hi(.L.str.3)
	addi	a0, a0, %lo(.L.str.3)
	tail	printf
.Lfunc_end3:
	.globl	getString
	.p2align	1
getString:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	lui	a0, 1
	call	malloc
	mv	a1, a0
	sw	a1, 8(sp)
	lui	a0, %hi(.L.str)
	addi	a0, a0, %lo(.L.str)
	call	scanf
	lw	a0, 8(sp)
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end4:
	.globl	getInt
	.p2align	1
getInt:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	lui	a0, %hi(.L.str.2)
	addi	a0, a0, %lo(.L.str.2)
	addi	a1, sp, 8
	call	scanf
	lw	a0, 8(sp)
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end5:
	.globl	toString
	.p2align	1
toString:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	sw	a0, 4(sp)
	li	a0, 64
	call	malloc
	lw	a2, 4(sp)
	sw	a0, 8(sp)
	lui	a1, %hi(.L.str.2)
	addi	a1, a1, %lo(.L.str.2)
	call	sprintf
	lw	a0, 8(sp)
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end6:
	.globl	CrazyDave..boolToString
	.p2align	1
CrazyDave..boolToString:
	addi	sp, sp, -16
	lui	a1, %hi(.L.str.5)
	addi	a1, a1, %lo(.L.str.5)
	sw	a1, 8(sp)
	lui	a1, %hi(.L.str.4)
	addi	a2, a1, %lo(.L.str.4)
	li	a1, 0
	sw	a2, 12(sp)
	bne	a0, a1, .LBB7_2
	lw	a0, 8(sp)
	sw	a0, 12(sp)
.LBB7_2:
	lw	a0, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end7:
	.globl	CrazyDave..AllocArray
	.p2align	1
CrazyDave..AllocArray:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	sw	a0, 8(sp)
	slli	a0, a0, 2
	addi	a0, a0, 4
	call	malloc
	lw	a1, 8(sp)
	sw	a1, 0(a0)
	addi	a0, a0, 4
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end8:
	.globl	CrazyDave..GetArraySize
	.p2align	1
CrazyDave..GetArraySize:
	lw	a0, -4(a0)
	ret
.Lfunc_end9:
	.globl	string.length
	.p2align	1
string.length:
	tail	strlen
.Lfunc_end10:
	.globl	string.substring
	.p2align	1
string.substring:
	addi	sp, sp, -32
	sw	ra, 28(sp)
	sw	a1, 12(sp)
	sw	a0, 16(sp)
	sub	a0, a2, a1
	sw	a0, 20(sp)
	addi	a0, a0, 2
	call	malloc
	lw	a3, 12(sp)
	lw	a1, 16(sp)
	lw	a2, 20(sp)
	sw	a0, 24(sp)
	add	a1, a1, a3
	call	memcpy
	lw	a1, 20(sp)
	lw	a0, 24(sp)
	add	a2, a0, a1
	li	a1, 0
	sb	a1, 0(a2)
	lw	ra, 28(sp)
	addi	sp, sp, 32
	ret
.Lfunc_end11:
	.globl	string.parseInt
	.p2align	1
string.parseInt:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	lui	a1, %hi(.L.str.2)
	addi	a1, a1, %lo(.L.str.2)
	addi	a2, sp, 8
	call	sscanf
	lw	a0, 8(sp)
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end12:
	.globl	string.ord
	.p2align	1
string.ord:
	add	a0, a0, a1
	lbu	a0, 0(a0)
	ret
.Lfunc_end13:
	.globl	string.add
	.p2align	1
string.add:
	addi	sp, sp, -32
	sw	ra, 28(sp)
	sw	a1, 16(sp)
	sw	a0, 4(sp)
	call	strlen
	mv	a1, a0
	lw	a0, 16(sp)
	sw	a1, 8(sp)
	call	strlen
	lw	a1, 8(sp)
	sw	a0, 12(sp)
	add	a0, a0, a1
	sw	a0, 20(sp)
	addi	a0, a0, 1
	call	malloc
	lw	a1, 4(sp)
	lw	a2, 8(sp)
	sw	a0, 24(sp)
	call	memcpy
	lw	a3, 8(sp)
	lw	a2, 12(sp)
	lw	a1, 16(sp)
	lw	a0, 24(sp)
	add	a0, a0, a3
	addi	a2, a2, 1
	call	memcpy
	lw	a1, 20(sp)
	lw	a0, 24(sp)
	add	a2, a0, a1
	li	a1, 0
	sb	a1, 0(a2)
	lw	ra, 28(sp)
	addi	sp, sp, 32
	ret
.Lfunc_end14:
	.globl	string.eq
	.p2align	1
string.eq:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	seqz	a0, a0
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end15:
	.globl	string.ne
	.p2align	1
string.ne:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	snez	a0, a0
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end16:
	.globl	string.lt
	.p2align	1
string.lt:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	srli	a0, a0, 31
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end17:
	.globl	string.le
	.p2align	1
string.le:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	slti	a0, a0, 1
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end18:
	.globl	string.gt
	.p2align	1
string.gt:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	mv	a1, a0
	li	a0, 0
	slt	a0, a0, a1
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end19:
	.globl	string.ge
	.p2align	1
string.ge:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	not	a0, a0
	srli	a0, a0, 31
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end20:
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%s"
.L.str.1:
	.asciz	"%s\n"
.L.str.2:
	.asciz	"%d"
.L.str.3:
	.asciz	"%d\n"
.L.str.4:
	.asciz	"true"
.L.str.5:
	.asciz	"false"
