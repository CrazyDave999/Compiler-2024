	.text
	.attribute	4, 16
	.attribute	5, "rv32i2p0_m2p0_a2p0_c2p0"
	.file	"builtin.ll"
	.globl	print
	.p2align	1
	.type	print,@function
print:
	mv	a1, a0
	lui	a0, %hi(.L.str)
	addi	a0, a0, %lo(.L.str)
	tail	printf
.Lfunc_end0:
	.size	print, .Lfunc_end0-print

	.globl	println
	.p2align	1
	.type	println,@function
println:
	mv	a1, a0
	lui	a0, %hi(.L.str.1)
	addi	a0, a0, %lo(.L.str.1)
	tail	printf
.Lfunc_end1:
	.size	println, .Lfunc_end1-println

	.globl	printInt
	.p2align	1
	.type	printInt,@function
printInt:
	mv	a1, a0
	lui	a0, %hi(.L.str.2)
	addi	a0, a0, %lo(.L.str.2)
	tail	printf
.Lfunc_end2:
	.size	printInt, .Lfunc_end2-printInt

	.globl	printlnInt
	.p2align	1
	.type	printlnInt,@function
printlnInt:
	mv	a1, a0
	lui	a0, %hi(.L.str.3)
	addi	a0, a0, %lo(.L.str.3)
	tail	printf
.Lfunc_end3:
	.size	printlnInt, .Lfunc_end3-printlnInt

	.globl	getString
	.p2align	1
	.type	getString,@function
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
	.size	getString, .Lfunc_end4-getString

	.globl	getInt
	.p2align	1
	.type	getInt,@function
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
	.size	getInt, .Lfunc_end5-getInt

	.globl	toString
	.p2align	1
	.type	toString,@function
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
	.size	toString, .Lfunc_end6-toString

	.globl	CrazyDave..boolToString
	.p2align	1
	.type	CrazyDave..boolToString,@function
CrazyDave..boolToString:
	.cfi_startproc
	addi	sp, sp, -16
	.cfi_def_cfa_offset 16
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
	.size	CrazyDave..boolToString, .Lfunc_end7-CrazyDave..boolToString
	.cfi_endproc

	.globl	CrazyDave..AllocArray
	.p2align	1
	.type	CrazyDave..AllocArray,@function
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
	.size	CrazyDave..AllocArray, .Lfunc_end8-CrazyDave..AllocArray

	.globl	CrazyDave..GetArraySize
	.p2align	1
	.type	CrazyDave..GetArraySize,@function
CrazyDave..GetArraySize:
	lw	a0, -4(a0)
	ret
.Lfunc_end9:
	.size	CrazyDave..GetArraySize, .Lfunc_end9-CrazyDave..GetArraySize

	.globl	string.length
	.p2align	1
	.type	string.length,@function
string.length:
	tail	strlen
.Lfunc_end10:
	.size	string.length, .Lfunc_end10-string.length

	.globl	string.substring
	.p2align	1
	.type	string.substring,@function
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
	.size	string.substring, .Lfunc_end11-string.substring

	.globl	string.parseInt
	.p2align	1
	.type	string.parseInt,@function
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
	.size	string.parseInt, .Lfunc_end12-string.parseInt

	.globl	string.ord
	.p2align	1
	.type	string.ord,@function
string.ord:
	add	a0, a0, a1
	lbu	a0, 0(a0)
	ret
.Lfunc_end13:
	.size	string.ord, .Lfunc_end13-string.ord

	.globl	string.add
	.p2align	1
	.type	string.add,@function
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
	.size	string.add, .Lfunc_end14-string.add

	.globl	string.eq
	.p2align	1
	.type	string.eq,@function
string.eq:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	seqz	a0, a0
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end15:
	.size	string.eq, .Lfunc_end15-string.eq

	.globl	string.ne
	.p2align	1
	.type	string.ne,@function
string.ne:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	snez	a0, a0
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end16:
	.size	string.ne, .Lfunc_end16-string.ne

	.globl	string.lt
	.p2align	1
	.type	string.lt,@function
string.lt:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	srli	a0, a0, 31
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end17:
	.size	string.lt, .Lfunc_end17-string.lt

	.globl	string.le
	.p2align	1
	.type	string.le,@function
string.le:
	addi	sp, sp, -16
	sw	ra, 12(sp)
	call	strcmp
	slti	a0, a0, 1
	lw	ra, 12(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end18:
	.size	string.le, .Lfunc_end18-string.le

	.globl	string.gt
	.p2align	1
	.type	string.gt,@function
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
	.size	string.gt, .Lfunc_end19-string.gt

	.globl	string.ge
	.p2align	1
	.type	string.ge,@function
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
	.size	string.ge, .Lfunc_end20-string.ge

	.type	.L.str,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%s"
	.size	.L.str, 3

	.type	.L.str.1,@object
.L.str.1:
	.asciz	"%s\n"
	.size	.L.str.1, 4

	.type	.L.str.2,@object
.L.str.2:
	.asciz	"%d"
	.size	.L.str.2, 3

	.type	.L.str.3,@object
.L.str.3:
	.asciz	"%d\n"
	.size	.L.str.3, 4

	.type	.L.str.4,@object
.L.str.4:
	.asciz	"true"
	.size	.L.str.4, 5

	.type	.L.str.5,@object
.L.str.5:
	.asciz	"false"
	.size	.L.str.5, 6

	.section	".note.GNU-stack","",@progbits
	.addrsig