	.globl main
	.align 8
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	movq	$65536, %rdi
	movq	$65536, %rsi
	callq	initialize
	movq	rootstack_begin(%rip), %r15
	addq	$0, %r15
	jmp mainstart

	.align 8
mainstart:
	callq	read_int
	movq	%rax, %rcx
	movq	$0, %rax
	cmpq	%rcx, %rax
	jl block51594
	jmp block51595

	.align 8
block51595:
	movq	$11, %rdx
	jmp block51593

	.align 8
block51594:
	movq	$10, %rdx
	jmp block51593

	.align 8
block51593:
	leaq	id.136(%rip), %rcx
	movq	$32, %rdi
	callq	*%rcx
	movq	%rax, %rcx
	movq	%rdx, %rax
	addq	%rcx, %rax
	jmp mainconclusion

	.align 8
mainconclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq

	.align 8
id.136:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.136start

	.align 8
id.136start:
	movq	%rdi, %rcx
	movq	%rcx, %rax
	jmp id.136conclusion

	.align 8
id.136conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



