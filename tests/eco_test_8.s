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
	cmpq	$0, %rcx
	je block51607
	jmp block51606

	.align 8
block51607:
	callq	read_int
	movq	%rax, %rcx
	cmpq	$1, %rcx
	je block51605
	jmp block51606

	.align 8
block51606:
	leaq	id.158(%rip), %rcx
	movq	$42, %rdi
	movq	%rcx, %rax
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	jmp *%rax

	.align 8
block51605:
	leaq	id.158(%rip), %rcx
	movq	$0, %rdi
	movq	%rcx, %rax
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	jmp *%rax

	.align 8
mainconclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq

	.align 8
id.158:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.158start

	.align 8
id.158start:
	movq	%rdi, %rcx
	movq	%rcx, %rax
	jmp id.158conclusion

	.align 8
id.158conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



