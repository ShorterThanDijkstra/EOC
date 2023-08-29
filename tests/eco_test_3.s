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
	movq	$9, %rcx
	addq	$8, %rcx
	movq	$10, %rcx
	movq	%rcx, %rdx
	addq	$11, %rdx
	movq	$15, %rcx
	addq	$16, %rcx
	addq	%rdx, %rcx
	addq	$6, %rcx
	addq	$5, %rcx
	leaq	id.123(%rip), %rcx
	movq	$4, %rdi
	callq	*%rcx
	movq	%rax, %rcx
	movq	$38, %rax
	addq	%rcx, %rax
	jmp mainconclusion

	.align 8
mainconclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq

	.align 8
id.123:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.123start

	.align 8
id.123start:
	movq	%rdi, %rcx
	movq	%rcx, %rax
	jmp id.123conclusion

	.align 8
id.123conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



