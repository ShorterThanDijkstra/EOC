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
	leaq	add.119(%rip), %rcx
	movq	$40, %rdi
	movq	$2, %rsi
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
add.119:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp add.119start

	.align 8
add.119start:
	movq	%rdi, %rcx
	movq	%rsi, %rdx
	movq	%rcx, %rax
	addq	%rdx, %rax
	jmp add.119conclusion

	.align 8
add.119conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



