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
	callq	read_int
	movq	%rax, %rdx
	cmpq	$2, %rcx
	je block51601
	jmp block51602

	.align 8
block51602:
	leaq	id.148(%rip), %rcx
	addq	$10, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rax
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	jmp *%rax

	.align 8
block51601:
	cmpq	$2, %rdx
	je block51599
	jmp block51600

	.align 8
block51600:
	movq	%rdx, %rax
	addq	%rcx, %rax
	jmp mainconclusion

	.align 8
block51599:
	movq	%rdx, %rax
	addq	$11, %rax
	jmp mainconclusion

	.align 8
mainconclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq

	.align 8
id.148:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.148start

	.align 8
id.148start:
	movq	%rdi, %rcx
	movq	%rcx, %rax
	jmp id.148conclusion

	.align 8
id.148conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



