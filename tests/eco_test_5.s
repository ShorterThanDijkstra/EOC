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
	cmpq	$1, %rcx
	jl block51596
	jmp block51598

	.align 8
block51598:
	cmpq	$2, %rcx
	je block51597
	jmp block51596

	.align 8
block51597:
	leaq	id.142(%rip), %rcx
	movq	$2, %rdi
	callq	*%rcx
	movq	%rax, %rcx
	movq	%rdx, %rax
	addq	%rcx, %rax
	jmp mainconclusion

	.align 8
block51596:
	movq	%rdx, %rax
	subq	$1, %rax
	jmp mainconclusion

	.align 8
mainconclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq

	.align 8
id.142:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.142start

	.align 8
id.142start:
	movq	%rdi, %rcx
	movq	%rcx, %rax
	jmp id.142conclusion

	.align 8
id.142conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



