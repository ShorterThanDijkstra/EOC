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
	movq	$10, %rcx
	cmpq	$9, %rcx
	jl block59493
	jmp block59494

	.align 8
block59494:
	movq	$42, %rax
	jmp mainconclusion

	.align 8
block59493:
	movq	$24, %rax
	jmp mainconclusion

	.align 8
mainconclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq

	.align 8
id.127:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.127start

	.align 8
id.127start:
	movq	%rdi, %rcx
	movq	%rsi, %rcx
	movq	%rcx, %rax
	jmp id.127conclusion

	.align 8
id.127conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



