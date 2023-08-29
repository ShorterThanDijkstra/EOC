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
	movq	$1, %rdx
	leaq	id.164(%rip), %rcx
	cmpq	$1, %rdx
	je block51609
	jmp block51610

	.align 8
block51610:
	movq	$42, %rdx
	jmp block51608

	.align 8
block51609:
	movq	$42, %rdx
	jmp block51608

	.align 8
block51608:
	movq	%rdx, %rdi
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
id.164:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.164start

	.align 8
id.164start:
	movq	%rdi, %rcx
	movq	%rcx, %rax
	jmp id.164conclusion

	.align 8
id.164conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



