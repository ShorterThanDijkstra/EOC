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
	cmpq	$10, %rcx
	jge block51603
	jmp block51604

	.align 8
block51604:
	movq	$12, %rax
	jmp mainconclusion

	.align 8
block51603:
	leaq	id.154(%rip), %rcx
	movq	$42, %rdi
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
id.154:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.154start

	.align 8
id.154start:
	movq	%rdi, %rcx
	movq	%rcx, %rax
	jmp id.154conclusion

	.align 8
id.154conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



