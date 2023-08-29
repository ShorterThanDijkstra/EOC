	.globl main
	.align 8
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	subq	$8, %rsp
	movq	$65536, %rdi
	movq	$65536, %rsi
	callq	initialize
	movq	rootstack_begin(%rip), %r15
	movq	$0, 0(%r15)
	movq	$0, 1(%r15)
	addq	$2, %r15
	jmp mainstart

	.align 8
mainstart:
	movq	free_ptr(%rip), %rcx
	movq	%rcx, %rdx
	addq	$16, %rdx
	movq	fromspace_end(%rip), %rcx
	cmpq	%rcx, %rdx
	jl block59461
	jmp block59462

	.align 8
block59462:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59460

	.align 8
block59461:
	movq	$0, %rcx
	jmp block59460

	.align 8
block59460:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 0(%r15)
	movq	0(%r15), %rax
	movq	%rax, 1(%r15)
	movq	1(%r15), %r11
	movq	8(%r11), %rcx
	movq	1(%r15), %rdi
	movq	$42, %rsi
	movq	%rcx, %rax
	subq	$2, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	jmp *%rax

	.align 8
mainconclusion:
	subq	$2, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
id.1:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.1start

	.align 8
id.1start:
	movq	%rdi, %rcx
	movq	%rsi, %rcx
	movq	%rcx, %rax
	jmp id.1conclusion

	.align 8
id.1conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



