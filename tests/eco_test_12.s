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
	movq	$0, 2(%r15)
	movq	$0, 3(%r15)
	movq	$0, 4(%r15)
	movq	$0, 5(%r15)
	movq	$0, 6(%r15)
	movq	$0, 7(%r15)
	movq	$0, 8(%r15)
	movq	$0, 9(%r15)
	movq	$0, 10(%r15)
	movq	$0, 11(%r15)
	movq	$0, 12(%r15)
	movq	$0, 13(%r15)
	movq	$0, 14(%r15)
	movq	$0, 15(%r15)
	addq	$16, %r15
	jmp mainstart

	.align 8
mainstart:
	movq	%rdi, %rcx
	movq	free_ptr(%rip), %rcx
	addq	$16, %rcx
	movq	fromspace_end(%rip), %rdx
	cmpq	%rdx, %rcx
	jl block59473
	jmp block59474

	.align 8
block59474:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59472

	.align 8
block59473:
	movq	$0, %rcx
	jmp block59472

	.align 8
block59472:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 1(%r15)
	movq	1(%r15), %rax
	movq	%rax, 0(%r15)
	movq	0(%r15), %r11
	movq	8(%r11), %rcx
	movq	$2, %rdx
	movq	$0, %r8
	movq	%rdx, %rdi
	movq	$40, %rdx
	addq	%rdx, %rdi
	movq	%r8, %rdx
	addq	%rdi, %rdx
	movq	0(%r15), %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rax
	subq	$16, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	jmp *%rax

	.align 8
mainconclusion:
	subq	$16, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
id.42:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.42start

	.align 8
id.42start:
	movq	%rdi, %rcx
	movq	%rsi, %rcx
	movq	%rcx, %rax
	jmp id.42conclusion

	.align 8
id.42conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



