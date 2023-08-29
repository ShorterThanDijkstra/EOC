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
	callq	read_int
	movq	%rax, %rcx
	movq	$11, %rax
	cmpq	%rcx, %rax
	jl block59511
	jmp block59512

	.align 8
block59512:
	movq	$14, %rcx
	jmp block59510

	.align 8
block59511:
	movq	$13, %rcx
	jmp block59510

	.align 8
block59510:
	movq	$11, %rdx
	addq	%rcx, %rdx
	movq	$13, %rcx
	addq	%rdx, %rcx
	movq	free_ptr(%rip), %rdx
	addq	$16, %rdx
	movq	fromspace_end(%rip), %rsi
	cmpq	%rsi, %rdx
	jl block59508
	jmp block59509

	.align 8
block59509:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59507

	.align 8
block59508:
	movq	$0, %rdx
	jmp block59507

	.align 8
block59507:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 0(%r15)
	movq	0(%r15), %rax
	movq	%rax, 1(%r15)
	movq	1(%r15), %r11
	movq	8(%r11), %rdx
	addq	$5, %rcx
	movq	1(%r15), %rdi
	movq	%rcx, %rsi
	movq	%rdx, %rax
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
id.167:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.167start

	.align 8
id.167start:
	movq	%rdi, %rcx
	movq	%rsi, %rcx
	movq	%rcx, %rax
	jmp id.167conclusion

	.align 8
id.167conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



