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
	movq	%rcx, %rdx
	addq	$16, %rdx
	movq	fromspace_end(%rip), %rcx
	cmpq	%rcx, %rdx
	jl block59487
	jmp block59488

	.align 8
block59488:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59486

	.align 8
block59487:
	movq	$0, %rcx
	jmp block59486

	.align 8
block59486:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 0(%r15)
	movq	0(%r15), %rax
	movq	%rax, 1(%r15)
	movq	1(%r15), %r11
	movq	8(%r11), %rcx
	movq	$11, %rdx
	callq	read_int
	movq	$17, %rdx
	cmpq	$13, %rdx
	jl block59484
	jmp block59485

	.align 8
block59485:
	callq	read_int
	movq	%rax, %rdx
	movq	$42, %rdx
	movq	1(%r15), %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rax
	subq	$16, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	jmp *%rax

	.align 8
block59484:
	callq	read_int
	movq	$42, %rdx
	movq	1(%r15), %rdi
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
id.91:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.91start

	.align 8
id.91start:
	movq	%rdi, %rcx
	movq	%rsi, %rcx
	movq	%rcx, %rax
	jmp id.91conclusion

	.align 8
id.91conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



