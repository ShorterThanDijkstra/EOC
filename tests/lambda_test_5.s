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
	addq	$8, %r15
	jmp mainstart

	.align 8
mainstart:
	movq	free_ptr(%rip), %rcx
	addq	$16, %rcx
	movq	fromspace_end(%rip), %rdx
	cmpq	%rdx, %rcx
	jl block59491
	jmp block59492

	.align 8
block59492:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59490

	.align 8
block59491:
	movq	$0, %rcx
	jmp block59490

	.align 8
block59490:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 4(%r15)
	movq	4(%r15), %rax
	movq	%rax, 5(%r15)
	movq	5(%r15), %r11
	movq	8(%r11), %rcx
	movq	5(%r15), %rdi
	movq	$5, %rsi
	callq	*%rcx
	movq	%rax, 6(%r15)
	movq	free_ptr(%rip), %rcx
	addq	$16, %rcx
	movq	fromspace_end(%rip), %rdx
	cmpq	%rdx, %rcx
	jl block59488
	jmp block59489

	.align 8
block59489:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59487

	.align 8
block59488:
	movq	$0, %rcx
	jmp block59487

	.align 8
block59487:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 3(%r15)
	movq	3(%r15), %rax
	movq	%rax, 7(%r15)
	movq	7(%r15), %r11
	movq	8(%r11), %rcx
	movq	7(%r15), %rdi
	movq	$3, %rsi
	callq	*%rcx
	movq	%rax, 2(%r15)
	movq	6(%r15), %r11
	movq	8(%r11), %rcx
	movq	6(%r15), %rdi
	movq	$11, %rsi
	callq	*%rcx
	movq	%rax, %rcx
	movq	2(%r15), %r11
	movq	8(%r11), %rdx
	movq	2(%r15), %rdi
	movq	$15, %rsi
	callq	*%rdx
	movq	%rax, %rdx
	movq	%rcx, %rax
	addq	%rdx, %rax
	jmp mainconclusion

	.align 8
mainconclusion:
	subq	$8, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
f.91:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	subq	$8, %rsp
	movq	$0, 0(%r15)
	movq	$0, 1(%r15)
	addq	$2, %r15
	jmp f.91start

	.align 8
f.91start:
	movq	%rdi, %rcx
	movq	%rsi, %rcx
	movq	$4, %rdx
	movq	free_ptr(%rip), %rsi
	addq	$32, %rsi
	movq	fromspace_end(%rip), %rdi
	cmpq	%rdi, %rsi
	jl block59485
	jmp block59486

	.align 8
block59486:
	movq	%r15, %rdi
	movq	$32, %rsi
	callq	collect
	jmp block59484

	.align 8
block59485:
	movq	$0, %rsi
	jmp block59484

	.align 8
block59484:
	movq	free_ptr(%rip), %r11
	addq	$24, free_ptr(%rip)
	movq	$4101, 0(%r11)
	movq	%r11, 1(%r15)
	movq	1(%r15), %r11
	movq	%rdx, 16(%r11)
	movq	$0, %rdx
	movq	1(%r15), %r11
	movq	%rcx, 24(%r11)
	movq	$0, %rcx
	movq	1(%r15), %rax
	jmp f.91conclusion

	.align 8
f.91conclusion:
	subq	$2, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
lambda.99:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	subq	$8, %rsp
	movq	$0, 0(%r15)
	addq	$1, %r15
	jmp lambda.99start

	.align 8
lambda.99start:
	movq	%rdi, 0(%r15)
	movq	0(%r15), %r11
	movq	16(%r11), %rdx
	movq	0(%r15), %r11
	movq	24(%r11), %rdi
	movq	%rdx, %rcx
	addq	%rsi, %rcx
	movq	%rdi, %rax
	addq	%rcx, %rax
	jmp lambda.99conclusion

	.align 8
lambda.99conclusion:
	subq	$1, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq



