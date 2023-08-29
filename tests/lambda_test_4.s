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
	jl block59482
	jmp block59483

	.align 8
block59483:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59481

	.align 8
block59482:
	movq	$0, %rcx
	jmp block59481

	.align 8
block59481:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 7(%r15)
	movq	7(%r15), %rax
	movq	%rax, 3(%r15)
	movq	3(%r15), %r11
	movq	8(%r11), %rcx
	movq	3(%r15), %rdi
	movq	$5, %rsi
	callq	*%rcx
	movq	%rax, 6(%r15)
	movq	free_ptr(%rip), %rcx
	addq	$16, %rcx
	movq	fromspace_end(%rip), %rdx
	cmpq	%rdx, %rcx
	jl block59479
	jmp block59480

	.align 8
block59480:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59478

	.align 8
block59479:
	movq	$0, %rcx
	jmp block59478

	.align 8
block59478:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 4(%r15)
	movq	4(%r15), %rax
	movq	%rax, 2(%r15)
	movq	2(%r15), %r11
	movq	8(%r11), %rcx
	movq	2(%r15), %rdi
	movq	$3, %rsi
	callq	*%rcx
	movq	%rax, 5(%r15)
	movq	6(%r15), %r11
	movq	8(%r11), %rcx
	movq	6(%r15), %rdi
	movq	$11, %rsi
	callq	*%rcx
	movq	%rax, %rcx
	movq	5(%r15), %r11
	movq	8(%r11), %rdx
	movq	5(%r15), %rdi
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
f6.55:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	subq	$8, %rsp
	movq	$0, 0(%r15)
	movq	$0, 1(%r15)
	addq	$2, %r15
	jmp f6.55start

	.align 8
f6.55start:
	movq	%rdi, %rcx
	movq	%rsi, %rcx
	movq	$4, %rdx
	movq	free_ptr(%rip), %rsi
	addq	$32, %rsi
	movq	fromspace_end(%rip), %rdi
	cmpq	%rdi, %rsi
	jl block59476
	jmp block59477

	.align 8
block59477:
	movq	%r15, %rdi
	movq	$32, %rsi
	callq	collect
	jmp block59475

	.align 8
block59476:
	movq	$0, %rsi
	jmp block59475

	.align 8
block59475:
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
	jmp f6.55conclusion

	.align 8
f6.55conclusion:
	subq	$2, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
lambda.63:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	subq	$8, %rsp
	movq	$0, 0(%r15)
	addq	$1, %r15
	jmp lambda.63start

	.align 8
lambda.63start:
	movq	%rdi, 0(%r15)
	movq	0(%r15), %r11
	movq	16(%r11), %rdx
	movq	0(%r15), %r11
	movq	24(%r11), %rdi
	movq	%rdx, %rcx
	addq	%rsi, %rcx
	movq	%rdi, %rax
	addq	%rcx, %rax
	jmp lambda.63conclusion

	.align 8
lambda.63conclusion:
	subq	$1, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq



