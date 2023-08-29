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
	addq	$6, %r15
	jmp mainstart

	.align 8
mainstart:
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
	movq	%r11, 4(%r15)
	movq	4(%r15), %rax
	movq	%rax, 5(%r15)
	movq	5(%r15), %r11
	movq	8(%r11), %rcx
	movq	5(%r15), %rdi
	movq	$42, %rsi
	callq	*%rcx
	movq	%rax, 3(%r15)
	movq	3(%r15), %r11
	movq	8(%r11), %rcx
	movq	3(%r15), %rdi
	movq	$444, %rsi
	movq	%rcx, %rax
	subq	$6, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	jmp *%rax

	.align 8
mainconclusion:
	subq	$6, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
lambda.40:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	subq	$8, %rsp
	movq	$0, 0(%r15)
	movq	$0, 1(%r15)
	movq	$0, 2(%r15)
	addq	$3, %r15
	jmp lambda.40start

	.align 8
lambda.40start:
	movq	%rdi, 2(%r15)
	movq	%rsi, %rdx
	movq	2(%r15), %r11
	movq	16(%r11), %rcx
	movq	%rcx, %rax
	jmp lambda.40conclusion

	.align 8
lambda.40conclusion:
	subq	$3, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
lambda.41:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	subq	$8, %rsp
	movq	$0, 0(%r15)
	movq	$0, 1(%r15)
	addq	$2, %r15
	jmp lambda.41start

	.align 8
lambda.41start:
	movq	%rdi, 0(%r15)
	movq	%rsi, %rcx
	movq	free_ptr(%rip), %rdx
	addq	$24, %rdx
	movq	fromspace_end(%rip), %rsi
	cmpq	%rsi, %rdx
	jl block59470
	jmp block59471

	.align 8
block59471:
	movq	%r15, %rdi
	movq	$24, %rsi
	callq	collect
	jmp block59469

	.align 8
block59470:
	movq	$0, %rdx
	jmp block59469

	.align 8
block59469:
	movq	free_ptr(%rip), %r11
	addq	$16, free_ptr(%rip)
	movq	$4099, 0(%r11)
	movq	%r11, 1(%r15)
	movq	1(%r15), %r11
	movq	%rcx, 16(%r11)
	movq	$0, %rcx
	movq	1(%r15), %rax
	jmp lambda.41conclusion

	.align 8
lambda.41conclusion:
	subq	$2, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq



