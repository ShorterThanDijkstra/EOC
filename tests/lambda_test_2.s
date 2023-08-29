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
	addq	$5, %r15
	jmp mainstart

	.align 8
mainstart:
	movq	free_ptr(%rip), %rcx
	addq	$16, %rcx
	movq	fromspace_end(%rip), %rdx
	cmpq	%rdx, %rcx
	jl block59467
	jmp block59468

	.align 8
block59468:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59466

	.align 8
block59467:
	movq	$0, %rcx
	jmp block59466

	.align 8
block59466:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 3(%r15)
	movq	3(%r15), %rax
	movq	%rax, 4(%r15)
	movq	4(%r15), %r11
	movq	8(%r11), %rcx
	movq	4(%r15), %rdi
	movq	$42, %rsi
	movq	%rcx, %rax
	subq	$5, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	jmp *%rax

	.align 8
mainconclusion:
	subq	$5, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
doubleid.12:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	subq	$8, %rsp
	movq	$0, 0(%r15)
	movq	$0, 1(%r15)
	movq	$0, 2(%r15)
	addq	$3, %r15
	jmp doubleid.12start

	.align 8
doubleid.12start:
	movq	%rdi, %rcx
	movq	%rsi, %rcx
	movq	free_ptr(%rip), %rdx
	addq	$16, %rdx
	movq	fromspace_end(%rip), %rsi
	cmpq	%rsi, %rdx
	jl block59464
	jmp block59465

	.align 8
block59465:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59463

	.align 8
block59464:
	movq	$0, %rdx
	jmp block59463

	.align 8
block59463:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 1(%r15)
	movq	1(%r15), %rax
	movq	%rax, 2(%r15)
	movq	2(%r15), %r11
	movq	8(%r11), %rdx
	movq	2(%r15), %rdi
	movq	%rcx, %rsi
	movq	%rdx, %rax
	subq	$3, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	jmp *%rax

	.align 8
doubleid.12conclusion:
	subq	$3, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
lambda.18:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	subq	$8, %rsp
	movq	$0, 0(%r15)
	addq	$1, %r15
	jmp lambda.18start

	.align 8
lambda.18start:
	movq	%rdi, 0(%r15)
	movq	%rsi, %rcx
	movq	%rcx, %rax
	jmp lambda.18conclusion

	.align 8
lambda.18conclusion:
	subq	$1, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq



