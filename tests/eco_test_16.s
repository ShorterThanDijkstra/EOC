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
	jmp loop59489

	.align 8
loop59489:
	cmpq	$3, %rcx
	jl block59490
	jmp block59494

	.align 8
block59494:
	movq	free_ptr(%rip), %rdx
	addq	$16, %rdx
	movq	fromspace_end(%rip), %rsi
	cmpq	%rsi, %rdx
	jl block59492
	jmp block59493

	.align 8
block59493:
	movq	%r15, %rdi
	movq	$16, %rsi
	callq	collect
	jmp block59491

	.align 8
block59492:
	movq	$0, %rdx
	jmp block59491

	.align 8
block59491:
	movq	free_ptr(%rip), %r11
	addq	$8, free_ptr(%rip)
	movq	$4097, 0(%r11)
	movq	%r11, 0(%r15)
	movq	0(%r15), %rax
	movq	%rax, 1(%r15)
	movq	1(%r15), %r11
	movq	8(%r11), %rdx
	movq	1(%r15), %rdi
	movq	%rcx, %rsi
	movq	%rdx, %rax
	subq	$16, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	jmp *%rax

	.align 8
block59490:
	callq	read_int
	movq	%rax, %rcx
	jmp loop59489

	.align 8
mainconclusion:
	subq	$16, %r15
	addq	$8, %rsp
	popq	%r15
	popq	%rbp
	retq

	.align 8
id.106:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	addq	$0, %r15
	jmp id.106start

	.align 8
id.106start:
	movq	%rdi, %rcx
	movq	%rsi, %rcx
	movq	%rcx, %rax
	jmp id.106conclusion

	.align 8
id.106conclusion:
	subq	$0, %r15
	addq	$0, %rsp
	popq	%rbp
	retq



