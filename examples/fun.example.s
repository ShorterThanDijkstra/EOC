	.globl main
	.align 8
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rbx
	subq	$8, %rsp
	jmp start

	.align 8
start:
	movq	$0, %rdx
	jmp loop18113

	.align 8
block18114:
	movq	%rdx, %rbx
	callq	read_int
	movq	%rax, %rcx
	movq	%rbx, %rdx
	addq	%rcx, %rdx
	jmp loop18113

	.align 8
loop18113:
	movq	%rdx, %rcx
	cmpq	$17, %rcx
	jl block18114
	movq	%rdx, %rax
	jmp conclusion

	.align 8
conclusion:
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	retq



