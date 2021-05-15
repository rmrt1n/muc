	.intel_syntax noprefix
	.globl main
main:
	mov rax, 1
	push rax
	mov rax, 2
	pop rbx
	add rax, rbx
	ret
