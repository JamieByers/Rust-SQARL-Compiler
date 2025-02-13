	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #96
	stp	x29, x30, [sp, #80]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 96
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w8, #27762                      ; =0x6c72
	mov	x9, #25928                      ; =0x6548
Lloh0:
	adrp	x0, l_format_str@PAGE
Lloh1:
	add	x0, x0, l_format_str@PAGEOFF
	movk	w8, #8548, lsl #16
	movk	x9, #27756, lsl #16
	stur	w8, [sp, #75]
	mov	w8, #33                         ; =0x21
	movk	x9, #8303, lsl #32
	stur	x8, [sp, #44]
	mov	x8, #111                        ; =0x6f
	movk	x9, #28503, lsl #48
	movk	x8, #114, lsl #32
	stur	x9, [sp, #67]
	mov	x9, #108                        ; =0x6c
	stur	x8, [sp, #28]
	mov	x8, #32                         ; =0x20
	movk	x9, #100, lsl #32
	movk	x8, #87, lsl #32
	stur	x9, [sp, #36]
	mov	x9, #72                         ; =0x48
	stur	x8, [sp, #20]
	mov	w8, #111                        ; =0x6f
	movk	x9, #101, lsl #32
	str	w8, [sp, #16]
	mov	x8, #108                        ; =0x6c
	movk	x8, #108, lsl #32
	strb	wzr, [sp, #79]
	stp	x9, x8, [sp]
	bl	_printf
	ldp	x29, x30, [sp, #80]             ; 16-byte Folded Reload
	mov	w0, wzr
	add	sp, sp, #96
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l_format_str:                           ; @format_str
	.asciz	"%d\n"

.subsections_via_symbols
