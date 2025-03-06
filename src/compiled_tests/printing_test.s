	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #48
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w8, #99                         ; =0x63
Lloh0:
	adrp	x0, l_format_str@PAGE
Lloh1:
	add	x0, x0, l_format_str@PAGEOFF
	strh	w8, [sp, #30]
	add	x8, sp, #30
	str	x8, [sp]
	bl	_printf
	mov	x8, #25928                      ; =0x6548
	mov	w9, #27762                      ; =0x6c72
Lloh2:
	adrp	x0, l_format_str.1@PAGE
Lloh3:
	add	x0, x0, l_format_str.1@PAGEOFF
	movk	x8, #27756, lsl #16
	movk	w9, #8548, lsl #16
	movk	x8, #8303, lsl #32
	stur	w9, [sp, #25]
	movk	x8, #28535, lsl #48
	strb	wzr, [sp, #29]
	stur	x8, [sp, #17]
	add	x8, sp, #17
	str	x8, [sp]
	bl	_printf
	mov	w8, #1                          ; =0x1
Lloh4:
	adrp	x0, l_format_str.2@PAGE
Lloh5:
	add	x0, x0, l_format_str.2@PAGEOFF
	str	x8, [sp]
	bl	_printf
	mov	x8, #4607182418800017408        ; =0x3ff0000000000000
Lloh6:
	adrp	x0, l_format_str.3@PAGE
Lloh7:
	add	x0, x0, l_format_str.3@PAGEOFF
	str	x8, [sp]
	bl	_printf
	mov	w8, #2                          ; =0x2
Lloh8:
	adrp	x0, l_format_str.4@PAGE
Lloh9:
	add	x0, x0, l_format_str.4@PAGEOFF
	str	x8, [sp]
	bl	_printf
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	mov	w0, wzr
	add	sp, sp, #48
	ret
	.loh AdrpAdd	Lloh8, Lloh9
	.loh AdrpAdd	Lloh6, Lloh7
	.loh AdrpAdd	Lloh4, Lloh5
	.loh AdrpAdd	Lloh2, Lloh3
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l_format_str:                           ; @format_str
	.asciz	"%s\n"

l_format_str.1:                         ; @format_str.1
	.asciz	"%s\n"

l_format_str.2:                         ; @format_str.2
	.asciz	"%d\n"

l_format_str.3:                         ; @format_str.3
	.asciz	"%s\n"

l_format_str.4:                         ; @format_str.4
	.asciz	"%d\n"

.subsections_via_symbols
