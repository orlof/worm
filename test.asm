.const ZP_W0 = $fb
*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061
{
expr:
    // byte literal 1 to stack
    // lda #1
    // pha
    // WHILE
    lda #1 // optimized
    bne suite
    jmp exit
suite:
    // push byte value of color to stack
    lda color
    // pha
    // pla
    sta inc.b
    jsr inc
    lda inc._RETURN_
    // pha
    // assign to byte ident
    // pla
    sta color
    // push byte value of color to stack
    // lda color
    pha
    // word literal 53280 to stack
    // lda #208
    // pha
    // lda #32
    // pha
    // POKE expr, expr
    lda #32 // optimized
    sta ZP_W0
    lda #208 // optimized
    sta ZP_W0+1
    ldx #0
    pla
    sta (ZP_W0,x)
    jmp expr
exit:
}
    rts
// MAIN END

inc: {
// CODE
    // eval left side of +
    // push byte value of b to stack
    lda b
    pha
    // eval right side of +
    // byte literal 1 to stack
    // lda #1
    // pha
    // byte l + r
    lda #1 // optimized
    sta ZP_W0
    pla
    clc
    adc ZP_W0
    // pha
    // pla
    sta _RETURN_
    rts
    // rts
// VARIABLES
_RETURN_:
    .byte $00
b:
    .byte $00
}
color:
    .byte $00
