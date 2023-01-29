.const ZP_W0 = $fb
*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061
{
expr:
    // lda #1
    // pha
    lda #1 // optimized
    bne suite
    jmp exit
suite:
    lda color
    // pha
    // pla    sta AstNode[3].color
    jsr AstNode[3]
    lda AstNode[3]._RETURN_
    // pha
    // pla
    sta color
    // lda color
    pha
    // lda #208
    // pha
    // lda #32
    // pha
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
%s: {
    lda b
    pha
    // lda #1
    // pha
    lda #1 // optimized
    sta ZP_W0
    pla
    clc
    adc ZP_W0
    // pha
    // pla
    sta _RETURN_    rts
    rts
_RETURN_:
    .byte 0x00
b:
    .byte 0x00
}
color:
    .byte 0x00
