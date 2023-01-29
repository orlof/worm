// PREAMBLE
.const ZP_W0 = $fb
*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061
    sei
    dec 1
    cli
// PROGRAM CODE
{ // BYTE *
    // byte literal 32 to stack
    // lda #32
    // pha
    // push byte value of a to stack
    // lda a
    // pha
    lda a // optimized
    sta ZP_W0
    lda #32 // optimized
    sta ZP_W0+1
    jsr BYTE_MULTIPLY
    // lda ZP_W0
    // pha
}
    // assign to byte ident
    lda ZP_W0 // optimized
    sta a
// POSTAMBLE
    sei
    inc 1
    cli
    rts
// END

BYTE_MULTIPLY: {
    lda #0
    ldx #$8
    lsr ZP_W0
loop:
    bcc no_add
    clc
    adc ZP_W0+1
no_add:
    ror
    ror ZP_W0
    dex
    bne loop
    sta ZP_W0+1    rts
}
a:
    .byte $05
