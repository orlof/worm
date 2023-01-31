// PREAMBLE
.const ZP_W0 = $fb
*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061
    sei
    dec 1
    cli
// PROGRAM CODE
{ // BYTE +
    // byte literal 1 to stack
    // lda #1
    // pha
    lda #1 // optimized
    tay
    // lda a,y
    // pha
    // byte literal 1 to stack
    // lda #1
    // pha
    // byte l + r
    lda #1 // optimized
    sta ZP_W0
    lda a,y // optimized
    clc
    adc ZP_W0
    pha
}
    // byte literal 0 to stack
    // lda #0
    // pha
    // byte[byte]=byte
    lda #0 // optimized
    tax
    pla
    sta a,x
// POSTAMBLE
    sei
    inc 1
    cli
    rts
// END

a:
    .byte $01, $02, $03
