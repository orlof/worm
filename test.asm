// PREAMBLE
.const ZP_W0 = $fb
*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061
    sei
    dec 1
    cli
// PROGRAM CODE
// POSTAMBLE
    sei
    inc 1
    cli
    rts
// END

f1: {
// CODE
    // byte literal 1 to stack
    // lda #1
    // pha
    lda #1 // optimized
    sta _RETURN_
    rts
    // rts
// VARIABLES
_RETURN_:
    .byte $00
}
f2: {
// CODE
    // push byte value of a to stack
    // lda a
    // pha
    lda a // optimized
    sta _RETURN_
    rts
    // rts
// VARIABLES
_RETURN_:
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
a:
    .byte $00
}
