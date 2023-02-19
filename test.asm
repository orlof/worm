// CONSTANTS
.const ZP_DW = $02
.const ZP_W0 = $06
.const ZP_W1 = $08
.const ZP_B0 = $10
.const ZP_B1 = $11
.const STACK = $12
.const KERNEL_CHROUT = $ffd2

*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061
    sei
    dec 1
    cli
    LOAD($cfff, STACK)

    jsr examples.__MAIN__

    sei
    inc 1
    cli
    rts
    // END OF PROGRAM

examples: {

    lda example2.b
    pha
    lda #0
    sta ZP_W0+1
    pla
    sta ZP_W0
    lda #0
    sta ZP_B0
    jsr LIB_CSTRING_WORD
    jsr LIB_DEBUG
    rts

    // VARIABLES
a:
    .byte $00
}

example2: {

    rts

inc: {

    // BYTE +
    lda b
    pha
    lda #1
    pha
    // byte l + r
    pla
    sta ZP_W0
    pla
    clc
    adc ZP_W0
    pha
}
    pla
    sta inc
    rts
    rts

    // VARIABLES
b:
    .byte $00
inc:
    .byte $00
}   // inc

    // VARIABLES
b:
    .byte $00
}

