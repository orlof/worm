// CONSTANTS
.const ZP_B0 = $02
.const STACK = $fd
.const ZP_W0 = $fb
// PREAMBLE
*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061
    sei
    dec 1
    cli
    LOAD($cfff, STACK)
// PROGRAM CODE
    // byte literal 1 to stack
    // lda #1
    // pha
    // byte literal 1 to stack
    // lda #1
    // pha
    // POKE expr, expr
    lda #1 // optimized
    sta ZP_W0
    lda #1 // optimized
    sta ZP_W0+1
    ldy #0
    pla
    sta (ZP_W0),y
// POSTAMBLE
    sei
    inc 1
    cli
    rts
// END

f: {
// CODE
    // string to stack
    LOAD(s, ZP_W0)
    jsr PUSH
    rts
    // rts
// VARIABLES
_RETURN_:
    .byte 0
    .text "   "
s:
    .byte 0
    .text "   "
}

.macro LOAD(Addr, ZP) {
    lda #<Addr
    sta ZP
    lda #>Addr
    sta ZP+1
}

// LITERALS
// SHARED
// MAIN LOCALS
result:
    .byte 0
    .text "   "
