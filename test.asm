// CONSTANTS
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
    jsr f
    LOAD(f._RETURN_, ZP_W0)
    jsr PUSH
    // assign string
    LOAD(result, ZP_W0)
    jsr PULL
// POSTAMBLE
    sei
    inc 1
    cli
    rts
// END

f: {
// CODE
    LOAD(STR_1356c67d7ad1638d816bfb822dd2c25d, ZP_W0)
    jsr PUSH
    LOAD(_RETURN_, ZP_W0)
    jmp PULL
    rts
// VARIABLES
_RETURN_:
    .byte 0
    .text "   "
}
.macro LOAD(Addr, ZP) {
    lda #<Addr
    sta ZP
    lda #>Addr
    sta ZP+1
}
PUSH: {
    ldy #0
    sec
    lda STACK
    sbc (ZP_W0),y
    sta STACK
    bcs !+
    dec STACK+1
!:
    lda (ZP_W0),y
    tay
loop:
    lda (ZP_W0),y
    sta (STACK),y
    dey
    bpl loop
    lda STACK
    bne !+
    dec STACK+1
!:
    dec STACK
    rts
}
PULL: {
    inc STACK
    bne !+
    inc STACK+1
!:
    ldy #0
    lda (STACK),y
    tay
loop:
    lda (STACK),y
    sta (ZP_W0),y
    dey
    bpl loop
    ldy #0
    clc
    lda STACK
    adc (STACK),y
    sta STACK
    bcc !+
    inc STACK+1
!:
    rts
}
// LITERALS
STR_1356c67d7ad1638d816bfb822dd2c25d:
    .byte 3
    .text "Foo"
// SHARED
// MAIN LOCALS
result:
    .byte 0
    .text "   "
