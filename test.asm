// CONSTANTS
.const ZP_DW = $02
.const ZP_W0 = $06
.const ZP_W1 = $08
.const ZP_B0 = $10
.const ZP_B1 = $11
.const STACK = $12
.const KERNEL_CHROUT = $ffd2

// PREAMBLE
*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061
    sei
    dec 1
    cli
    LOAD($cfff, STACK)

    // PROGRAM CODE

    LOAD(STR_b078ffd28db767c502ac367053f6e0ac, ZP_W0)
    jsr LIB_STRING_PUSH
    lda #12
    sta ZP_B0
    LOAD(f.s, ZP_W0)
    jsr LIB_STRING_PULL
    jsr f
    // DEBUG
    jsr LIB_DEBUG

    // POSTAMBLE
    sei
    inc 1
    cli
    rts
// END

f: {
// CODE
{ // STRING +
    LOAD(STR_b1a326c06d88bf042f73d70f50197905, ZP_W0)
    jsr LIB_STRING_PUSH
    // string to stack
    LOAD(s, ZP_W0)
    jsr LIB_STRING_PUSH
    jsr LIB_STRING_MERGE
}
    rts
    rts
// VARIABLES
s:
    .byte 0
    .text "            "
}
// MACROS

.macro LOAD(Addr, ZP) {
    lda #<Addr
    sta ZP
    lda #>Addr
    sta ZP+1
}

.macro INC16(Addr) {
    inc Addr
    bne !+
    inc Addr+1
!:
}

.macro ADD16_BYTE(Addr, Value) {
    clc
    lda #Value
    adc Addr
    sta Addr
    bcc exit
    inc Addr+1
exit:
}

.macro SUB16_BYTE(Addr, Value) {
    sec
    lda #Value
    sbc Addr
    sta Addr
    bcs exit
    dec Addr+1
exit:
}

.macro DEC16(Addr) {
    lda Addr
    bne !+
    dec Addr+1
!:
    dec Addr
}

// LIBRARIES

LIB_STRING_PUSH: {
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

    DEC16(STACK)
    rts
}


LIB_STRING_PULL: {
    INC16(STACK)

    ldy #0
    lda (STACK),y   // size
    cmp ZP_B0
    bcc !+          // size <= capacity
    lda ZP_B0       // capacity

!:
    sta (ZP_W0),y   // write size
    tay
    beq !+
loop:
    lda (STACK),y
    sta (ZP_W0),y
    dey
    bne loop

!:
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


LIB_DEBUG: {
    INC16(STACK)
    ldy #0
    lda (STACK),y
    tax
!:
    iny
    lda (STACK),y
    jsr KERNEL_CHROUT
    dex
    bne !-

    clc
    tya
    adc STACK
    sta STACK
    bcc !+
    inc STACK+1
!:
    lda #13
    jsr KERNEL_CHROUT

    rts
}


LIB_STRING_MERGE: {
    INC16(STACK)

    ldy #0
    lda (STACK),y   // left size
    tay
    iny
    clc
    adc (STACK),y   // right size
    tax
!:
    dey
    lda (STACK),y
    iny
    sta (STACK),y
    dey
    bne !-

    iny
    txa
    sta (STACK),y
    rts
}

// LITERALS
.encoding "petscii_upper"
STR_b078ffd28db767c502ac367053f6e0ac:
    .byte 5
    .text "START"
STR_b1a326c06d88bf042f73d70f50197905:
    .byte 3
    .text "END"

// SHARED

// MAIN LOCALS
