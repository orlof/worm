// CONSTANTS
.const ZP_DW = $02
.const ZP_W0 = $06
.const ZP_W1 = $08
.const ZP_B0 = $10
.const ZP_B1 = $11
.const STACK = $12
.const KERNEL_CHROUT = $ffd2

.const SIZE2 = 2
.const SIZE = SIZE2 + 1
// PREAMBLE
*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061
    sei
    dec 1
    cli
    LOAD($cfff, STACK)

    // PROGRAM CODE

{
    lda #1
loop:
// FOOBAR
    stx   // foobar
    inc
    bne SIZE
exit:
}
    // lda #3
    // pha
    lda #0
    sta ZP_W0+1
    lda #3 // optimized
    sta ZP_W0
    lda #0
    sta ZP_B0
    jsr LIB_CSTRING_WORD
    jsr LIB_DEBUG

    // POSTAMBLE
    sei
    inc 1
    cli
    rts
// END

f: {
// CODE
{
    lda #1
loop:
//FOOBAR
    stx    //foobar
    inc
    bne
exit:
}
{ // STRING +
    // lda #1
    // pha
    lda #0
    sta ZP_W0+1
    lda #1 // optimized
    sta ZP_W0
    lda #0
    sta ZP_B0
    jsr LIB_CSTRING_WORD
{ // STRING +
    // lda #2
    // pha
    lda #0
    sta ZP_W0+1
    lda #2 // optimized
    sta ZP_W0
    lda #0
    sta ZP_B0
    jsr LIB_CSTRING_WORD
{ // STRING +
    STR_PUSH_FROM(STR_b1a326c06d88bf042f73d70f50197905)
    STR_PUSH_FROM(s)
    jsr LIB_STRING_MERGE
}
    jsr LIB_STRING_MERGE
}
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

.macro ADD16_BYTE(wAddr, bAddr) {
    clc
    lda bAddr
    adc wAddr
    sta wAddr
    bcc exit
    inc wAddr+1
exit:
}

.macro SUB16_BYTE(wAddr, bAddr) {
    sec
    lda wAddr
    sbc bAddr
    sta wAddr
    bcs exit
    dec wAddr+1
exit:
}

.macro DEC16(Addr) {
    lda Addr
    bne !+
    dec Addr+1
!:
    dec Addr
}

.macro STR_PUSH_FROM(Addr) {
    LOAD(Addr, ZP_W0)
    jsr LIB_STRING_PUSH
}

.macro STR_PULL_TO(Addr) {
    // capacity in a
    LOAD(Addr, ZP_W0)
    jsr LIB_STRING_PULL
}

// LIBRARIES

/*
---------------------------
Print 16-bit decimal number
---------------------------
On entry, ZP_W0=number to print
          ZP_B0=0 or pad character (eg '0' or ' ')
On entry at PrDec16Lp1,
          Y=(number of digits)*2-2, eg 8 for 5 digits
On exit,  A,X,Y,ZP_W0,ZP_B0 corrupted
Size      69 bytes
-----------------------------------------------------------------
*/
LIB_CSTRING_WORD: {
PrDec16:
    lda #1                              // String length
    sta ZP_B1
    ldy #8                              // Offset to powers of ten

PrDec16Lp1:
    ldx #$ff
    sec                                 // Start with digit=-1
PrDec16Lp2:
    lda ZP_W0+0
    sbc PrDec16Tens+0,y
    sta ZP_W0+0                         // Subtract current tens
    lda ZP_W0+1
    sbc PrDec16Tens+1,y
    sta ZP_W0+1
    inx
    bcs PrDec16Lp2                      // Loop until <0
    lda ZP_W0+0
    adc PrDec16Tens+0,Y
    sta ZP_W0+0                         // Add current tens back in
    lda ZP_W0+1
    adc PrDec16Tens+1,y
    sta ZP_W0+1
    txa
    bne PrDec16Digit                    // Not zero, print it
    lda ZP_B0
    bne PrDec16Print
    beq PrDec16Next                     // pad<>0, use it
PrDec16Digit:
    ldx #$30
    stx ZP_B0                           // No more zero padding
    ora #$30                            // Print this digit
PrDec16Print:
    inc ZP_B1
    pha
PrDec16Next:
    dey
    dey
    bpl PrDec16Lp1                      // Loop for next digit

    SUB16_BYTE(STACK, ZP_B1)
    ldy ZP_B1
!:
    pla
    sta (STACK),y
    dey
    cpy #1
    bne !-

    ldx ZP_B1
    dex
    txa
    sta (STACK),y

    rts

PrDec16Tens:
    .word 1
    .word 10
    .word 100
    .word 1000
    .word 10000
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
    cmp (STACK),y   //
    bcc !+          // capacity < size
    lda (STACK),y
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

// LITERALS
.encoding "petscii_upper"
STR_b1a326c06d88bf042f73d70f50197905:
    .byte 3
    .text "END"

// SHARED

// MAIN LOCALS
a:
    .byte $03
