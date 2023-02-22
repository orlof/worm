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

    jsr examples

    sei
    inc 1
    cli
    rts
    // END OF PROGRAM

examples: {

    lda #0
    pha
    pla
    tay
    lda x,y
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


_x:
    .byte 0, 1
}  // examples

// MACROS

.macro LOAD(Addr, ZP) {
    lda #<Addr
    sta ZP
    lda #>Addr
    sta ZP+1
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


.macro INC16(Addr) {
    inc Addr
    bne !+
    inc Addr+1
!:
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
    cpy #0
    beq PrDec16Digit
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

