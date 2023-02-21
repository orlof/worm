
class Includes:
    def __init__(self):
        self.macros = {}
        self.libraries = {}

    def include_libraries(self, lines):
        for line in lines:
            token = line.split()
            if len(token) >= 2:
                if token[0] == "jsr":
                    if token[1].startswith("LIB_"):
                        name = token[1]
                        if name not in self.libraries:
                            self.libraries[name] = LIBRARY[name]
                            self.include_macros(LIBRARY[name])
                            self.include_libraries(LIBRARY[name])

    def include_macros(self, lines):
        for line in lines:
            for macro in sorted(MACRO.keys(), key=len, reverse=True):
                if ("    %s" % macro) in line:
                    if macro not in self.macros:
                        self.macros[macro] = MACRO[macro]
                        self.include_macros(MACRO[macro])
                        self.include_libraries(MACRO[macro])


MACRO = {
"LOAD":
"""
.macro LOAD(Addr, ZP) {
    lda #<Addr
    sta ZP
    lda #>Addr
    sta ZP+1
}
""".split("\n"),

"INC16":
"""
.macro INC16(Addr) {
    inc Addr
    bne !+
    inc Addr+1
!:
}
""".split("\n"),

"ADD16_BYTE":
"""
.macro ADD16_BYTE(wAddr, bAddr) {
    clc
    lda bAddr
    adc wAddr
    sta wAddr
    bcc exit
    inc wAddr+1
exit:
}
""".split("\n"),

"SUB16_BYTE":
"""
.macro SUB16_BYTE(wAddr, bAddr) {
    sec
    lda wAddr
    sbc bAddr
    sta wAddr
    bcs exit
    dec wAddr+1
exit:
}
""".split("\n"),

"DEC16":
"""
.macro DEC16(Addr) {
    lda Addr
    bne !+
    dec Addr+1
!:
    dec Addr
}
""".split("\n"),

"STR_PUSH_FROM":
"""
.macro STR_PUSH_FROM(Addr) {
    LOAD(Addr, ZP_W0)
    jsr LIB_STRING_PUSH
}
""".split("\n"),

"STR_PULL_TO":
"""
.macro STR_PULL_TO(Addr) {
    // capacity in a
    LOAD(Addr, ZP_W0)
    jsr LIB_STRING_PULL
}
""".split("\n")
}

LIBRARY = {
"LIB_DEBUG":
"""
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
""".split("\n"),

"LIB_STRING_SUBSCRIPTION":
"""
LIB_STRING_SUBSCRIPTION: {
    tay
    iny
    iny
    lda (STACK),y
    pha
    jsr LIB_STRING_POP
    pla
    ldy #0
    sta (STACK),y
    DEC16(STACK)
    lda #1
    sta (STACK),y
    DEC16(STACK)
    rts
}
""".split("\n"),

"LIB_MUL16":
"""
LIB_MUL16: {
    lda #0          // Initialize RESULT to 0
    sta ZP_DW+2
    ldx #16         // There are 16 bits in ZP_W1
L1:
    lsr ZP_W1+1     // Get low bit of ZP_W1
    ror ZP_W1
    bcc L2          // 0 or 1?
    tay             // If 1, add ZP_W0 (hi byte of RESULT is in A)
    clc
    lda ZP_W0
    adc ZP_DW+2
    sta ZP_DW+2
    tya
    adc ZP_W0+1
L2:
    ror             // "Stairstep" shift
    ror ZP_DW+2
    ror ZP_DW+1
    ror ZP_DW
    dex
    bne L1
    sta ZP_DW+3
    rts
}
""".split("\n"),

"LIB_DIV8":
"""
LIB_DIV8 {
    // ZP_B0 divident, ZP_B1 divisor
    lda #0
    ldx #8
    asl ZP_B0
l1:
    rol
    cmp ZP_B1
    bcc l2
    sbc ZP_B1
l2:
    rol ZP_B0
    dex
    bne l1
    // ZP_B0 quotient, ZP_B1 remainder
    rts
}
""".split("\n"),

"LIB_MUL8":
"""
LIB_MUL8: {
    lda #0          // Initialize ZP_W0 to 0
    ldx #8          // There are 8 bits in ZP_B1
L1:
    lsr ZP_B1       // Get low bit of ZP_B1
    bcc L2          // 0 or 1?
    clc             // If 1, add ZP_B0
    adc ZP_B0
L2:
    ror             // "Stairstep" shift (catching carry from add)
    ror ZP_W0
    dex
    bne L1
    sta ZP_W0+1
    rts
}
""".split("\n"),

"LIB_STRING_MERGE":
"""
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
""".split("\n"),

"LIB_STRING_POP":
"""
LIB_STRING_POP: {
    ldy #1
    lda (STACK),y
    clc
    adc #1
    adc STACK
    sta STACK
    bcc exit
    inc STACK+1
exit:
    rts
}
""".split("\n"),

"LIB_STRING_PUSH":
"""
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
""".split("\n"),

"LIB_CSTRING_WORD":
"""
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
""".split("\n"),

"LIB_STRING_PULL":
"""
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
""".split("\n"),

}
