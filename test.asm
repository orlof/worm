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

    .byte 0, 1
    .text "HELLO"
    .byte 3
    .text "Hello"
    .byte 6
    .byte 0, 1
    .text "HELLO"
    .byte 3, 4, 5

    // POSTAMBLE
    sei
    inc 1
    cli
    rts
// END

// MACROS

.macro LOAD(Addr, ZP) {
    lda #<Addr
    sta ZP
    lda #>Addr
    sta ZP+1
}

// LITERALS
.encoding "petscii_upper"
STR_eb61eead90e3b899c6bcbe27ac581660:
    .byte 5
    .text "HELLO"
STR_8b1a9953c4611296a827abf8c47804d7:
    .byte 5
    .text "Hello"
// DATA

// SHARED

// MAIN LOCALS
