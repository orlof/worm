// code segment
.const ZP_W0 = $fb
*=2048
.byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061
*=2061

    lda print.color

print: {
    // local variables
    color: .byte 0

    start:
    lda color
}
    lda #1
