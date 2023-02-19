main: {
.const A=1
start:
    .byte 0
}

plain: {
    lda main.start
    lda A
}
