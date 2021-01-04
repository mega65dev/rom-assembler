
; DCLEAR - reinitilaize the drive

dclear          jsr dospar                              ; parse the line
                ldy #finit                              ; set code
                lda #2
                jsr trans                               ; send command
                jsr print_dos_error                     ; if any
                +lbra dclall
