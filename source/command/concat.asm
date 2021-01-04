; CONCAT

concat           jsr dospar                               ; parse the line
                 jsr chk4
                 ldy #fconc                               ; offset
                 lda #12                                  ; length
                 jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any


