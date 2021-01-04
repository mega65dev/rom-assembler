

link_program
                lda txttab
                ldy txttab+1
                sta index
                sty index+1
                clc

chead           ldy #0
                jsr indin1                              ; lda (index),y .. check for null link
                bne l27_1
                iny
                jsr indin1                              ; lda (index),y
                beq lnkrts

l27_1           ldy #3                                  ; [900524]
l27_2           iny                                     ; ???? very expensive loop ????
                cpy #254
                bcs link_error                          ; failsafe- program is mangled  [910103]
                jsr indin1                              ; lda (index),y
                bne l27_2
                iny
                tya
                adc index
                pha
                ldy #0
                ldx #index
                jsr sta_far_ram0                        ; sta (index),y   (bleed-thru)
                tya
                adc index+1
                iny
                jsr sta_far_ram0                        ; sta (index),y   (bleed-thru)
                plx
                stx index
                sta index+1
                bra chead                               ; always


link_error                                              ; [910103]
                jsr highlight_text                      ; [911119]
                jsr _primm
                !text cr,"?PROGRAM MANGLED",cr,0
                jsr highlight_done                      ; [911119]


lnkrts          rts
