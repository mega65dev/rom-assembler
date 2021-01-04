


; WAIT<location>,<mask1>[,<mask2>] statement waits until the contents of
; <location> is nonzero when XORed with mask2 and then ANDed with mask1.
; If mask2 is not present, it is assumed to be zero.

wait            jsr getnum                              ; get required mask1
                stx andmsk
                ldx #0
                jsr chrgot
                beq l137_1
                jsr combyt                              ; get optional mask2
l137_1          stx eormsk

                phz
                ldz current_bank                        ; set up bank number for fetch
                ldx #poker                              ; ..and address
                ldy #0                                  ; ..and index

l137_2          bit current_bank
                bmi l137_3                              ; NOMAP?
                jsr _lda_far                            ; lda (poker),y
                !text $2c

l137_3          lda (poker),y
                eor eormsk
                and andmsk
                beq l137_2
                plz
                rts                                     ; got a nonzero

;.end