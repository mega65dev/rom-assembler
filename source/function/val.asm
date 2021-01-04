


; The VAL function takes a string and turns it into a number by interpreting
; the PETSCII digits etc.  Except for the problem that a terminator must be
; supplied by replacing the character beyond the string, VAL is merely a call
; to floating point input (FIN).

val             jsr len1                                ; get length
                +lbeq zerofc                            ; return 0 if len=0

; Use text to fp number code by faking a new text poiner

val_1           clc                                     ; ///jump table entry.  convert PETSCII to floating point
                adc index1
                sta strng2                              ; add length to index1 and put in strng2
                lda index1+1
                adc #0
                sta strng2+1

                ldy #0
                lda #strng2
                jsr lda_far_ram1                        ; replace character after string with $00 (fake EOL)
                pha                                     ; save old character
                tya                                     ; (.A=0)
                ldx #strng2
                jsr sta_far_ram1 ;sta (strng2),y        ; ..and put in null
                jsr fin_chrget_2                        ; get character pointed to and set flags.(sorta like chrgot)
                ldx #1                                  ; flag 'bank 1'
                jsr fin                                 ; go do evaluation
                pla                                     ; get saved character
                phx
                ldx #strng2
                ldy #0
                jsr sta_far_ram1 ;sta (strng2),y        ; restore zeroed-out character
                plx
                rts

;.end