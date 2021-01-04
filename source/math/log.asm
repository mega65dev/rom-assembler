
; Natural Log Function
;
; Calculation is by   LN(f*2^n) = (n+LOG2(f))*LN(2)
; An approximation polynomial is used to calculate LOG2(f).


log             jsr sign                                ; is it positive?
                +lbeq fcerr                             ; can't tolerate neg or zero

                lda facexp                              ; get exponent into (a)
                sbc #$7f                                ; remove bias (carry is off)
                pha                                     ; save exponent a while
                lda #$80
                sta facexp                              ; result is FAC in range (0.5,1)
                lda #<sqr05                             ; get pointer to sqr(0.5)
                ldy #>sqr05
                jsr romadd
                lda #<sqr20
                ldy #>sqr20
                jsr romdiv
                lda #<fone
                ldy #>fone
                jsr romsub
                lda #<logcn2
                ldy #>logcn2
                jsr polyx                               ; evaluate approximation polynomial
                lda #<neghlf                            ; add in last constant
                ldy #>neghlf
                jsr romadd
                pla                                     ; get exponent back
                jsr finlog
                lda #<log2                              ; multiply result by ln(2)
                ldy #>log2


rommlt          jsr romupk
                bra fmultt                              ; multiply together


faddh           lda #<fhalf
                ldy #>fhalf

romadd          jsr romupk
                +lbra faddt


romsub          jsr romupk
                +lbra fsubt


romdiv          jsr romupk
                +lbra fdivt
