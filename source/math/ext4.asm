


; Quick Greatest Integer Function
;
; Leaves INT(FAC) in FACHO&MO&LO signed
; Assumes FAC < 2~23 =8388608

qint             lda facexp
                 beq clrfac                               ; if zero, got it
                 sec
                 sbc #$a0                                 ; get number of places to shift

                 bbr7 facsgn,l176_1

                 tax
                 lda #$ff
                 sta bits                                 ; put $ff in when shftr shifts bytes
                 jsr negfch                               ; truly negate quantity in FAC
                 txa

l176_1           ldx #fac
                 cmp #$f9
                 bpl qint1                                ; if number of places > 7 shift 1 place at a time
                 jsr shiftr                               ; start shifting bytes, then bits
                 sty bits                                 ; zero bits since adder wants zero
qintrt           rts


qint1            tay                                      ; put count in counter
                 lda facsgn
                 and #$80                                 ; get sign bit
                 lsr facho                                ; save first shifted byte
                 ora facho
                 sta facho
                 jsr rolshf                               ; shift the rest
                 sty bits                                 ; zero (bits)
                 rts



; Greatest Integer Function

int              lda facexp
                 cmp #$a0
                 bcs intrts                               ; forget it
                 jsr round                                ; round FAC per FACOV (fixes the  INT(.9+.1) -> 0  Microsoft bug.  FAB)
                 jsr qint                                 ; INT(FAC)
                 sty facov                                ; clr overflow byte
                 lda facsgn
                 sty facsgn                               ; make FAC look positive
                 eor #$80                                 ; get complement of sign in carry
                 rol
                 lda #$a0                                 ; @230+8
                 sta facexp
                 lda faclo
                 sta integr
                 +lbra fadflt


clrfac           sta facho                                ; make it really zero
                 sta facmoh
                 sta facmo
                 sta faclo
                 tay
intrts           rts
