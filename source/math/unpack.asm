; Unpack a ROM constant into the FAC

romupk           sta index1
                 sty index1+1
                 ldy #4
                 lda (index1),y                           ; it's in ROM, so ok to use ind
                 sta arglo
                 dey
                 lda (index1),y
                 sta argmo
                 dey
                 lda (index1),y
                 sta argmoh
                 dey
                 lda (index1),y
                 sta argsgn
                 eor facsgn
                 sta arisgn
                 lda argsgn
                 ora #$80
                 sta argho
                 dey
                 lda (index1),y
                 sta argexp
                 lda facexp                               ; sets code of facexp
                 rts


; Unpack a RAM constant into the FAC

conupk           sta index1
                 sty index1+1

; lda mmu_config_reg
; pha   ;preserve caller's memory config????

                 ldy #4
                 jsr indin1_ram1
                 sta arglo
                 dey
                 jsr indin1_ram1
                 sta argmo
                 dey
                 jsr indin1_ram1
                 sta argmoh
                 dey
                 jsr indin1_ram1
                 sta argsgn
                 eor facsgn
                 sta arisgn
                 lda argsgn
                 ora #$80
                 sta argho
                 dey
                 jsr indin1_ram1
                 sta argexp

; pla
; sta mmu_config_reg ;restore caller's memory config????

                 lda facexp                               ; set codes of facexp
                 rts


; Check special cases and add exponents for FMULT, FDIV

muldiv
                 lda argexp                               ; exp of arg=0?
mldexp           beq zeremv                               ; so we get zero exponent
                 clc
                 adc facexp                               ; result is in (a)
                 bcc l172_1                               ; find (c) xor (n)
                 +lbmi overr                              ; overflow if bits match
                 clc
                 !text $2c

l172_1           bpl zeremv                               ; underflow
                 adc #$80                                 ; add bias
                 sta facexp
                 +lbeq zeroml                             ; zero the rest of it
                 lda arisgn
                 sta facsgn                               ; arisgn is result's sign
                 rts                                      ; done


mldvex           lda facsgn                               ; get sign
                 eor #$ff                                 ; complement it
                 +lbmi overr

zeremv           pla                                      ; get addr off stack
                 pla
                 +lbra zerofc                             ; underflow


; Multiply FAC by 10

mul10            jsr movaf                                ; copy FAC into ARG
                 tax
                 beq mul10r                               ; if (FAC)=0, got answer
                 clc
                 adc #2                                   ; augment exp by 2
                 +lbcs overr                              ; overflow

finml6           ldx #0
                 stx arisgn                               ; signs are same
                 jsr faddc                                ; add together
                 inc facexp                               ; multiply by two
                 +lbeq overr                              ; overflow

mul10r           rts


div10            jsr movaf                                ; move FAC to ARG
                 lda #<tenc
                 ldy #>tenc                               ; point to constant of 10.0
                 ldx #0                                   ; signs are both positive

fdivf            stx arisgn
                 jsr movfm                                ; put it into FAC
                 bra fdivt

fdivt_c65                                                 ; [910402]
                 lda argsgn
                 eor facsgn
                 sta arisgn                               ; resultant sign
                 ldx facexp                               ; set signs on thing to divide
                 bra fdivt                                ; go divide

fdiv             jsr conupk                               ; unpack constant
fdivt            +lbeq doverr                             ; can't divide by zero
                 jsr round                                ; take FACOV into account in FAC
                 lda #0                                   ; negate facexp
                 sec
                 sbc facexp
                 sta facexp
                 jsr muldiv                               ; fix up exponents
                 inc facexp                               ; scale it right
                 +lbeq overr                              ; overflow
                 ldx #$fc                                 ; set up procedure
                 lda #1


divide                                                    ; this is the best code in the whole pile
                 ldy argho                                ; see what relation holds
                 cpy facho
                 bne savquo                               ; (c)=0,1. n(c=0)=0.
                 ldy argmoh
                 cpy facmoh
                 bne savquo
                 ldy argmo
                 cpy facmo
                 bne savquo
                 ldy arglo
                 cpy faclo

savquo           php
                 rol                                      ; save result
                 bcc qshft                                ; if not done, continue
                 inx
                 sta reslo,x
                 beq ld100
                 bpl divnrm                               ; note this req 1 no ram then access
                 lda #1

qshft            plp                                      ; return condition codes
                 bcs divsub                               ; FAC <= ARG

shfarg           asl arglo                                ; shift ARG one place left
                 rol argmo
                 rol argmoh
                 rol argho
                 bcs savquo                               ; save a result of one for this position and divide
                 bmi divide                               ; if msb on, go decide whether to sub
                 bpl savquo


divsub           tay                                      ; notice c must be on here
                 lda arglo
                 sbc faclo
                 sta arglo
                 lda argmo
                 sbc facmo
                 sta argmo
                 lda argmoh
                 sbc facmoh
                 sta argmoh
                 lda argho
                 sbc facho
                 sta argho
                 tya
                 bra shfarg



ld100            lda #$40                                 ; only want two more bits
                 bra qshft                                ; always branches



divnrm           asl                                      ; get last two bits into MSB and B6
                 asl
                 asl
                 asl
                 asl
                 asl
                 sta facov
                 plp



movfr            lda resho                                ; move result to FAC
                 sta facho
                 lda resmoh
                 sta facmoh
                 lda resmo
                 sta facmo
                 lda reslo                                ; move lo and sign
                 sta faclo
                 +lbra normal                             ; all done



movfm            sta index1                               ; move memory into FAC from ROM (unpacked)
                 sty index1+1
                 ldy #4
                 lda (index1),y
                 sta faclo
                 dey
                 lda (index1),y
                 sta facmo
                 dey
                 lda (index1),y
                 sta facmoh
                 dey
                 lda (index1),y
                 sta facsgn
                 ora #$80
                 sta facho
                 dey
                 lda (index1),y
                 sta facexp
                 sty facov
                 rts


; Move number from FAC to memory

mov2f            ldx #tempf2                              ; move from FAC to temp FAC2
                 !text $2c

mov1f            ldx #tempf1                              ; move from FAC to temp FAC1

                 ldy #0
movmf            jsr round
                 stx index1
                 sty index1+1
                 ldy #4
                 lda faclo
                 sta (index),y                            ; BasePage
                 dey
                 lda facmo
                 sta (index),y                            ; BasePage
                 dey
                 lda facmoh
                 sta (index),y                            ; BasePage
                 dey
                 lda facsgn                               ; include sign in ho
                 ora #$7f
                 and facho
                 sta (index),y                            ; BasePage
                 dey
                 lda facexp
                 sta (index),y                            ; BasePage
                 sty facov                                ; zero it since rounded
                 rts                                      ; (y)=0


movmf_ram1
                 jsr round
                 stx index1
                 sty index1+1
                 phx
                 ldx #index
                 ldy #4
                 lda faclo
                 jsr sta_far_ram1                         ; sta (index),y
                 dey
                 lda facmo
                 jsr sta_far_ram1                         ; sta (index),y
                 dey
                 lda facmoh
                 jsr sta_far_ram1                         ; sta (index),y
                 dey
                 lda facsgn                               ; include sign in ho
                 ora #$7f
                 and facho
                 jsr sta_far_ram1                         ; sta (index),y
                 dey
                 lda facexp
                 jsr sta_far_ram1                         ; sta (index),y
                 sty facov                                ; zero it since rounded
                 plx
                 rts                                      ; (y)=0


movfa            lda argsgn

movfa1           sta facsgn

                 ldx #5
l173_1           lda argexp-1,x
                 sta facexp-1,x
                 dex
                 bne l173_1
                 stx facov
                 rts


movaf            jsr round

movef            ldx #6
l174_1           lda facexp-1,x
                 sta argexp-1,x
                 dex
                 bne l174_1
                 stx facov                                ; zero it since rounded
movrts           rts



round            lda facexp                               ; zero?
                 beq movrts                               ; yes, done rounding
                 asl facov                                ; round?
                 bcc movrts                               ; no, msb off

incrnd           jsr incfac                               ; yes, add one to lsb(FAC) /// entry from EXP
;note .c=1 since incfac doesn't touch .c
                 +lbeq rndshf                             ; carry:   squeeze msb in and rts
                 rts                                      ; no carry: rts now



; Put sign in FAC into (a).

sign             lda facexp
                 beq signrt                               ; if number is zero, so is result

fcsign           lda facsgn
fcomps           rol
                 lda #$ff                                 ; assume negative
                 bcs signrt
                 lda #1                                   ; get +1
signrt           rts



; SGN function

sgn              jsr sign

;float the signed integer in accb
float            sta facho                                ; put (accb) in high order
                 lda #0
                 sta facho+1
                 ldx #$88                                 ; get the exponent
;float the signed number in FAC


floats           lda facho
                 eor #$ff
                 rol                                      ; get comp of sign in carry
floatc           lda #0                                   ; zero (a) but not carry
                 sta faclo
                 sta facmo

floatb           stx facexp
                 sta facov
                 sta facsgn
                 +lbra fadflt




; Absolute value of FAC

abs              lsr facsgn
                 rts



; Compare two numbers:
;
; a=1  if  ARG < FAC
; a=0  if  ARG = FAC
; a=-1 if  ARG > FAC

fcomp            sta index2
                 sty index2+1
                 ldy #0
                 lda (index2),y                           ; has argexp
                 iny                                      ; bump pointer up
                 tax                                      ; save a in x and reset codes
                 beq sign
                 lda (index2),y
                 eor facsgn                               ; signs the same
                 bmi fcsign                               ; signs differ so result is
                 cpx facexp                               ; sign of FAC again
                 bne l175_1

                 lda (index2),y
                 ora #$80
                 cmp facho
                 bne l175_1
                 iny
                 lda (index2),y
                 cmp facmoh
                 bne l175_1
                 iny
                 lda (index2),y
                 cmp facmo
                 bne l175_1
                 iny
                 lda #$7f
                 cmp facov
                 lda (index2),y
                 sbc faclo                                ; get zero if equal
                 beq qintrt                               ; rts

l175_1           lda facsgn
                 bcc l175_2
                 eor #$ff
l175_2           bra fcomps                               ; a part of sign sets up (a)

;.end