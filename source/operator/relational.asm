
; Time to perform a relational operator.
; (domask) contains the bits as to which relational operator it was.
; Carry bit on = string compare.


dorel            jsr chkval                               ; check for match
                 bcs strcmp                               ; is it a string?
                 lda argsgn                               ; pack argument for fcomp
                 ora #$7f
                 and argho
                 sta argho
                 lda #<argexp
                 ldy #>argexp
                 jsr fcomp
                 tax
                 bra qcomp


strcmp           lda #0
                 sta valtyp
                 dec opmask
                 jsr frefac                               ; free the faclo string
                 sta dsctmp                               ; save it for later
                 stx dsctmp+1
                 sty dsctmp+2
                 lda argmo                                ; get pointer to other string
                 ldy argmo+1
                 jsr fretmp                               ; frees first desc pointer
                 stx argmo
                 sty argmo+1
                 tax                                      ; copy count into x
                 sec
                 sbc dsctmp                               ; which is greater. if 0, all set up
                 beq stasgn                               ; just put sign of difference away
                 lda #1
                 bcc stasgn                               ; sign is positive
                 ldx dsctmp                               ; length of fac is shorter
                 lda #$ff                                 ; get a minus one for negatives
stasgn           sta facsgn                               ; keep for later
                 ldy #255                                 ; set pointer to first string. (arg)
                 inx                                      ; to loop properly
nxtcmp           iny
                 dex                                      ; any characters left to compare?
                 bne getcmp                               ; not done yet
                 ldx facsgn                               ; use sign of length difference
;since all characters are the same
qcomp            bmi docmp                                ; c is always set then
                 clc
                 bra docmp                                ; always branch


getcmp           lda #argmo
                 jsr lda_far_ram1                         ; lda (argmo),y from RAM1
                 pha
                 lda #dsctmp+1
                 jsr lda_far_ram1                         ; lda (dsctmp+1),y from RAM1
                 sta syntmp
                 pla
                 cmp syntmp
                 beq nxtcmp
                 ldx #$ff
                 bcs docmp
                 ldx #1



docmp
                 inx                                      ; -1 to 1, 0 to 2, 1 to 4
                 txa
                 rol
                 and domask
                 beq l20_1
                 lda #$ff                                 ; map 0 to 0, map all others to -1
l20_1            +lbra float                              ; float the one-byte result into FAC


;.end
