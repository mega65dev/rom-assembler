; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      rnd.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;    Random Number Function  RND(x)
;
;  x=0 ==> generate a random number based on hardware clock & noisy POT lines
;  x<0 ==> seed a reproducable, pseudo-random number generator
;  x>0 ==> generate a reproducable pseudo-random # based upon seed value above


rnd             jsr     sign                            ; get sign into .a

rnd_0           bmi     l150_2                          ; /// entry from jump table
                bne     l150_1


; Get value from hardware

                jsr     go_slow                         ; Use CIA#1 timer B & SID#2 pot X & Y for seeds  [910314]
                lda     sid2+25                         ; go slow to read POT-X
                asl
                asl
                asl
                asl
                ora     sid2+26                         ; and POT-Y
                eor     vic+18                          ; ???? should be okay- we're in Slow mode
                sta     facmoh
                jsr     go_fast                         ; restore speed
                lda     d1pra+6                         ; timer B is free-running
                sta     facmo
                lda     d1pra+7
                sta     faclo
                eor     facho
                adc     facmoh
                eor     facmo
                adc     faclo
                sta     facho
                bra     l150_3


l150_1          lda     #<rndx                          ; get last one into FAC
                ldy     #>rndx
                jsr     movfm
                lda     #<rmulc
                ldy     #>rmulc                         ; FAC was zero.  restore last one
                jsr     rommlt                          ; multiply by random constant
                lda     #<raddc
                ldy     #>raddc
                jsr     romadd                          ; add random constant

l150_2          ldx     faclo
                lda     facho
                sta     faclo
                stx     facho                           ; reverse hi and lo
                ldx     facmoh
                lda     facmo
                sta     facmoh
                stx     facmo

l150_3          lda     #0                              ; strnex.  make number positive
                sta     facsgn
                lda     facexp                          ; put exp where it will
                sta     facov                           ; be shifted in by normal
                lda     #$80
                sta     facexp                          ; make result between 0 and 1
                jsr     normal                          ; normalize
                ldx     #<rndx
                ldy     #>rndx
                +lbra   movmf                           ; put new one into memory

rmulc           !text 152,53,68,122,0
raddc           !text 104,40,177,70,0

;.end


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
