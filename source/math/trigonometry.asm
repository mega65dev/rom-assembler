; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      trigonometry.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; Sine, Cosine, and Tangent Functions.



; Cosine function cos(x)=sin(x+pi/2)


cos             lda     #<pi2                           ; pointer to pi/2
                ldy     #>pi2
                jsr     romadd                          ; add it in.  fall into sine



; Sine function
;
; Use identities to get FAC in quadrants I or IV.  The FAC is divided by 2*pi
; and the integer part is ignored because sin(x+2*pi)=sin(x).  Then the
; argument can be compared with pi/2 by comparing the result of the division
; with pi/2(2*pi)=1/4.  Identities are then used to get the result in quadrants
; I or IV.  An approximation polynomial is then used to compute sin(x).


sin             jsr     movaf
                lda     #<twopi                         ; get pointer to divisor
                ldy     #>twopi
                ldx     argsgn                          ; get sign of result
                jsr     fdivf
                jsr     movaf                           ; get result into ARG
                jsr     int                             ; integerize FAC
                lda     #0
                sta     arisgn                          ; always have the same sign
                jsr     fsubt                           ; keep only the fractional part
                lda     #<fr4                           ; get pointer to 1/4
                ldy     #>fr4
                jsr     romsub
                lda     facsgn                          ; save sign for later
                pha
                bpl     sin1                            ; first quadrant
                jsr     faddh                           ; add 1/2 to FAC
                lda     facsgn                          ; sign is negative?
                bmi     sin2
                lda     tansgn                          ; quads II and III come here
                eor     #$ff
                sta     tansgn

sin1            jsr     negop                           ; if positive, negate it

sin2            lda     #<fr4                           ; pointer to 1/4
                ldy     #>fr4
                jsr     romadd                          ; add it in
                pla                                     ; get original quadrant
                bpl     l186_1
                jsr     negop                           ; if negative, negate result

l186_1          lda     #<sincon
                ldy     #>sincon
                +lbra   polyx                           ; do approximation polyomial



; Tangent function


tan             jsr     mov1f                           ; move FAC into temporary
                lda     #0
                sta     tansgn                          ; remember whether to negate
                jsr     sin                             ; compute the sin
                ldx     #<tempf3
                ldy     #>tempf3
                jsr     movmf                           ; put sign into other temp
                lda     #<tempf1
                ldy     #>tempf1
                jsr     movfm                           ; put this memory location into FAC
                lda     #0
                sta     facsgn                          ; start off positive
                lda     tansgn
                jsr     l187_1                          ; compute cosine
                lda     #<tempf3
                ldy     #>tempf3                        ; address of sine value
; bra fdiv ;divide sine by cosine and return
                jsr     conupk                          ; unpack constant    [910226] FAB
                +lbeq   overr                           ; overflow error     "
                +lbra   fdivt                           ; "

l187_1          pha                                     ; cosc.
                bra     sin1


; Arctangent function
;
; Use identities to get arg between 0 and 1 and then use an approximation
; polynomial to compute arctan(x).


atn             lda     facsgn                          ; what is sign?
                pha                                     ; save for later
                bpl     l188_1
                jsr     negop                           ; if negative, negate FAC
;use arctan(x)=-arctan(-x)
l188_1          lda     facexp
                pha                                     ; save this too for later
                cmp     #$81                            ; see if FAC >= 1.0
                bcc     l188_2                          ; it is less than 1
                lda     #<fone                          ; get pntr to 1.0
                ldy     #>fone
                jsr     romdiv                          ; compute reciprocal
;use aectan(x)=pi/2-arctan(1/x)
l188_2          lda     #<atncon                        ; pointer to arctan constants
                ldy     #>atncon
                jsr     polyx
                pla
                cmp     #$81                            ; was original argument < 1?
                bcc     l188_3                          ; yes
                lda     #<pi2
                ldy     #>pi2
                jsr     romsub                          ; subtract arctan from pi/2

l188_3          pla                                     ; was original aurgument positive?
                bpl     l188_4                          ; yes
                +lbra   negop                           ; if negative, negate result

l188_4          rts                                     ; all done

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
