; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      polyeval.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


; Polynomial Evaluator and the Random Number Generator.
;
; Evaluate  p(x^2)*x
; The pointer to degree is in (a,y) and the constants follow the degree.
; For x=FAC, compute  c0*x + c1*x^3 + c2*x^5 + c3*x^7 +...+ c(n)*x^(2*n+1)


polyx           sta     polypt                          ; retain polynomial pointer for later
                sty     polypt+1
                jsr     mov1f                           ; save FAC in factmp (y=0 upon return)
                lda     #tempf1
                jsr     fmult                           ; compute x^2.
                jsr     poly1                           ; compute p(x^2).
                lda     #<tempf1
                ldy     #>tempf1
                +lbra   fmult                           ; multiply by FAC again


; Polynomial Evaluator
;
; Pointer to degree is in (a,y).
; Compute:  c0+c1*x+c2*x^2+c3*x^3+c4*x^4...+c(n-1)*x^(n-1)+c(n)*x^n
;  which is roughly (LOG(2)^n)/LOG(EXP(1))/n!


poly            sta     polypt
                sty     polypt+1

poly1           jsr     mov2f                           ; save FAC (rounds, .y=0)
                lda     (polypt),y
                sta     degree
                inw     polypt
                lda     polypt
                ldy     polypt+1

l185_1          jsr     rommlt
                lda     polypt                          ; get current pointer
                ldy     polypt+1
                clc
                adc     #5
                bcc     l185_2
                iny
l185_2          sta     polypt
                sty     polypt+1
                jsr     romadd                          ; add in constant
                lda     #<tempf2                        ; multiply the original FAC
                ldy     #>tempf2
                dec     degree                          ; done?
                bne     l185_1
                rts                                     ; yes

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
