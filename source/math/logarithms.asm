; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      logarithms.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; Exponentation Function
;
; First save the original argument and multiply the FAC by LOG2(e).  The
; result is used to determine if overflow will occur since
;
;  EXP(x) = 2^(x*LOG2(e))
;
; where
;  LOG2(e) = LOG(e), base 2
;
; Then save the integer part of this to scale the answer at the end, since
; 2^y=2^INT(y)*2^(y-INT(y)) and 2^INT(y) are easy to compute.  Now compute
;
;  2^(x*LOG2(e)-INT(x*LOG2(e))
; by
;  p(LOG(2)*(INT(x*LOG2(e))+1)-x
;
; where p is an approximation polynomial. The result is then scaled by the
; power of two previously saved.  Re: Taylor expansion.


exp             lda     #<logeb2                        ; multiply by LOG(e) base 2
                ldy     #>logeb2
                jsr     rommlt                          ; LOGEB2->ARG, FAC=FAC*ARG
                lda     facov
                adc     #$50                            ; ????
                bcc     l184_1
                jsr     incrnd

l184_1          sta     oldov
                jsr     movef                           ; to save in ARG without round.  ARG=FAC, facov=0)
                lda     facexp
                cmp     #$88                            ; if ABS(FAC) >= 128, too big
                bcc     l184_3

l184_2          jsr     mldvex                          ; overflow or overflow
l184_3          jsr     int                             ; FAC=INT(FAC), uses facov
                lda     integr                          ; get low part
                clc
                adc     #$81
                beq     l184_2                          ; overflow or overflow!!

                sec
                sbc     #1                              ; subtract it
                pha                                     ; save a while

                ldx     #5                              ; swap FAC and ARG
l184_4          lda     argexp,x
                ldy     facexp,x
                sta     facexp,x
                sty     argexp,x
                dex
                bpl     l184_4

                lda     oldov
                sta     facov
                jsr     fsubt                           ; FAC=ARG-FAC
                jsr     negop                           ; negate FAC
                lda     #<expcon
                ldy     #>expcon
                jsr     poly
                lda     #0
                sta     arisgn                          ; multiply by positive 1.0

                pla                                     ; recall scale factor
                jsr     mldexp                          ; modify facexp and check for overflow
                rts                                     ; (has to do jsr due to pla's in muldiv)


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
