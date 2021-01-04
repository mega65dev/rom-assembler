; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      binary.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

orop            ldy     #255                            ; must always complement
                !text $2c

andop           ldy     #0
                sty     count                           ; operator
                jsr     ayint                           ; (facmo&lo)=int value and check size
                lda     facmo                           ; use Demorgan's Law on high
                eor     count
                sta     integr
                lda     faclo                           ; and low
                eor     count
                sta     integr+1
                jsr     movfa
                jsr     ayint                           ; (facmo&lo)=int of arg
                lda     faclo
                eor     count
                and     integr+1
                eor     count                           ; finish out Demorgan
                tay                                     ; save high
                lda     facmo
                eor     count
                and     integr
                eor     count
                +lbra   givayf                          ; float (a,y) and return to user



; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
