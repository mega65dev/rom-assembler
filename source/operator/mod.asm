; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      mod.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;**************************************************************
;*
;* MOD  -  Modulus of a number
;*
;* Syntax : MOD (number, range)
;*      910402 FAB
;**************************************************************

; Calculate   MOD = NUMBER-RANGE*INT(NUMBER/RANGE)

mod             jsr     chknum                          ; 1st arg in FAC1 (number)
                jsr     pushf1                          ; save two copies of it for later
                jsr     pushf1
                jsr     chkcom                          ; check for comma
                jsr     frmnum                          ; 2nd arg in FAC1 (range)
                jsr     chkcls                          ; check for closing paren

                jsr     movaf                           ; save range in FAC2
                jsr     pullf1                          ; get back number in FAC1
                ldx     #5                              ; swap FAC1 and FAC2
l148_1          lda     facexp,x
                ldy     argexp,x
                sta     argexp,x
                sty     facexp,x
                dex
                bpl     l148_1
                jsr     pushf1                          ; save one copy of range for later

                jsr     fdivt_c65                       ; number/range
                jsr     int                             ; INT(number/range)
                jsr     movaf                           ; round & move to FAC2
                jsr     pullf1                          ; retrieve arg2 (range)
                jsr     fmultt_c65                      ; range*INT(number/range)
                jsr     negop                           ; -range*INT(number/range)
                jsr     movaf                           ; move to FAC2
                jsr     pullf1                          ; retrieve arg1 (number)
                +lbra   faddt_c65                       ; number-range*INT(number/range)


;.end


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
