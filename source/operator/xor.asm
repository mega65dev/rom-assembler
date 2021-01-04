; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      xor.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;**************************************************************
;*
;*   XOR - Exclusive-or two 16 bit arguments
;*
;* Syntax : XOR (arg1, arg2)
;*
;**************************************************************

xor             phw     poker                           ; protect the poker value (could be in use)  [910911]
                jsr     chknum
                jsr     getadr                          ; get first arg
                pha                                     ; save MSB
                phy                                     ; save LSB

                jsr     comwrd                          ; check for comma, get word
                jsr     chkcls                          ; check for closing parens

                pla
                eor     poker                           ; xor LSB (comwrd left a copy of its arg in POKER)
                tay
                pla
                eor     poker+1                         ; ..and MSB
                jsr     nosflt                          ; ..and go float 'em

                pla
                sta     poker+1
                pla
                sta     poker
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
