; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      dmode.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;*****************************************************************
;* DMODE   Set Draw Mode
;*
;*  Syntax : DMODE  jam, complement, stencil, style, thickness
;*
;*         parm1 = jam           0-1
;*         parm2 = complement (XOR) 0-1
;*         parm3 = stencil       0-1  <<< not implemented  [911003]
;*         parm4 = style         0-3  <<< only 0-1 implemented [911003]
;*         parm5 = thickness     1-8  <<< not implemented  [911003]
;*******************************************************************

C65__setdmode
                jsr     getbyt                          ; jam mode
                cpx     #2
                bcs     l269_1
                stx     GKI__parm1

                jsr     combyt                          ; complement (xor) mode
                cpx     #2                              ; (ignores jam mode if set)
                bcs     l269_1
                stx     GKI__parm2

                jsr     combyt                          ; stencil mode (not implemented)
                cpx     #2
                bcs     l269_1
                stx     GKI__parm3

                jsr     combyt                          ; style mode
                cpx     #4                              ; 0=solid, 1=pattern, 2=tile (not implemented), 3=reserved
                bcs     l269_1
                stx     GKI__parm4

                jsr     combyt                          ; thickness mode (not implemented)
; dex   ; adjust to 0-7     [911003]
                cpx     #8+1
l269_1          +lbcs   fcerr                           ; illegal quantity error
                stx     GKI__parm5

                jmp     ($8014)                         ; bra setdmode


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
