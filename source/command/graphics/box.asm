; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      box.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

;*****************************************************************
;* BOX   draw a 4-sided figure
;*
;*  Syntax :  BOX x0,y0, x1,y1, x2,y2, x3,y3 [,solid flag]
;*
;* parm1,2   = x0  (lo/hi)
;* parm3,4   = y0
;* parm5,6   = x1
;* parm7,8   = y1
;* parm9,10  = x2
;* parm11,12 = y2
;* parm13,14 = x3
;* parm15,16 = y3
;* parm17    = solid flag
;*****************************************************************

C65__box
                jsr     CheckGraphicMode
                jsr     sadwrd                          ; get x0
                sty     GKI__parm1
                sta     GKI__parm2

                jsr     comsad                          ; get y0
                sty     GKI__parm3
                sta     GKI__parm4

                jsr     comsad                          ; get x1
                sty     GKI__parm5
                sta     GKI__parm6

                jsr     comsad                          ; get y1
                sty     GKI__parm7
                sta     GKI__parm8

                jsr     comsad                          ; get x2
                sty     GKI__parm9
                sta     GKI__parm10

                jsr     comsad                          ; get y2
                sty     GKI__parm11
                sta     GKI__parm12

                jsr     comsad                          ; get x3
                sty     GKI__parm13
                sta     GKI__parm14

                jsr     comsad                          ; get y3
                sty     GKI__parm15
                sta     GKI__parm16

                jsr     optzer                          ; get solid flag
                stx     GKI__parm17

                jmp     ($801a)                         ; bra box


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
