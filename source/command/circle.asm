
;*****************************************************************
;* CIRCLE   draw a Circle
;*
;*  Syntax : CIRCLE  CenterX, CenterY, radius [,solid flag]
;*
;*         parm1 = center x lo
;*         parm2 = center x hi
;*         parm3 = center y lo
;*         parm4 = center y hi
;*         parm5 = radius lo
;*         parm6 = radius hi
;*         parm7 = solid flag 0=no, 1=yes
;*****************************************************************

C65__circle
                jsr CheckGraphicMode
                jsr sadwrd                              ; get center x
                sty GKI__parm1
                sta GKI__parm2

                jsr comsad                              ; get center y
                sty GKI__parm3
                sta GKI__parm4

                jsr comsad                              ; get radius
                sty GKI__parm5
                sta GKI__parm6

                jsr optzer                              ; get solid flag
                cpx #2
                +lbcs fcerr
                stx GKI__parm7

                jmp ($801c)                             ; bra circle

