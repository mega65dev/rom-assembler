

;*****************************************************************
;* ELLIPSE   draw an Ellipse
;*
;*  Syntax : ELLIPSE  CenterX, CenterY, RadiusX,  RadiusY  [,solid flag]
;*
;*         parm1 = center x lo
;*         parm2 = center x hi
;*         parm3 = center y lo
;*         parm4 = center y hi
;*         parm5 = x radius lo
;*         parm6 = x radius hi
;*         parm7 = y radius lo
;*         parm8 = y radius hi
;*         parm9 = solid flag 0-1
;*****************************************************************

C65__ellipse
                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get center x
                 sty GKI__parm1
                 sta GKI__parm2

                 jsr comsad                               ; get center y
                 sty GKI__parm3
                 sta GKI__parm4

                 jsr comsad                               ; get xradius
                 sty GKI__parm5
                 sta GKI__parm6

                 jsr comsad                               ; get yradius
                 sty GKI__parm7
                 sta GKI__parm8

                 jsr optzer                               ; get solid flag
                 cpx #2
                 +lbcs fcerr
                 stx GKI__parm9

                 jmp ($8020)                              ; bra ellipse
