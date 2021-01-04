


;*****************************************************************
;* POLYGON   draw a regular n-sided Polygon
;*
;*  POLYGON  X,Y, Xradius, Yradius, sides [,drawsides [,subtend [,angle [,solid] ]]]
;*
;*         parm1 = center x lo
;*         parm2 = center x hi
;*         parm3 = center y lo
;*         parm4 = center y hi
;*         parm5 = xradius lo
;*         parm6 = xradius hi
;*         parm7 = yradius lo
;*         parm8 = yradius hi
;*         parm9 = solid flag 0-1
;*         parm10 = sa lo (starting angle 0-360)
;*         parm11 = sa hi
;*         parm12 = # of sides to draw (1 to 127)
;*         parm13 = # of sides (3 to parm12)
;*         parm14 = subtend flag 0-1
;****************************************************************

C65__polygon                                              ; changed BASIC syntax to something more reasonable [910923] FAB
                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get center x
                 sty GKI__parm1
                 sta GKI__parm2

                 jsr comsad                               ; get center y
                 sty GKI__parm3
                 sta GKI__parm4

                 jsr comwrd                               ; get x radius
                 sty GKI__parm5
                 sta GKI__parm6

                 jsr comwrd                               ; get y radius
                 sty GKI__parm7
                 sta GKI__parm8

                 jsr combyt                               ; get number of sides
                 cpx #3
                 bcc l274_2                               ; too few
                 cpx #128
l274_1           +lbcs fcerr                              ; too many
                 stx GKI__parm13

; ldx GKI__parm13  ;get number of sides to draw (default=#sides)
                 jsr optbyt
                 cpx #1                                   ; must be at least 1 side
l274_2           +lbcc fcerr
                 stx GKI__parm12
                 dex
                 cpx GKI__parm13                          ; draw sides must be <= #sides
                 bcs l274_1

                 jsr optzer                               ; get subtend flag
; cpx #2
; bcs l274_1
                 stx GKI__parm14

                 jsr optwrd                               ; get starting angle (default=0 degrees)
                 sty GKI__parm10                          ; lo
                 sta GKI__parm11                          ; hi

                 jsr optzer                               ; get solid flag
; cpx #2
; bcs l274_1
                 stx GKI__parm9

                 jmp ($801e)                              ; bra polygon
