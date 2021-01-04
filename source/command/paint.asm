


;*****************************************************************
;* PAINT   fill a graphic area with color
;*
;*  Syntax : PAINT x, y [,mode [,color]]
;*
;* parm1  = x lo
;* parm2  = x lo
;* parm3  = y lo
;* parm4  = y hi
;* parm5  = mode
;* parm6  = color
;*
;* fill color is pen-A
; mode 0: fill region defined by color at x,y (default) new modes [910916] FAB
; mode 1: fill region using given color as boundary
; mode 2: fill connected region
;****************************************************************

C65__paint                                                ; new [910228] FAB
                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get x
                 sty GKI__parm1
                 sta GKI__parm2

                 jsr comsad                               ; get y
                 sty GKI__parm3
                 sta GKI__parm4

                 ldx #0                                   ; [910916]
                 jsr optbyt                               ; mode, default = 0 (fill region pointed to)
                 cpx #3
                 +lbcs fcerr                              ; (range 0-2)
                 stx GKI__parm5
                 ldx #0
                 jsr optbyt                               ; boundary color, default = 0
                 stx GKI__parm6

l278_1           jsr garba2                               ; create space in var bank for paint stack [910716]
                 lda strend
                 sta GKI__parm7                           ; pass pointer to bottom of bank-1 free space
                 lda strend+1                             ; (top of stack)
                 sta GKI__parm8
                 sec
                 lda fretop                               ; pass pointer to top of free space
                 sbc #3                                   ; (bottom of stack)
                 sta GKI__parm9
                 lda fretop+1
                 sbc #0
                 sta GKI__parm10

                 jsr ($802e)                              ; bra paint
                 bcs l278_2                               ; error- stack overflow or stop key
                 rts

l278_2           cpx #errom
                 +lbeq error                              ; stack overflow, say 'out of memory'
                 +lbra break_exit                         ; user hit stop key
