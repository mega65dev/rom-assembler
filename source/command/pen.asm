
;*****************************************************************
;* PEN
;*
;*  Syntax : PEN  Pen#, ColorReg
;*
;*           parm1 = pen#  0-2
;*           parm2 = color reg#  0-255
;*****************************************************************

C65__setpen
                 jsr getbyt                               ; get pen#
                 cpx #3                                   ; range 0-2
                 +lbcs fcerr                              ; branch if out of range
                 stx GKI__parm1

                 jsr combyt                               ; get color reg#
;???? error check for max color allowed
; for the current screen.
                 stx GKI__parm2

                 jmp ($8010)                              ; bra setpen
