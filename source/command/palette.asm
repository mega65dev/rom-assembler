

;*****************************************************************
;* PALETTE   set palette colors
;*
;*  Syntax : PALETTE {screen|COLOR}, color_index, red, green, blue
;*           PALETTE RESTORE
;*
;*           parm1 = screen  0-3     [910711]
;*           parm2 = color_index 0-255
;*           parm3 = red           0-31 (b0-3 red, b4=fgbg)  [910520]
;*           parm4 = green         0-15
;*           parm5 = blue          0-15
;*****************************************************************

C65__setpalette
                 cmp #restore_token                       ; restore palette?
                 bne l271_1                               ; no
                 jsr chrget                               ; yes- advance past Restore token
                 jmp _palette_init

l271_1           cmp #color_token                         ; set physical color register?
                 bne l271_2                               ; no- set logical color register
                 sta GKI__parm1
                 jsr chrget                               ; yes- advance past Color token
                 jsr getbyt
                 bra l271_3

l271_2           jsr getbyt                               ; get screen#
                 cpx #4                                   ; [910711]
                 +lbcs fcerr
                 stx GKI__parm1

                 jsr combyt                               ; get color reg #
l271_3           stx GKI__parm2                           ; (GKI will check for out of range????)

set_palette
                 jsr combyt                               ; get red & fgbg
                 cpx #32                                  ; [910520]
                 +lbcs fcerr
                 stx GKI__parm3

                 jsr getcomnyb                            ; get green
; cpx #16
; bcs 10$
                 stx GKI__parm4

                 jsr getcomnyb                            ; get blue
; cpx #16
; bcs fcerr  ; illegal quantity error
                 stx GKI__parm5

                 lda GKI__parm1                           ; logical or physical color register?
                 bpl l272_1                               ; logical
                 ldx GKI__parm2
                 lda GKI__parm3                           ; physical
                 sta _red,x
                 lda GKI__parm4
                 sta _green,x
                 lda GKI__parm5
                 sta _blue,x
                 bra l272_2

l272_1           jsr ($8012)                              ; go set screen palette
                 +lbcs NoGraphicArea                      ; illegal screen# or color#  [910917]

l272_2           jsr optbyt                               ; get another color reg # ?
                 stx GKI__parm2
                 bcs set_palette                          ; yes- loop
                 rts
