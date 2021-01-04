
; Return the color of a drawscreen's PEN      [910820]
;  RPEN (pen#) where pen# = 0,1,2

rpen             jsr CheckGraphicMode                     ; verify screen open
                 jsr PushParms                            ; preserve Graphics parameters & LINNUM  [910820]

                 jsr conint                               ; get 1 byte arg in .x (old style single arg function)
                 cpx #3
                 bcs l299_1                               ; illegal pen #?
                 stx GKI__parm1

                 jsr ($8036)                              ; convert to logical color# (palette index#)
l299_1           +lbcs fcerr                              ; drawscreen not set or illegal quantity somewhere

                 jsr sngflt                               ; go float 1 byte arg in .Y

                 jsr PopParms                             ; restore graphics parameters
                 rts

