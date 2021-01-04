
; Return the color of a given X,Y pixel location on the drawscreen  [910801]
;  PIXEL (x,y)

pixel           jsr CheckGraphicMode                    ; verify screen open
                jsr PushParms                           ; preserve Graphics parameters & LINNUM  [910820]

                jsr getsad                              ; get x
                sty GKI__parm1
                sta GKI__parm2
                jsr comsad                              ; get y
                sty GKI__parm3
                sta GKI__parm4
                jsr chkcls                              ; check for closing parens

                jsr ($8032)                             ; get Bitplane data at pixel (x,y), returned in .y
                jsr sngflt                              ; go float 1 byte arg in .Y

                jsr PopParms                            ; restore graphics parameters
                rts
