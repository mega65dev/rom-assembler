
;*****************************************************************
;* LINE  draw a dot, a line or a stick shape
;*
;*  Syntax : LINE  x0, y0 [,[x1] [,y1]]...
;*
;* parm1,2 = x0 (lo/hi)
;* parm3,4 = y0
;* parm5,6 = x1 (x1,y1)=(x0,y0) if not specified
;* parm7,8 = y1
;*****************************************************************

C65__line
                 cmp #input_token                         ; special check for 'line input#'  [910103]
                 +lbeq linputn                            ; yes
                 cmp #input_token+1                       ; special check for 'line input'
                 +lbeq linput                             ; yes

                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get x0
                 sty GKI__parm1
                 sta GKI__parm2
                 sty GKI__parm5                           ; [910228]
                 sta GKI__parm6

                 jsr comsad                               ; get y0
                 sty GKI__parm3
                 sta GKI__parm4
                 sty GKI__parm7                           ; [910228]
                 sta GKI__parm8

                 jsr optsad                               ; get x1     [910228]
                 bcc l273_2                               ; use x0
l273_1           sty GKI__parm5
                 sta GKI__parm6

l273_2           jsr optsad                               ; get y1     [910228]
                 bcc l273_3                               ; use y0
                 sty GKI__parm7
                 sta GKI__parm8

l273_3           jsr ($8018)                              ; draw a line from x0,y0 to x1,y1

                 ldx #3
l273_4           lda GKI__parm5,x                         ; copy x1,y1 to x0,y0
                 sta GKI__parm1,x
                 dex
                 bpl l273_4

                 jsr optsad                               ; more?
                 bcs l273_1                               ; yes, continue
                 rts
