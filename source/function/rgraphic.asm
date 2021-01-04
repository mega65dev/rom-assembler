


; Return graphic screen status & parameters      [910826]
; RGRAPHIC (screen, param) where param = 0 open (1), closed (0), or invalid (>1)
;            1 width  (0=320, 1=640, 2=1280)
;            2 height (0=200, 1=400)
;            3 depth (1-8 bitplanes)
;            4 bitplanes used  (bitmask)
;            5 bank A blocks used (bitmask)
;            6 bank B blocks used (bitmask)
;            7 drawscreen # (0-3)
;            8 viewscreen # (0-3)
;            9 drawmodes  (bitmask)
;           10 pattern type  (bitmask)
;
; Requires a kludge, because RGR used to be a normal 1-arg function in the C128
; but now it takes two args.

rgraphic
; jsr CheckGraphicMode ;verify screen open
                pla                                     ; remove token from stack
                jsr PushParms                           ; preserve Graphics parameters & LINNUM  [910820]

                jsr chkopn                              ; check for open paren
                jsr getbyt                              ; get screen # in .X
                stx GKI__parm1
                jsr combyt                              ; get param # in .X
                cpx #10+1                               ; [911028]
                bcs l298_1                              ; illegal param #
                phx
                jsr chkcls                              ; check for closing parens

                jsr ($8038)                             ; read screen params
l298_1          +lbcs fcerr                             ; bad input????

                lda GKI__parm2
                plx                                     ; get back desired param #
                dex
                bpl l298_2
                eor #$80                                ; make 0=closed, 1=open, >1=invalid
                lsr
                lsr
                bra l298_3                              ; return screen open status

l298_2          dex
                bpl l298_5
l298_3          lsr
l298_4          lsr
                lsr
                lsr
                and #3
                bra l298_8                              ; return width, height

l298_5          dex
                bpl l298_6
                and #8
                bra l298_4
l298_6          dex
                bpl l298_7
                and #7                                  ; return depth
                inc                                     ; make depth 1-8
                bra l298_8

l298_7          lda GKI__parm3,x                        ; return bp bask, banks, etc.
l298_8          tay
                jsr sngflt                              ; float 1 byte arg in .y

                jsr PopParms                            ; restore Graphics parameters & LINNUM
                rts
