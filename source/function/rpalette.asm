; Return the R,G, or B component of a color     [910820]
; RPALETTE (screen#, color#, rgb)

rpalette
                jsr CheckGraphicMode                    ; verify screen open
                jsr PushParms                           ; Save graphics parameters

                jsr conint                              ; get screen# in .x
                cpx #4
                bcs l300_1                              ; illegal screen#
                stx GKI__parm1

                jsr combyt                              ; get color# in .x ????check for legal color#
                stx GKI__parm2

                jsr ($8034)                             ; get RGB components of color# in PARM3,4,5
                bcs l300_1                              ; something is wrong????

                jsr combyt                              ; get r,g,b component#
                cpx #3
l300_1          +lbcs fcerr                             ; illegal value

                ldy GKI__parm3,x                        ; get r,g,b value
                jsr sngflt                              ; float 1 byte arg in .y

                jsr chkcls                              ; check for closing paren
                jsr PopParms                            ; restore graphics parameters
                rts


PushParms                                               ; [910820]
                ply                                     ; Grab return address
                plz

                phw linnum                              ; Save 'poker' value

                ldx #17-1
l301_1          lda GKI__parm1,x                        ; Save Graphics parameters
                pha                                     ; [eg: CHAR x,y,1,1,2,str$(PIXEL(x,y))]
                dex
                bpl l301_1

                phz                                     ; Restore return address
                phy
                rts


PopParms                                                ; [910820]
                ply                                     ; Grab return address
                plz

                ldx #0
l302_1          pla                                     ; Restore Graphics parameters
                sta GKI__parm1,x
                inx
                cpx #17
                bcc l302_1

                pla                                     ; Restore 'poker' value
                sta linnum+1
                pla
                sta linnum

                phz                                     ; Restore return address
                phy
                rts

;.end