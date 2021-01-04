


;************************************************************************
;  RCOLOR (source)  --  return current color assigned to source
;   0  :  Background color
;   1  :  Foreground color
;   2  :  Highlight color
;   3  :  Border color
;************************************************************************

rcolor           jsr conint                               ; evaluate integer argument, put in .X
; jsr put_io_in_map

                 cpx #4
                 +lbcs fcerr                              ; illegal qty
                 txa
                 asl                                      ; make into word pointer
                 tax
                 lda color_source,x                       ; get address of source
                 sta grapnt
                 lda color_source+1,x
                 sta grapnt+1
                 ldy #0
                 lda (grapnt),y                           ; read source (aways system space or I/O????)
                 and #$0f                                 ; mask unused bits
                 tay
; iny   ; make color match keytops
                 +lbra sngflt                             ; float 1 byte in .Y

color_source
                 !word vic+33,_color,highlight_color,vic+32
