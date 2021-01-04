


;******************************************************************
;* RSPRITE - Return sprite information
;*
;* Syntax : RSPRITE (sprite_number, argument)
;*
;* Where  : sprite_number = [0..7]
;*   argument = [0..5]
;*   0 : enabled?   (y(1)/n(0))
;*   1 : color?     (0-15)
;*   2 : priority over background? (y(1)/n(0))
;*   3 : expand in x direction? (y(1)/n(0))
;*   4 : expand in Y direction? (y(1)/n(0))
;*   5 : multicolor sprite?  (y(1)/n(0))
;******************************************************************

rsprite          jsr conint                               ; get first arg, sprite #, in .X
; dex  ;adjust [1..8] to [0..7]   [910220]
                 cpx #8  ; (318018-03 mod                 ; fab)
                 bcs l303_1                               ; value error
                 txa
                 pha                                      ; save sprite number

; jsr chkcom ;check for proper delimiter
; jsr getbyt ;do frmevl, get 1 byte arg (arg) in .X
                 jsr combyt                               ; [910820]
                 jsr chkcls                               ; look for closing paren
                 cpx #6
l303_1           +lbcs fcerr                              ; value error

                 ply                                      ; sprite number
; jsr put_io_in_map
                 lda vic+39,y                             ; assume 'color'
                 and #$0f                                 ; range 0-15
; inc a  ;adjust to 'keyboard' colors   [910724]
                 cpx #1
                 beq l303_2                               ; it was color. set up for float

                 lda rspmod,x                             ; get index for this function
                 tax
                 lda sbits,y                              ; get mask for this sprite number
                 and vic,x
                 beq l303_2
                 lda #1                                   ; return all non-zeros as '1'

l303_2           tay
                 +lbra sngflt                             ; go float 1 byte arg in .Y


rspmod           !text 21,39,27,29,23,28                  ; VIC registers associated with arg#

;.end



;******************************************************************
;* RSPCOLOR - return sprite multicolor reg's
;*
;* Syntax : RSPCOLOR (argument)
;*
;* Where  : argument = [1..2]
;*   1 : return multicolor #1
;*   2 : return multicolor #2
;******************************************************************

rspcolor
                 jsr chkcls                               ; check for closing paren
                 jsr conint                               ; get arg in .X
                 dex                                      ; adjust [1..2] to [0..1
                 cpx #2
                 +lbcs fcerr                              ; value error

; jsr put_io_in_map
                 lda vic+37,x
                 and #$0f
                 tay
; iny  ;range 0-15     [910724]
                 +lbra sngflt                             ; float 1 byte arg in .Y

;.end