; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      collision.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;*****************************************************************
; COLLISION Command
;
; Syntax:  COLLISION n [,address]
;
; Where:   n= 1 ==> sprite / sprite
;   2 ==> sprite / background
;   3 ==> light pen
;
; Address ==> BASIC line number to trap to on interrupt
;      (no address ==> disable trapping)
;*****************************************************************

collision
                jsr     getbyt                          ; get type in .X
                dex                                     ; adjust 1..3 to 0..2
                cpx     #3
                +lbcs   fcerr                           ; value error

                phx                                     ; save collision type
                jsr     optwrd                          ; get address (line number) in .Y,.A (optional)
                plx
; php   ;save .C (.C == 1 ==> real value)
                sta     int_adr_hi,x                    ; save address given
                sty     int_adr_lo,x

                lda     intval                          ; this records valid interrupts
                ora     sbits,x                         ; set correct bit
; plp
                bcs     l297_1                          ; ..unless this is a 'clear',
                eor     sbits,x                         ; ..in which case we'll reset bit
l297_1          sta     intval
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
