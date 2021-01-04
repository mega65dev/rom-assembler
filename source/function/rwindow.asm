; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      rwindow.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;******************************************************************************
;
; RWINDOW  - Returns information about the current console display environment.
;
;   Syntax : RWINDOW (n)
;
;   Where: n=0 : number of lines in the current window
;   =1 : number of rows in the current window
;   =2 : returns either 40 or 80, depending on the
;   current console device
;
;******************************************************************************

rwindow         jsr     chkcls
                jsr     conint
                cpx     #2
                beq     l149_2                          ; return current console
                +lbcs   fcerr

                cpx     #0
                bne     l149_1

                lda     _screen_bottom
                sec
                sbc     _screen_top
                bra     l149_3                          ; always

l149_1          lda     _screen_right
                sec
                sbc     _screen_left
                bra     l149_3                          ; always


l149_2          lda     #80                             ; assume 80 col
                bbr7    _mode,l149_3
                lsr
l149_3          tay
                +lbra   sngflt                          ; float 1 byte arg in .Y

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
