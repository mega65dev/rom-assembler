; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      window.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;****************************************************************
;*
;*  WINDOW Command
;*
;*  Syntax : WINDOW upper-left col, upper-left row,
;*      lower-left col, lower-right row [,clear]
;*
;*  Where  :  0 <= row <= 24
;*       0 <= column <= (80/40)
;*       clear : 0 (no) or 1 (yes)
;*
;****************************************************************

window          jsr     getbyt                          ; get u.l. col
                cpx     #80
                bbr7    _mode,l116_1
                cpx     #40
l116_1          bcs     l116_4
                stx     window_temp

                jsr     combyt                          ; get u.l. row
                cpx     #25
                bcs     l116_4
                stx     window_temp+1

                jsr     combyt                          ; get l.r. column
                cpx     #80
                bbr7    _mode,l116_2
                cpx     #40
l116_2          bcs     l116_4
                stx     window_temp+2
                cpx     window_temp                     ; can't be less than u.l. column
                bcc     l116_4

                jsr     combyt                          ; get l.r. row
                cpx     #25
                bcs     l116_4
                stx     window_temp+3
                cpx     window_temp+1                   ; can't be less than u.l. row
                bcc     l116_4

                jsr     optzer                          ; get optional clear flag
                cpx     #2
                bcs     l116_4
                phx

                ldx     window_temp
                lda     window_temp+1
                clc
                jsr     _set_window

                ldx     window_temp+2
                lda     window_temp+3
                sec
                jsr     _set_window

                ldx     #19                             ; assume 'home', not 'cls'
                pla
                beq     l116_3
                ldx     #147
l116_3          txa
                jmp     _bsout

l116_4          +lbra   fcerr                           ; illegal value error

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
