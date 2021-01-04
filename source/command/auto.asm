; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      auto.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; AUTO Increment
;   Syntax :    auto {line-number} (line-number = 0 means turn off)

auto
                jsr     errind
                jsr     linget
                lda     linnum
                sta     autinc
                lda     linnum+1
                sta     autinc+1
                rts

;.end



help            ldx     errnum                          ; check for error status
                inx
                beq     l67_1                           ; exit if there is no current error
                lda     errlin
                ldy     errlin+1
                sta     linnum
                sty     linnum+1
                jsr     FindLine                        ; find the beginning of line with error
                bcc     l67_1                           ; exit if line not found?

                jsr     crdo                            ; begin a new line
                ldx     linnum
                lda     linnum+1
                ldz     helper
                rmb4    helper                          ; temporarily disable token highlighting
                smb7    helper                          ; set 'help' flag for P1LINE
                jsr     p1line                          ; display line & highlight error
                stz     helper
l67_1           rmb7    helper                          ; reset 'help' flag
                +lbra   crdo                            ; and return to caller



helpsb                                                  ; logic to highlight error or find string
                bbs4    helper,highlight_done           ; branch if highlighting tokens
                bbs5    helper,l68_3                    ; branch if FIND

                ldx     lowtr+1                         ; has P1LINE reached code in error?
                tya
                clc
                adc     lowtr                           ; add character pointer to line pointer...
                bcc     l68_1
                inx
l68_1           cpx     errtxt+1                        ; and compare to error pointer
                bne     l68_2                           ; not there
                cmp     errtxt
                bcs     highlight_text                  ; we're there- begin highlighting
l68_2           rts


l68_3           cpy     fndpnt                          ; at first character of find string?
                bcc     l68_5                           ; before it
                lda     find_count
                beq     l68_5                           ; past it
                bmi     l68_6                           ; at last character
                cmp     fstr1+2
                bcc     l68_4                           ; in middle of string
                jsr     highlight_text                  ; at first char- start highlight
l68_4           dec     find_count                      ; one less character to highlight
                beq     l68_4                           ; special case-
;make it negative for next time around
l68_5           rts

l68_6           inc     find_count                      ; make it zero


highlight_done                                          ; nasty kludge to colorize error or found text
                lda     highlight_save
                bmi     l69_1                           ; (unless it's already normal)
                sta     _color                          ; restore normal color
                ora     #$80
                sta     highlight_save                  ; mark highlight_save invalid
                rmb7    helper                          ; remove HELP flag
                rmb1    helper                          ; remove token flag
l69_1           rts


highlight_text                                          ; nasty kludge to colorize error or found text
                bit     highlight_save
                bpl     l70_1                           ; (unless it's already highlighted)
                lda     _color                          ; save current (normal) color
                sta     highlight_save                  ; msb=0 to mark highlight_save valid
                lda     highlight_color
                sta     _color                          ; change color to highlight
l70_1           rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
