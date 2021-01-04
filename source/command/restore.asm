; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      restore.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;*********************************************************************
;*
;* RESTORE Command
;*
;* Reset pointers to next DATA statement.  Allows optional argument
;* specifying a specific line number, otherwise the default is the
;* beginning of text area.
;*
;*********************************************************************

restor
                beq     restore__1                      ; branch if no argument...use default
                jsr     getwrd                          ; get 2 byte argument (???? no check for real number means a var legal)
                sty     linnum
                sta     linnum+1
                jsr     FindLine                        ; get pointer to specified line
                +lbcc   userr                           ; error if not found

                lda     lowtr                           ; decrement 2 byte pointer, and save it
                ldy     lowtr+1
                bra     restore__2                      ; always


restore__1                                              ; entry from FLOAD
                sec
                lda     txttab
                ldy     txttab+1

restore__2
                sbc     #1
                bcs     l73_1
                dey
l73_1           sta     datptr
                sty     datptr+1
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
