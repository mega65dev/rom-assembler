; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      continue.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;**********************************************************
;*
;* CONTINUE Execution after STOP/END
;*
;**********************************************************

cont            bne     cont_rts                        ; make sure there is a terminator
                bbs4    runmod,edit_err                 ; [910620]
                bbs7    runmod,cont_rts                 ; if in run-mode just rts

                ldx     #errcn                          ; continue error.
                ldy     oldtxt+1                        ; a stored txtptr of zero set up by INIT_STACK
                +lbeq   error                           ; indicates there is nothing to continue

                lda     oldtxt                          ; STOP, END, typing crlf to INPUT, and STOP key
                sta     txtptr
                sty     txtptr+1
                lda     oldlin
                ldy     oldlin+1
                sta     curlin
                sty     curlin+1

setexc          smb7    runmod                          ; set up run mode
                lda     #0
                sta     autinc                          ; turn auto increment off
                sta     autinc+1
                sta     intval                          ; enable & reset collision-trapping mechanism
                sta     _autoinsert                     ; disable auto-insert mode ?????

                ldx     #2                              ; turn off all interrupt trip flags
l72_1           sta     int_trip_flag,x
                dex
                bpl     l72_1

                jsr     _setmsg                         ; turn kernel messages off & rts

cont_rts
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
