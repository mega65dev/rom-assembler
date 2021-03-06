; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      dclear.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

; DCLEAR - reinitilaize the drive

dclear          jsr     dospar                          ; parse the line
                ldy     #finit                          ; set code
                lda     #2
                jsr     trans                           ; send command
                jsr     print_dos_error                 ; if any
                +lbra   dclall


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
