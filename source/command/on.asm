; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      on.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;*********************************************************
;* ON expression {GOTO | GOSUB} line_number
;*********************************************************
ongoto
                jsr     getbyt                          ; get & save GOTO/GOSUB
                pha
                cmp     #goto_token                     ; GOTO?
                beq     l50_1                           ; yes
                cmp     #gosub_token                    ; GOSUB?
                +lbne   snerr                           ; no, syntax error

l50_1           dec     faclo
                bne     l50_2                           ; skip another line number
                pla                                     ; get dispatch character
                +lbra   xeqcm2

l50_2           jsr     chrget                          ; advance and set codes
                jsr     linget                          ; read next line
                cmp     #','                            ; is it a "comma"?
                beq     l50_1
                pla                                     ; remove stack entry (token)
                rts                                     ; either end of line or syntax error

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
