; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      sprcor.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;  SPRCOR  -- Get sprite position coordinate
;

sprcor          jsr     chkcom_1                        ; check for a comma
sprcor_1
                ror     numcnt                          ; reset msb if comma else set msb
                bpl     l292_1                          ; skip if got a comma
                cmp     #';'                            ; test if angular data
                beq     l292_3                          ; skip if yes - 2 msb's = 1 1
                cmp     #'#'                            ; test if speed type
                beq     l292_2                          ; skip if yes - 2 msb's = 0 1
                +lbra   snerr                           ; syntax error if none of above

l292_1          jsr     chrgot                          ; test for relative coordinate
                cmp     #plus_token                     ; test if plus sign
                beq     l292_3                          ; skip if yes - show relative
                cmp     #minus_token                    ; test if minus sign
                beq     l292_3                          ; skip if yes - show relative
l292_2          clc                                     ; reset to show absolute
l292_3          ror     numcnt                          ; shift in second flag bit

sadwrd          jsr     frmnum                          ; get number     label [910307]
                +lbra   getsad                          ; get signed 2 byte coordinate,do rts


;*************************************************************
; CHKCOM_1  --  Check for a comma
;
;  carry set & eq = end of string
;  carry set & neq = not a comma
;  carry clear = a comma
;*************************************************************

chkcom_1
                jsr     chrgot                          ; get character in input stream
                beq     l293_2                          ; skip if end of string
                cmp     #','                            ; check if a comma
                clc
                beq     l293_1                          ; skip if yes
                sec                                     ; set carry if not
l293_1          php
                pha
                jsr     chrget                          ; move to next non-space character
                pla
                plp
l293_2          rts


sproff          !text 0,11,22,33,44,55,66,77            ; sprite offsets into speed table

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
