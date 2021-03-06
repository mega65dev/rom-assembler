; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      pointer.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;******************************************************************
;
; POINTER(var_name) - Return address of descriptor for var_name
;
;******************************************************************

pointer         jsr     chrget                          ; skip over escape token
                jsr     chkopn                          ; test for open paren
                jsr     isletc                          ; test if character follows parens
                +lbcc   snerr                           ; ...syntax error if not.
                jsr     ptrget                          ; look for this varname in table

pointer_ret     =*-1
                tax
                phy
                jsr     chkcls                          ; look for closing paren
                txa
                tay
                pla
                cmp     #>zero                          ; is this a dummy pointer?
                bne     l147_1
                lda     #0                              ; if so, return 0
                tay
l147_1          +lbra   nosflt

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
