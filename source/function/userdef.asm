; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      userdef.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


; User Defined Function Code
;
; Note only single arguments are allowed to functions, and functions must
; be of the single line form:
;
;  DEF FNA(x)=x~2 + x-2
;
; No strings may be involved with these functions.
;
; Idea: create a simple variable entry whose first character has the MSB set.
; The value will be:
;
;  A text pointer to the formula
;  A pointer to the argument variable

def             jsr     getfnm                          ; get a pointer to the function
                jsr     errdir
                jsr     chkopn                          ; must have a (
                lda     #$80
                sta     subflg                          ; prohibit subscripted & integer variables
                jsr     ptrget                          ; get pointer to argument
                jsr     chknum                          ; is it a number?
                jsr     chkcls                          ; must have )
                lda     #equal_token                    ; followed by =
                jsr     synchr
                pha
                lda     varpnt+1
                pha
                lda     varpnt
                pha
                lda     txtptr+1
                pha
                lda     txtptr
                pha
                jsr     data
                bra     deffin


; Subroutine to get a pointer to a function name

getfnm          lda     #fn_token                       ; must start with fn
                jsr     synchr
                ora     #$80                            ; put function bit on
                sta     subflg                          ; (disallows array & integer variables)
                jsr     ptrgt2                          ; get pointer to function or create anew
                sta     defpnt
                sty     defpnt+1
                +lbra   chknum                          ; make sure it's not a string, and return


fndoer          jsr     getfnm                          ; get the function's name
                lda     defpnt+1
                pha
                lda     defpnt
                pha
                jsr     parchk                          ; evaluate parameter
                jsr     chknum
                pla
                sta     defpnt
                pla
                sta     defpnt+1
                ldy     #2
                jsr     inddef                          ; get pointer to the variable
                sta     varpnt                          ; save variable pointer
                tax
                iny
                jsr     inddef
                beq     errguf
                sta     varpnt+1
                iny                                     ; since def uses only 4


defstf          lda     #varpnt
                jsr     lda_far_ram1
                pha                                     ; push it all on the stack, since we might be recursing
                dey
                bpl     defstf
                ldy     varpnt+1

                jsr     movmf_ram1                      ; put current FAC into our argument variable
                lda     txtptr+1                        ; save variable pointer
                pha
                lda     txtptr
                pha
                jsr     inddef                          ; get pointer to function
                sta     txtptr
                iny
                jsr     inddef
                sta     txtptr+1
                lda     varpnt+1                        ; save variable pointer
                pha
                lda     varpnt
                pha
                jsr     frmnum                          ; evaluate variable, and check numeric
                pla
                sta     defpnt
                pla
                sta     defpnt+1
                jsr     chrgot
                +lbne   snerr                           ; it didn't terminate, syntax error

                pla                                     ; restore text pointer
                sta     txtptr
                pla
                sta     txtptr+1

deffin          ldy     #0
l151_1          pla                                     ; get old arg value off stack,
                phx
                ldx     #defpnt
                jsr     sta_far_ram1 ;sta (defpnt),y    ; and put it back in variable
                plx
                iny
                cpy     #5
                bne     l151_1
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
