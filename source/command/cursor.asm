; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      cursor.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;*****************************************************************
;*   CURSOR [ON|OFF,] [column] [,row [,style] ]
;*
;*   where: column,row = x,y logical screen position
;*  style      = flashing (0) or solid (1)
;*  ON,OFF     = to turn the cursor on or off
;*****************************************************************

cursor          cmp     #on_token                       ; Check for ON | OFF
                clc
                beq     l261_3                          ; turn cursor on (.c=0)
                cmp     #esc_command_token
                bne     l261_1                          ; (might be a function)
                jsr     chkesc
                cmp     #off_token                      ; turn cursor off (.c=1)
                beq     l261_3
                +lbra   snerr

l261_1          pha                                     ; Evaluate cursor position parameters
                sec
                jsr     _plot                           ; get current cursor position & save it
                stx     srow
                sty     column

                ldx     column                          ; get new column, default=current column
                pla
                cmp     #','
                beq     l261_2                          ; not given, use default
                jsr     getbyt
l261_2          stx     column
                ldx     srow                            ; get new row, default=current row
                jsr     optbyt
; stx srow
                ldy     column
                clc
                jsr     _plot                           ; set new cursor position
                +lbcs   fcerr                           ; error if bad position

                jsr     optzer                          ; Get new cursor type   ???? assumes screen output
                bcc     l261_4                          ; not given, exit
                lda     #esc
                jsr     _bsout                          ; use escape sequence to set
                txa
                and     #1
                eor     #1                              ; [910808]
                clc
                adc     #'E'                            ; 0=F=flash, 1=E=solid
                jmp     _bsout                          ; set it and exit

l261_3          jsr     _cursor                         ; Turn cursor ON or OFF per .c

                jsr     chrget                          ; eat token, get next character
                beq     l261_4                          ; eol- exit
                jsr     chkcom                          ; else, must be comma
                bra     l261_1                          ; it is- go evaluate position

l261_4          rts                                     ; eol


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
