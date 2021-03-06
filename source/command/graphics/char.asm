; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      char.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;*****************************************************************
;* CHAR   draw a character string
;*
;*  Syntax : CHAR column, row, height, width, direction, "string" [,charsetadr [,bank]]
;*
;* parm1  = column#
;* parm2  = row lo
;* parm3  = row hi
;* parm4  = height
;* parm5  = width
;* parm6  = direction
;* parm7  = len of string
;* parm8  = lo addr of string
;* parm9  = hi addr of string
;* parm10 = lo addr of character set $29800 default
;* parm11 = hi addr of character set
;* parm12 = bank of character set   [910912]
;****************************************************************

C65__char
                jsr     CheckGraphicMode
                jsr     getbyt                          ; get column
                stx     GKI__parm1

                jsr     comsad                          ; get row
                sty     GKI__parm2
                sta     GKI__parm3

                jsr     combyt                          ; get height
                stx     GKI__parm4

                jsr     combyt                          ; get width
                stx     GKI__parm5

                jsr     combyt                          ; get direction
                stx     GKI__parm6

                jsr     chkcom
                jsr     frmevl                          ; evaluate the string
                jsr     chkstr                          ; type mismatch error if not string
                ldy     #0
                jsr     indfmo                          ; pointer to string descriptor is left in the fac by frmevl
                sta     GKI__parm7                      ; length  ???? check for null string ????
                pha
                iny
                jsr     indfmo
                sta     GKI__parm8                      ; adrlo
                iny
                jsr     indfmo
                sta     GKI__parm9                      ; adrhi
                jsr     frefac                          ; [910917]
                pla
                jsr     getspa

                jsr     optwrd                          ; get charset address (????bank)
                bcs     l277_1                          ; given
                ldy     #<$9800                         ; not given- use ROM as default   [910207] FAB
                lda     #>$9800                         ; ???? uc/lc or graphic set ????
l277_1          sty     GKI__parm10                     ; lo
                sta     GKI__parm11                     ; hi
                ldx     #2                              ; default to ROM bank 2    [910912] FAB
                jsr     optbyt
                stx     GKI__parm12

                lda     GKI__parm7                      ; ???? check for null string ????
                beq     l277_2
                jmp     ($802c)                         ; bra kg65.char

l277_2          rts


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
