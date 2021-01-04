; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      errors.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

print_dos_error                                         ; [900725]
                bbs7    runmod,header_rts               ; branch if not direct mode
                jsr     Check_DS                        ; get current disk error message
                ldy     #0
                lda     #dsdesc+1
                jsr     lda_far_ram1                    ; lda (dsdesc+1),y peek at first character
                cmp     #'2'
                bcc     header_rts                      ; branch if no error occured ('00' or '01')
                cmp     #'7'
                bne     l227_1                          ; [900730]
                iny
                lda     #dsdesc+1
                jsr     lda_far_ram1                    ; might be '73' powerup message
                cmp     #'3'
                beq     header_rts                      ; yup

; ldx #errbdk  ; bad disk error (carry is set)
; bra error

; Print DOS error message as if it were a BASIC error message   [900910]

l227_1          lda     #$ff                            ; reset error line
                sta     errlin                          ;
                sta     errlin+1
                jsr     _clrch
; inc a   ;a=0 restore output to screen   [910909]
                sta     channl
                jsr     RestoreTextScreen               ; make sure we're in text mode????  [910404]
                jsr     init_stack                      ; clean up system, string temps, etc.  [910121]

                jsr     highlight_text                  ; [910624]
                jsr     _primm                          ; start a new line with '?DOS: '
                !text cr,"?DOS: ",0                     ; (to distinguish ?DOS: SYNTAX ERROR from ?SYNTAX ERROR)

                ldy     #3                              ; print text part of message only
                lda     #dsdesc+1
                jsr     lda_far_ram1                    ; skip err#, comma, & leading space if any
                cmp     #' '
                bne     l227_3
                iny
l227_2          lda     #dsdesc+1
                jsr     lda_far_ram1
                cmp     #','                            ; finished at comma preceding trk, sector
                beq     l227_4
l227_3          jsr     outch
                iny
                bpl     l227_2                          ; loop always (bpl=failsafe)

l227_4          jsr     highlight_done                  ; [910624]
                jsr     crdo
                +lbra   ready                           ; we're in direct mode, error msg has been printed, abort

header_rts
                clc
                rts


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
