


;****************************************************************
;*
;* IF Statment
;*
;* IF exp {GOTO line#  | THEN {line# | statements | b-block} }
;*  [:ELSE {line# | statements | b-block} ]
;*
;* B-block
;*
;* BEGIN : [statement(s) on one or more lines] : BEND
;*
;****************************************************************

if              jsr frmevl                              ; evaluate the conditional expression
                jsr chrgot                              ; re-get current character
                cmp #goto_token                         ; is terminating character a GOTO?
                beq l44_1                               ; yes
                lda #then_token                         ; no, it must be THEN
                jsr synchr

l44_1           lda facexp                              ; test truth value of argument
                bne if_true                             ; branch if true

if_false
                jsr chrgot                              ; is there a b-block?
                cmp #esc_command_token
                bne l45_1                               ; no, must be an escape command
                iny                                     ; might be, look at escape token
                jsr indtxt
                cmp #begin_token
                bne l45_1                               ; branch if not
                jsr find_bend                           ; skip to end of b-block

l45_1           jsr data                                ; may be 'else' clause. first skip over 'then' clause..
                ldy #0
                jsr indtxt                              ; ..and see if end of stmt or end of line
                beq rem                                 ; end of line, no 'else'. go to next line
                jsr chrget                              ; another statement on this line.. is it 'else'?
                cmp #else_token
                bne l45_1                               ; no, keep looking on this line
                jsr chrget                              ; yes! skip over token and execute clause (below)

if_true         jsr chrgot
                beq l46_2                               ; branch if end of statement
                bcs l46_1                               ; branch if not a number
                +lbra goto                              ; here if of the form 'THEN line#'

l46_1           cmp #esc_command_token                  ; is this the beginning of a b-block?
                bne l46_2                               ; no, must be an escape command
                iny                                     ; might be, look at escape token
                jsr indtxt
                cmp #begin_token
                bne l46_2
                jsr chrget                              ; skip over 'BEGIN' if so...
                jsr chrget                              ; ..and the second token, as well.

l46_2           jsr chrgot                              ; get back original character, & set up flags
                +lbra xeqcm3                            ; ..and go execute whatever it is


find_bend                                               ; ... subroutine to find end of current b-block
                jsr chrget
                bne l47_3

; End of statement.. set up next

l47_1           cmp #':'                                ; is this EOL?
                beq find_bend                           ; no, keep looking

l47_2           bbr7 runmod,l47_7                       ; EOL: branch if direct mode, 'block terminator not found' error

                ldy #2
                jsr indtxt                              ; end of text?
                beq l47_7                               ; yes, msb of next stmt pointer = 0. error

                iny
                jsr indtxt
                sta curlin                              ; set up next line of text
                iny
                jsr indtxt
                sta curlin+1
                tya
                clc
                adc txtptr
                sta txtptr
                bcc find_bend
                inc txtptr+1
                bra find_bend                           ; always

l47_3           cmp #'"'
                bne l47_4
                jsr un_quote                            ; look for terminating quote, or EOL
                beq l47_1                               ; EOL or ':' after closing quote
                bne find_bend                           ; ..else normal char, keep looking

l47_4           cmp #rem_token                          ; REM?
                bne l47_5                               ; no
                jsr rem                                 ; yes, trash this line
                bra l47_2                               ; and go test for end of text

l47_5           cmp #esc_command_token                  ; is this a BEND?
                bne find_bend                           ; can't be, has to be an escape

                jsr chrget                              ; skip over esc token
                cmp #bend_token
                beq l47_6                               ; this is what we came for, bye!

                cmp #begin_token                        ; not a BEND. is it a BEGIN?
                bne find_bend                           ; it's just a normal, stick-in-the-mud char. keep looking.

                jsr find_bend                           ; oh-oh, recursion. Dr. Ja-Ja warned me about this.
                bra find_bend

l47_6           rts

l47_7           ldx #err_no_bend
                +lbra error

un_quote                                                ; txtptr points to a '"'. look for closing '"', or EOL
                ldy #0
l48_1           inw txtptr
                jsr indtxt
                beq l48_2                               ; EOL, get out here with .z set and a '00' in .a
                cmp #'"'
                bne l48_1                               ; keep looking until quote
                jmp chrget                              ; got closing quote, get byte after quote, set flags

l48_2           rts



else            cmp #esc_command_token                  ; is this of the form "ELSE b-block"?
                bne l49_1                               ; no, must be an escape command
                iny                                     ; might be, look at escape token
                jsr indtxt
                cmp #begin_token
                bne l49_1                               ; no, justa plain-old "ELSE statement"
                jsr find_bend                           ; yes, it is a b-block. skip over the b-block.
l49_1           +lbra rem


;.end