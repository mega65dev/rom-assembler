; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      tokeniser.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************
;        CRUNCH
;
;  Entry:  TXTPTR points to start of text to crunch
;  Exit:   TXTPTR points to start of crunched text
;
;  Calls:  CHRGET
;          CHRGOT
;          RESER
;          KLOOP
;          REM
;          DATA
;
;  CRUNCH collapses all reserved words into tokens.  It removes all graphic
;  characters (characters with msb set) not in quoted strings, DATA or REM
;  statements.
;
;  An escape token is implemented as follows:
;
; As each character on a line of text to be crunched is scanned, an
; indirect jump is performed.  Anyone wishing to scan for their own
; commands should grab off this vector, saving the return vector.
; On entry, if the carry flag is set, it is still up for grabs.
; The current text pointer is at TXTPTR.  If the escape routine
; recognizes the command, it should:
;
;  ) put the length of the reserved word in .y
;  ) put the desired 'second' token in .a
;  ) clear the carry flag
;  ) put type of token in x: 0==>command, ff==>function
;
; If it is not your command, leave .a and the carry flag intact.
; NOTE:  The reserved word must be >= 2 characters long.  Exit through
; the old vector (for daisy chaining).  If the carry flag is clear on
; entry it means someone else before you recognized this command.  In
; this case, just pass control through the old vector.


crunch          jmp     (icrnch)


ncrnch          phw     txtptr                          ; save old text pointer

crun05          jsr     chrgot
                bra     crun20

crun10          jsr     chrget


crun20          bcc     crun10                          ; don't crunch numbers
                jmp     (iesclk)                        ; give others a chance at this.  (carry is set)

nesclk
                +lbcc   l8_12                           ; carry clear if someone wanted it
                cmp     #0                              ; end of line?
                beq     l8_10                           ; yes
                cmp     #':'                            ; multi-stmt char?
                beq     crun10                          ; yes
                cmp     #'?'                            ; print ('?') abreviation?
                bne     l8_1                            ; no
                lda     #print_token                    ; yes- substitute print token
                bra     l8_8

l8_1            cmp     #$80                            ; graphics?
                bcc     l8_2                            ; no
                cmp     #pi                             ; pi? (special case)
                beq     crun10                          ; yes, leave alone
                ldy     #1
                jsr     kloop                           ; crunch out graphics
                bra     crun05


l8_2            cmp     #'"'                            ; quote string?
                bne     l8_4                            ; no- try escape token

l8_3            jsr     chrget
                cmp     #0                              ; end of line?
                beq     l8_10                           ; yes
                cmp     #'"'                            ; close quote?
                beq     crun10                          ; yes
                bra     l8_3                            ; no, continue skipping characters


; Crunch escape token

l8_4            lda     #>esc_command_list              ; look for token in escape-command list
                ldy     #<esc_command_list
                jsr     reser
                bcc     l8_5                            ; not found
                lda     #first_esc_command_token+$80-1  ; set up for common escape routine
                ldx     #0                              ; ..flag 'cmd' type escape
                bra     l8_11                           ; ..and go to it.

l8_5            lda     #>esc_function_list             ; look for token in escape-function list
                ldy     #<esc_function_list
                jsr     reser
                bcc     l8_6                            ; not found
                lda     #first_esc_function_token+$80-1 ; set up for common escape routine
                ldx     #$ff                            ; ..flag 'function' type escape
                bra     l8_11                           ; ..and go to it

l8_6            lda     #>keyword_list                  ; look for token in normal list
                ldy     #<keyword_list
                jsr     reser
                bcc     crun10                          ; not found
                cpy     #0                              ; anything to move?
                beq     l8_7                            ; no
                jsr     kloop                           ; crunch it out
l8_7            lda     count

l8_8            ldy     #0
                jsr     sta_far_txt                     ; put token into text  (bleed-thru)
                cmp     #rem_token
                beq     l8_9
                cmp     #data_token
                bne     crun10
                jsr     chrget
                jsr     data
                +lbra   crun05

l8_9            jsr     chrget
                jsr     rem


;  No other statements can follow a REM

l8_10           ldx     txtptr
                pla
                sta     txtptr+1
                pla
                sta     txtptr
                sec                                     ; compute length of line
                txa
                sbc     txtptr
                tay
                iny
                rts


; Crunch out old text, install an escape token

l8_11           adc     count                           ; make pointer into a token
l8_12           pha                                     ; save second token
                dey                                     ; waste (# of chars) - 1
                jsr     kloop

; See if this is function (x=ff) or command (x=0)

                lda     #esc_command_token              ; assume command
                inx
                bne     l8_13                           ; branch if command
                lda     #esc_function_token             ; ..else get correct token

l8_13           ldy     #0
                jsr     sta_far_txt                     ; install escape token... (bleed-thru)
                iny
                pla
                jsr     sta_far_txt                     ; ..and second token  (bleed-thru)
                jsr     chrget                          ; skip over token,
                +lbra   crun10                          ; ..and continue with line.


;      KLOOP
;
;  Crunch loop.  Moves offset .y characters from txtptr to end of line.
;  .x is preserved

kloop           clc                                     ; compute source address
                tya
                adc     txtptr
                sta     index1
                lda     txtptr+1
                adc     #0
                sta     index1+1
                ldy     #$ff

l9_1            iny
                lda     (index1),y                      ; move source..  ????assumes text in common area
                sta     (txtptr),y                      ; to destination offset ????assumes text in common area
                bne     l9_1                            ; not end of line
                rts


;      RESER
;
;  Search reserved word list for a match
;
;  Entry:  (txtptr) is first char of word to match
;    (y,a) is start of table to check
;
;  Exit:   .y  length of word matched
;    .c  success/fail (set/clear) flag
;    count token value

reser           sta     index1+1
                sty     index1
                ldy     #0
                sty     count
                dey
l10_1           iny
l10_2           lda     (txtptr),y                      ; assumes common memory
                bmi     l10_7                           ; abrieviation    [900510]
                sec
                sbc     (index1),y                      ; does letter match? (ind.ok)
                beq     l10_1                           ; yes...continue
                cmp     #$80                            ; end of word?
                beq     l10_6                           ; yes...c set...done


;  find next word

l10_3           lda     (index1),y                      ; ind.ok
                bmi     l10_4                           ; found end of current
                iny
                bne     l10_3
l10_4           iny                                     ; start of next
                inc     count                           ; value of token
                clc
                tya
                adc     index1
                sta     index1
                bcc     l10_5
                inc     index1+1
l10_5           clc
                ldy     #0
                lda     (index1),y                      ; end of list? ind.ok
                bne     l10_2                           ; no


;  yes...carry clear...fail

l10_6           ora     count                           ; .a=$80 if match
                sta     count                           ; token is formed
                rts


; special case- last character is shifted (necessary for 'diR' compatibility)

l10_7           sec                                     ; allow last chr to be shifted   [900510]
                sbc     (index1),y                      ; does letter match? (ind.ok)
                beq     l10_8                           ; yes- end of word
                cmp     #$80                            ; end of word?
                beq     l10_6                           ; yes
                bne     l10_3                           ; no- next word

l10_8           lda     #$80                            ; last chr is shifted & so is end of current word
                bra     l10_6

;.end




; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
