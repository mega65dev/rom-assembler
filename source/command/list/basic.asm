; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      basic.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;******************************************************
; P1LINE Print 1 line of BASIC text
;
; Entry: (a,x) contains line number low,high
;  (lowtr) points to beginning of line
;
; next-line   line-num  BASIC text......  null
; lo    hi    lo    hi  byte byte...byte   00
;        ^           ^     ^
;    (lowtr)        .A    .X
;******************************************************

p1line          bbr4    runmod,l37_1                    ; [910620]
                +lbra   edit_p1line                     ; handle things differently for plain text

l37_1           ldy     #3
                sty     lstpnt
                sty     dores                           ; reset quote-switch
                jsr     linprt                          ; print line number
                lda     #' '                            ; print a space

p1l010          ldy     lstpnt
                and     #$7f

p1l015          cmp     #':'                            ; end-of-stmt?     [900516]
                bne     l38_1                           ; no
                bbr7    helper,l38_1                    ; yes, but skip e-o-s check if not HELP...
                bbs7    dores,l38_1                     ; or ':' is inside quotes
                jsr     highlight_done                  ; yes, restore normal text color
                lda     #':'

l38_1           jsr     outch                           ; outdo
                cmp     #'"'                            ; if quote character, toggle quote-switch
                bne     l38_2
                lda     dores
                eor     #$ff
                sta     dores

l38_2           iny                                     ; point to next character (should never wrap)
                bbs0    helper,l38_3                    ; branch if highlighting tokens
                bbs5    helper,l38_3                    ; branch if called by FIND/CHANGE
                bbr7    helper,l38_4                    ; branch if called by LIST or HELP satisfied
l38_3           jsr     helpsb

l38_4           jsr     indlow
                +lbeq   highlight_done                  ; finished when trailing null is found
                jmp     (iqplop)                        ; usually points to nqplop


nqplop                                                  ; <<<<<<< vector entry
                bpl     p1l015                          ; not a token, just print character
                bbs7    dores,p1l015                    ; branch if inside quotes, print chr as is

;  At this point, we know we're talking token.  Scan the token text
;  list until the correct text is found, and print that text.

                sta     token_saver                     ; save token for REM check   [910626]
                cmp     #esc_command_token              ; is this an escape token?
                beq     print_esc_cmd                   ; yes- escape command
                cmp     #esc_function_token
                beq     print_esc_fn                    ; yes- escape function
                cmp     #pi
                beq     p1l015                          ; no- pi is >$80, but should be printed 'as is'
                tax
                sty     lstpnt                          ; no- use the token as index into ROM keyword list
                lda     #>keyword_list
                ldy     #<keyword_list

; Scan list pointed to by (y,a) for token in (x), and print token's text

p1l026          sta     index1+1                        ; index1 points to token text list in ROM
                sty     index1
                ldy     #0                              ; begin scanning lists for this token's text
                dex
                bpl     p1l070                          ; what luck! it's the first one

l39_1           inw     index1                          ; scan text until next command found
                lda     (index1),y                      ; ind.ok (ROM)
                bpl     l39_1                           ; loop until terminal char (msb=1)
                dex                                     ; is next text the one we want?
                bmi     l39_1                           ; no, keep scanning
                inw     index1                          ; yes, point to first character

                bbr3    helper,p1l070                   ; found text for this token, is it REM?  [910626]
                lda     token_saver                     ; [910628]
                cmp     #rem_token
                beq     p1l071                          ; yes, and REM highlighting is enabled

p1l070                                                  ; found text for this token
                bbr4    helper,p1l072                   ; branch if not highlighting tokens
                lda     (index1),y                      ; peek at first character
                bmi     p1l010                          ; branch if operator (1-byte, msb=1)
                smb0    helper                          ; else begin highlight
p1l071          jsr     highlight_text

p1l072          lda     (index1),y                      ; get char from ROM table
                bmi     p1l010                          ; msb=1=last char this token, contine line
                jsr     outch                           ; else print it
                iny
                bra     p1l072


; Print Escape Command

print_esc_cmd
                tax                                     ; save type (cmd) in case it is a foreign esc token
                iny
                jsr     indlow                          ; look at second token
                +lbeq   p1l015                          ; none?  print funny character
                sty     lstpnt
                cmp     #first_esc_command_token        ; is this one of ours?
                bcc     print_foreign_esc               ; nope
                cmp     #last_esc_command_token+1
                bcs     print_foreign_esc               ; nope
                adc     #$80-first_esc_command_token    ; yes- make a pointer p1l will be proud of
                tax
                ldy     #<esc_command_list
                lda     #>esc_command_list
                bra     p1l026                          ; go scan list and print it



; Print Escape Function

print_esc_fn
                tax                                     ; save type (function) in case it's a foreign esc token
                iny
                jsr     indlow                          ; look at second token
                +lbeq   p1l015                          ; none?  print funny character
                sty     lstpnt
                cmp     #first_esc_function_token       ; is this one of ours?
                bcc     print_foreign_esc               ; nope
                cmp     #last_esc_function_token+1
                bcs     print_foreign_esc               ; nope
                adc     #$80-first_esc_function_token   ; yes- make a pointer p1l will be proud of
                tax
                ldy     #<esc_function_list
                lda     #>esc_function_list
                bra     p1l026                          ; go scan list and print it


; The token to be printed is an escape token which is NOT recognized by BASIC.
; We will jump through the indirect chain and see if anyone claims this token.
;
; At this point:
; .C = 1 to signal 'unclaimed'
; .X = type (0==>command, ff==>function)
; .A = second token character
;
; If anyone claims this token, they should:
;
; > Clear .C to flag 'taken'
; > Point (INDEX1) at the string to be printed (with msb of last char set)
; > Note: string to print MUST be in RAM-0!

print_foreign_esc
                cpx     #esc_command_token
                bne     l40_1
                ldx     #0
                !text $2c

l40_1           ldx     #$ff
                sec
                jmp     (iescpr)

nescpr          +lbcs   p1l015                          ; no takers, print a funny graphic character
                ldy     #0
                bra     p1l070


;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
