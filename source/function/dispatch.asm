; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      dispatch.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; At this point, eval has determined that the token in a has to be a
; function.  It must therefor be in the range SGN...MID$ (old BASIC),
; or RGR...INSTR (new extensions).  We will collapse these two disjoint
; blocks into one continuous range.
;
; On entry, we can assume the token is >= 'sgn'

isfun           cmp     #esc_function_token             ; is this an escape function?
                beq     do_esc_fn                       ; yes
                cmp     #last_function_token+1
                bcs     snerr1                          ; no- must be syntax error
                cmp     #mid_token+1
                bcc     l18_1                           ; no need to adjust
                sbc     #rgraphic_token-mid_token-1

l18_1           pha                                     ; save token
                tax
                jsr     chrget                          ; set up for synchk.
                cpx     #instr_token-1                  ; look for (adjusted) instr token
                beq     l18_2                           ; yes
                cpx     #rgraphic_token-1               ; look for rgraphic which now takes 2 args [910801]
                +lbeq   rgraphic                        ; yes

                cpx     #mid_token+1
                bcs     oknorm                          ; LEFT$,RIGHT$,MID$ require multiple args
                cpx     #left_token                     ; is it past last single-arg function?
                bcc     oknorm                          ; no, must be normal function


; Most functions take a single argument.  The return address of these functions
; is CHKNUM, which ascertains that VALTYP=0 (numeric).  Normal functions which
; return string results (eg. CHR$) must pop off that return address and return
; directly to FRMEVL.
;
; The so called "funny" functions can take more than one argument, the first
; of which must be string and the second of which must be a number between 0
; and 255.  The closed parenthesis must be checked and return is directly to
; FRMEVL with the text pointer pointing beyond the ")".  The pointer to the
; description of the string argument is stored on the stack underneath the
; value of the integer argument.

l18_2           jsr     chkopn                          ; check for an open parenthesis
                jsr     frmevl                          ; eat open paren and first argument
                jsr     chkcom                          ; two args so comma must delimit
                jsr     chkstr                          ; make sure first was string

                pla                                     ; check token
                cmp     #instr_token-1                  ; special case: INSTR() bails out here
                +lbeq   instr
                ldx     facmo+1                         ; push address of string arg1
                phx
                ldx     facmo
                phx
                pha                                     ; push token
                jsr     getbyt                          ; get arg2
                pla                                     ; retrieve token
                phx                                     ; push value of arg2
                bra     fingo                           ; go set up to evaluate fn



oknorm
                jsr     parchk                          ; check for open parens, evaluate argument
                pla                                     ; restore token

fingo
                sec                                     ; convert token to index into jump table
                sbc     #first_function_token
                asl
                tay
                lda     fundsp+1,y
                sta     jmper+2
                lda     fundsp,y
                sta     jmper+1
                jsr     jmper                           ; dispatch
;string functions remove this ret addr
                +lbra   chknum                          ; check for "numeric-ness" and return

;;[[system.escape]]

; Escape Function handler

do_esc_fn
                jsr     chrget                          ; get second token
                +lbeq   snerr                           ; error if no second token
                cmp     #pointer_token
                beq     l19_1                           ; skip pre-parse if 'POINTER()'
                pha
                jsr     chrget                          ; should be '('
                jsr     chkopn
                jsr     frmevl                          ; evaluate first argument
                pla
l19_1           cmp     #first_esc_function_token       ; see if this esc fn is one of ours
                bcc     foreign_esc_fn                  ; nope.
                cmp     #last_esc_function_token+1
                bcs     foreign_esc_fn                  ; nope

; Convert to index into the function dispatch table

                adc     #last_function_token-first_esc_function_token-1
                bra     fingo                           ; always

foreign_esc_fn
                sec                                     ; flag 'up for grabs'
                jsr     go_foreign_esc_fn
n_esc_fn_vec
                +lbcs   snerr                           ; it's unwanted. off to the refuse pile
                +lbra   chknum

go_foreign_esc_fn
                jmp     (esc_fn_vec)


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
