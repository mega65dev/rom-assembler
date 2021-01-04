; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      newclr.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;
; The NEW command clears the program text as well as variable space.
;

new             beq     init_text                       ; Erase program in memory
                cmp     #restore_token                  ; Restore an erased program?    [910103]
                +lbne   snerr                           ; no- syntax error    [910410]
                jsr     chkeos                          ; yes- eat token, error if not eos  [910429]
                lda     txttab                          ; "seed" first link to fool 'chead'
                ldx     txttab+1
                sta     index
                stx     index+1
                lda     #0
                ldy     #1
                ldx     #index
                jsr     sta_far_ram0                    ; clear msb  (bleed-thru)
                dey
                inc
                jsr     sta_far_ram0                    ; set lsb   (bleed-thru)
                +lbra   renumber                        ; make renumber check it for us (not 100%) & relink


init_text
                lda     txttab                          ; find the bottom of basic text
                ldx     txttab+1
                sta     index
                stx     index+1
                dew     index                           ; (the absolute bottom)

                lda     #0
                tay
                ldx     #index
                jsr     sta_far_ram0                    ; clear bottom     (bleed-thru)
                iny
                jsr     sta_far_ram0                    ; clear first link bytes    (bleed-thru)
                iny
                jsr     sta_far_ram0                    ; (bleed-thru)
                clc
                lda     txttab
                adc     #2
                sta     text_top                        ; set up (text_top), the end of text
                lda     txttab+1
                adc     #0
                sta     text_top+1

                rmb5    runmod                          ; trcflg. reset trace flag


runc            jsr     reset_txtptr                    ; load (txtptr) with (txttab)-1
                bra     clearc                          ; "CLR" to clear vars    [910410]


; CLeaR Routines
;

; Special forms of CLR command:
;
; CLR ERR$ Clears program error status, useful in TRAP handlers which
;   have resolved an error & wish to RESUME with a clean status.
;
; CLR DS$  Clears the currently buffered DS,DS$ messages.  The next
;   use of DS or DS$ will make BASIC re a new message from DOS.

clear           beq     clearc                          ; branch if no args    [910410]

                cmp     #err_token                      ; CLR ERR$
                bne     l41_1                           ; no
                jsr     chkeos                          ; yes- eat token & error if not eos
                +lbra   error_clear                     ; and go clear ERR$

l41_1           cmp     #'D'                            ; CLR DS$     [910717]
                bne     l41_2                           ; no- error
                jsr     chrget
                cmp     #'S'
                bne     l41_2
                jsr     chrget
                cmp     #'$'
l41_2           +lbne   snerr                           ; no- error
                jsr     chkeos
                +lbra   Clear_DS                        ; yes- clear current DS$


; Clearc is a subroutine which initializes the variable and array space by
; resetting STREND (the end of array storage).  It falls into INIT_STACK,
; which resets the stack.

clearc          jsr     _clall                          ; close all files
                ldy     #0
                sty     dsdesc                          ; flag 'no DS$ string'
                dey                                     ; (y=$ff)
                sty     trapno+1                        ; flag no current trap line
                sty     errlin                          ; reset last error pointers
                sty     errlin+1
                sty     errnum

                lda     max_mem_1                       ; clear string space
                ldy     max_mem_1+1
                sta     fretop
                sty     fretop+1

                lda     #<stkbot                        ; empty run-time stack
                ldy     #>stkbot
                sta     tos
                sty     tos+1

                lda     vartab
                ldy     vartab+1
                sta     arytab                          ; this will delete all variables,
                sty     arytab+1
                sta     strend                          ; ..and arrays
                sty     strend+1

                ldx     #pumony-puchrs                  ; reset print using chars
l42_1           lda     pudefs,x
                sta     puchrs,x
                dex
                bpl     l42_1

fload           jsr     restore__1                      ; reset pointer for DATA statements



; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
