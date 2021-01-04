


; Find a specific token in the run-time stack. token to be found is in srchtk.
;
; If called by 'for' or 'next', scan entries in stack, looking for a specific
; 'for-variable' (in (forpnt)).  If found, (fndpnt) will point to it, and z is
; set.  Otherwise, (fndpnt) will point to either:
;  1) the non-for token
;  2) bottom-of-stack
;
; Special case: 'next' with no argument will match first 'for' entry on stack
; found, if any.  This case is signaled by a (forpnt) with a msb of $ff (an
; impossible value).
;
; All other calls to search will result in either:
;  1) (success) z = 1, (fndpnt) = address
;  2) (failure) z = 0


; Set up temporary pointer with current top of stack

search          sta srchtk                              ; save token to search for
                jsr movtos                              ; tos => fndpnt


; Test if pointer is at bottom of stack.  If so, the item was not found.

l29_1           lda fndpnt
                cmp #<stkbot
                bne l29_2                               ; (fndpnt) <> bottom, ok
                lda fndpnt+1                            ; lsb's the same, test msb's
                cmp #>stkbot
                beq l29_6                               ; stack empty, rts

l29_2           ldy #0
                lda srchtk                              ; what are we looking for?
                cmp #for_token                          ; 'for' tokens are special cases
                bne l29_4

; Looking for a 'for' token.  If next token examined is not a 'for' token,
; return with z = 0.  Otherwise, check the pointer to its 'for' variable.
; If the variable pointer = (forpnt) or if (forpnt) = $FFxx, return with z=1.
; Otherwise, set up x with length of a 'for' entry, and use the usual
; mechanisim for examining the next entry.

                cmp (fndpnt),y                          ; indirect ok- looking at runtime stack????
                bne l29_7                               ; not 'for', do rts with z = 0
                ldy #2                                  ; point to msb of 'for' variable
                lda forpnt+1
                cmp #$ff
                beq l29_7                               ; do rts with z = 1
                cmp (fndpnt),y
                bne l29_3                               ; not right variable, keep looking.
                dey
                lda forpnt                              ; test lsb
                cmp (fndpnt),y
                beq l29_7                               ; a hit! rts with z = 1

l29_3           ldx #lenfor
                bra l29_5                               ; keep looking

l29_4           lda (fndpnt),y
                cmp srchtk                              ; is this the correct type of entry?
                beq l29_7                               ; rts with z = 1

; The entry on top of the run-time stack is not the entry we are looking for.
; Find out what is there, and advance temp. pointer past it.

                ldx #lenfor                             ; is it a 'for' entry?
                cmp #for_token
                beq l29_5
                ldx #5                                  ; must be gosub or do by default

l29_5           txa
                clc
                adc fndpnt
                sta fndpnt
                bcc l29_1
                inc fndpnt+1
                bra l29_1                               ; always

l29_6           ldy #1                                  ; clear z flag
l29_7           rts


; GETSTK
;
; Add (.A) elements to top of run-time stack.  Error if result exceeds tos.

getstk          eor #$ff                                ; make value 2's comp.
                sec
                adc tos
                sta tos
                ldy tos+1
                bcs l30_1
                dey
l30_1           sty tos+1
                cpy #>stktop
                +lbcc omerr
                bne l30_2
                cmp tos
                +lbcc omerr
l30_2           rts


; (a,y) is a certain address.  REASON makes sure it is less than (fretop).

reason          cpy fretop+1
                bcc l31_4
                bne l31_1                               ; go garbage collect
                cmp fretop
                bcc l31_4

l31_1           pha
                ldx #9                                  ; if tempf2 has zero in between
                tya

l31_2           pha
                lda highds-1,x                          ; save highds on stack
                dex
                bpl l31_2                               ; put 8 of them on stack
                jsr garba2                              ; go garbage collect
                ldx #$f7

l31_3           pla
                sta highds+9,x                          ; restore after garbage collect
                inx
                bmi l31_3
                ply
                pla                                     ; restore .a and .y
                cpy fretop+1                            ; compare highs
                bcc l31_4
                +lbne omerr                             ; higher is bad
                cmp fretop                              ; compare the lows
                +lbcs omerr
l31_4           rts



;  Utilities involved in the operation of the BASIC run-time stack.


; Move top-of-stack pointer to (fndpnt)

movtos          lda tos
                sta fndpnt
                lda tos+1
                sta fndpnt+1
                rts



; move (fndpnt) to (tos)

movfnd          lda fndpnt
                sta tos
                lda fndpnt+1
                sta tos+1
                rts

; Reduce size of run-time stack by (y).  No error checking performed!

rlsstk          tya
                clc
                adc tos
                sta tos
                bcc l32_1
                inc tos+1
l32_1           rts

;.end