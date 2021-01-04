


; Delete a range of source   -or-   Delete a disk file
;
; Syntax: DELETE from# - to# (same range parameters as LIST)
;  DELETE "filename" (same parameters as SCRATCH)

; Determine which form of DELETE we have...

delete           bcc delete_line                          ; branch if a number (assume range parameter)
                 cmp #minus_token
                 beq delete_line                          ; branch if a dash (assume range parameter)
                 +lbra scratch                            ; branch if string (assume filename or U#)

delete_line
                 jsr errind                               ; direct mode only command
                 jsr chrgot                               ; requires line# or range, no default
                 +lbeq snerr                              ; error, none given

                 jsr range                                ; parse range, find starting line, ptr to ending line
                 lda lowtr
                 ldx lowtr+1
                 sta index1                               ; (destination)
                 stx index1+1

                 jsr FindLine                             ; find ending line
                 bcc l83_2                                ; branch if not found
                 ldy #1
                 jsr indlow                               ; if eot, use this ptr.  else, need ptr to next
                 dey
                 tax                                      ; save it in case of swap
                 bne l83_1                                ; branch if not eot (end-of-text)
                 jsr indlow
                 beq l83_2                                ; branch if eot (null link bytes)

l83_1            jsr indlow
                 sta lowtr                                ; (source)
                 stx lowtr+1

l83_2            lda lowtr                                ; check that start <= end
                 sec
                 sbc index1                               ; calculate delta
                 sta count                                ; (count)
                 lda lowtr+1                              ; (does not catch case where
                 sbc index1+1                             ; start>end when end=start+1,
                 sta argmo                                ; but it does no harm)
                 ora count
                 beq fix_links                            ; all done- nothing to move!?
                 +lbcc snerr                              ; error- bad range (start > end)

                 lda text_top                             ; setup for common DMA move routine: [900530]
                 ldx text_top+1
                 sta index2                               ; index2 = top
                 stx index2+1                             ; index1 = destination
; count  = delta

                 jsr movedown                             ; delete the text, then relink & exit



fix_links                                                 ; <<<<<<<<<<<<<<<<<<<<<<<<<<< entry from renumber

                 jsr link_program                         ; relink program
                 lda index1
                 ldx index1+1
                 clc
                 adc #2
                 bcc l84_1
                 inx
l84_1            sta text_top                             ; set eot pointer
                 stx text_top+1
                 rts                                      ; C128-04 fix: was 'jmp ready' (FAB)


;********************************
;*
;*    Input Range Parameters
;*
;********************************

range            beq l85_1                                ; a terminator from chrgot?
                 bcc l85_1                                ; a number?
                 cmp #minus_token                         ; a dash?
                 bne l85_4   ;if it's not a dash, error (C128-03 fix ; FAB)
                 ldy #1
                 jsr indtxt                               ; let's peek, and see what follows the dash!
                 beq l85_4                                ; uh-oh! it's of the form 'delete -' - error
                 cmp #':'                                 ; the other terminator
                 beq l85_4                                ; ..still bad
                 sec                                      ; set up for linget

l85_1            jsr linget                               ; get first #
                 jsr FindLine                             ; find it & set ptrs
                 jsr chrgot                               ; get last char
                 beq l85_2                                ; skip done
                 cmp #minus_token                         ; a dash?
                 bne l85_4                                ; no- syntax error
                 jsr chrget                               ; yes- skip dash
                 jsr linget                               ; get second #
                 bne l85_4                                ; error- wasn't a number

l85_2            lda endchr                               ; was a # input?
                 bne l85_3                                ; yes
                 lda #$ff                                 ; no - make max
                 sta linnum
                 sta linnum+1
l85_3            rts


l85_4            +lbra snerr                              ; syntax error

;.end