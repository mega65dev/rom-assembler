

; Function to get a line one character at a time from the input
; channel and build it in the input buffer.
;

PromptedInput                                             ; qinlin.
                 lda channl                               ; entry for things line INPUT, wanting a prompt
                 bne InputLine                            ; prompt only if terminal
                 jsr outqst                               ; yes- print '? '
                 jsr realsp


InputLine                                                 ; inlin.
                 ldx #0                                   ; read & buffer data until 'return' or buffer full
l28_1            jsr inchr                                ; get a character
                 cmp #0
                 beq l28_2
                 cmp #cr                                  ; a carriage return?
                 beq l28_2                                ; yes...done build

                 sta buf,x                                ; no...buffer it
                 inx
                 cpx #buflen                              ; buffer full?
                 bcc l28_1                                ; no...continue
                 +lbra errlen                             ; yes...string too long error


l28_2            lda #0                                   ; fininl.  terminate input with a null
                 sta buf,x
                 ldx #<buf_txtptr                         ; set up pointer to start of buffer-1 (for chrget)
                 ldy #>buf_txtptr
                 lda channl                               ; print 'return' only if terminal
                 +lbeq crdo
                 rts

;.end
