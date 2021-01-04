

; Routine to translate error message # in .a
; into address of string containing message in index2

erstup          tax                                     ; error set up
                ldy #0                                  ; start with address of first error message
                lda #<error_message_list
                sta index2
                lda #>error_message_list
                sta index2+1

l11_1           dex
                bmi l11_3                               ; finished when .x decrements out

l11_2           lda (index2),y                          ; look at msg, and find end (msb set) (ind.ok)
                inw index2
                and #$ff                                ; was msb set?
                bpl l11_2                               ; no, not end of message
                bra l11_1                               ; yes, tick off another msg

l11_3           rts

;.end