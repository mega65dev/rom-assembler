



; LINGET  Reads a line # from the current txtptr position
;   and stores it in linnum  (valid range is 0-63999).
;
;   On exit txtptr is pointing to the terminating char
;   which is in .a with condition codes set.
;   Endchr will be =0 if no digit input, else >0.  Use it
;   to distinguish between line # 0 & null input.

linget          ldx #0                                  ; enter with CHRGET flags set
                stx endchr                              ; flags line # input
                stx linnum                              ; init line # to 0
                stx linnum+1

l34_1           bcs l34_5                               ; it's not a digit, do rts
l34_2           inc endchr                              ; indicate line # input
                sbc #$2f                                ; '0'-1 since .c=0
                sta charac                              ; save for later
                lda linnum+1
                sta index
                cmp #25                                 ; line number will be < 64000?
                bcc l34_3                               ; yes, continue
                bbs1 helper,l34_5                       ; no, if called by AutoScroll it's okay
                +lbra snerr                             ; else syntax error

l34_3           lda linnum
                asl                                     ; multiply by 10
                rol index
                asl
                rol index
                adc linnum
                sta linnum
                lda index
                adc linnum+1
                sta linnum+1
                asl linnum
                rol linnum+1
                lda linnum
                adc charac                              ; add in digit
                sta linnum
                bcc l34_4
                inc linnum+1
l34_4
; jsr chrget  ;ALLOW SPACES to terminate number  [910620]
; bra l34_1
                inw txtptr                              ; get next character from text
                ldy #0                                  ; re-get current character from text
                jsr indtxt                              ; lda (txtptr),y from RAM0
                cmp #' '                                ; space=eol    [910708]
                beq l34_6
                cmp #':'                                ;
                bcs l34_5                               ; eol
                sec
                sbc #'0'                                ; alpha or numeric?
                sec
                sbc #$d0
                bcc l34_2                               ; numeric
l34_5           rts                                     ; exit

l34_6           jsr chargt                              ; terminating character is a space, eat it just this once
                +lbra chrtst                            ; return with flags set appropriately (esp. for 'range')

;.end
