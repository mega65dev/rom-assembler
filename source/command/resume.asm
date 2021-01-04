



; RESUME command
;
; Used to resume execution following a TRAPped error.
;
; Syntax: RESUME [line_number | NEXT]
;
; Can take the following forms:
;
; RESUME   :resume executing at the statement which caused
;     the error.
; RESUME NEXT  :resume execution at the statement FOLLOWING
;     the statement which caused the error.
; RESUME line_number :resume at the specified line number.


resume          jsr errdir                              ; no direct mode
                ldx errlin+1                            ; is there an error to resume from?
                inx
                beq rescnt                              ; can't resume!
                jsr chrgot                              ; look for arguments
                beq resswp                              ; no arg's...restart err'd line
                bcc l93_3                               ; numeric argument
                cmp #next_token                         ; only other choice is 'next'
                +lbne snerr                             ; if not, syntax error

                jsr resswp                              ; resume execution with next stm't
                ldy #0
                jsr indtxt
                bne l93_2                               ; must be a ':'
                iny                                     ; must be a null,get next line
                jsr indtxt                              ; make sure its not end-of-text
                bne l93_1
                iny
                jsr indtxt
                +lbeq ready                             ; 2 nulls, eot. bye!

l93_1           ldy #3                                  ; new line, update pointers
                jsr indtxt
                sta curlin
                iny
                jsr indtxt
                sta curlin+1
                tya
                clc
                adc txtptr
                sta txtptr
                bcc l93_2
                inc txtptr+1
l93_2           jsr chrget                              ; skip over this character, into body of statement
                +lbra data                              ; advance until null or ':', then rts


l93_3           jsr getwrd                              ; resnum. numeric argument
                sta linnum+1
                jsr resend
                +lbra luk4it


resswp          lda errtxt                              ; backup one so chrget will work
                bne l94_1
                dec errtxt+1
l94_1           dec errtxt

                ldx #1
l94_2           lda errlin,x                            ; restore line#
                sta curlin,x
                lda errtxt,x                            ; restore text pointer to statement
                sta txtptr,x
                dex
                bpl l94_2


resend          ldx tmptrp                              ; restore trap line to allow traps again
                stx trapno+1
error_clear
                ldx #$ff
                stx errnum                              ; reset error status- he's saying he's fixed it
                stx errlin
                stx errlin+1                            ; flag 'no further resumes until next error'
                rts


rescnt          ldx #errcr
                +lbra error

;.end
