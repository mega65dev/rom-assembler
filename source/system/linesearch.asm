



; FindLine
; Searches the program text for the line whose number is passed in "linnum".
; There are two possible returns:
;
; 1) carry set.
;  Line found.  (lowtr) points to the link bytes of line sought.
;
; 2) carry clear.
;  Line not found.  (lowtr) points to the link bytes of the next
;  line greater than the one sought.

FindLine
                lda txttab                              ; init pointer to beginning of program
                ldx txttab+1

FindLink
                sta lowtr                               ; current position in program
                stx lowtr+1
                ldy #1
                jsr indlow                              ; end of program (null link)?
                beq l33_3                               ; yes, exit with .c=0 (not found)
                iny
                iny
                jsr indlow                              ; get line number of this line (high byte first)
; sta syntmp
; lda linnum+1 ;is this the line we're looking for?
; cmp syntmp
; bcc l33_4  ; no- too high, so the line does not exist, exit
; beq l33_1
; dey  ; no- too low, so get link to next line
; bra l33_2
                cmp linnum+1                            ; is this the line we're looking for?   [910925]
                beq l33_1                               ; maybe
                bcs l33_3                               ; no- too high, so the line does not exist, exit with .c=0
                dey                                     ; no- too low, so get link to next line
                bra l33_2

l33_1           dey                                     ; maybe- have to check low byte
                jsr indlow
; sta syntmp
; lda linnum
; cmp syntmp
; bcc l33_4  ; no- too high, exit
; beq l33_4  ; yes- got it, exit
                cmp linnum                              ; is this the line we're looking for?   [910925]
                beq l33_4                               ; yes- got it, exit with .c=1
                bcs l33_3                               ; no- too high, so the line does not exist, exit with .c=0

l33_2           dey                                     ; get link to next line
                jsr indlow
                tax
                dey
                jsr indlow
                bra FindLink                            ; continue looking


l33_3           clc                                     ; exit, line not found (.c=0)
l33_4           rts                                     ; exit, line found (.c=1)

;.end