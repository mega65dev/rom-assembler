
execute_a_line                                            ; EXECUTE PLAIN TEXT IN BUFFER
                 stx txtptr                               ; init buffer pointer
                 sty txtptr+1
                 jsr chrget                               ; get first character of null-terminated string
                 tax
                 beq main                                 ; got null input
                 bcc l25_1                                ; got line number
                 jsr crunch                               ; got text- tokenize buffer,
                 jsr chrgot                               ; get first command (token),
                 +lbra xeqdir                             ; and execute it


;ADD or DELETE NEW LINE
l25_1            jsr linget                               ; evaluate line number, put into into linnum
                 bbr4 runmod,l25_2
                 jsr edit_crunch                          ; if edit mode, find end of input   [910620]
                 bra l25_3

l25_2            jsr crunch                               ; tokenize rest of input if not edit mode
l25_3            sty count                                ; save length
                 jsr FindLine                             ; locate line in program
                 +lbcc nodel                              ; not found, go insert line into program
; else delete current line and insert this one