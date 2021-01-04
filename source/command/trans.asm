

; Trans subroutine

trans            jsr sendp                                ; build string to output
                 jsr _clrch
                 ldx #sys_bank                            ; name is in system space, bank0 ????  [910620]
                 txa
                 jsr _setbank
                 jsr _open                                ; send it...
                 php                                      ; save error status (.c)
                 pha                                      ; save error code (if any)
                 lda dosla
                 sec
                 jsr _close                               ; special close...
                 pla                                      ; pop error
                 plp                                      ; pop error status
                 +lbcs erexit                             ; ...branch if there was an error opening
                 rts

;.end