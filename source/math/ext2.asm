


gtbytc          jsr chrget

getbyt          jsr frmnum                              ; read formula into FAC

conint          jsr posint                              ; convert the FAC to a single byte int
                ldx facmo
                +lbne fcerr                             ; result must be <= 255
                ldx faclo
                jmp chrgot                              ; set condition codes on terminator


getnum                                                  ; get 2-byte value in y,a: check for a comma, get 1 byte val in x
                jsr frmnum                              ; get address
                jsr getadr                              ; get that location

combyt                                                  ; check for a comma, get a 1 byte value in x
                jsr chkcom                              ; check for comma
                bra getbyt                              ; get something to store and return


comwrd          jsr chkcom

getwrd          jsr frmnum                              ; get an unsigned 2-byte value in y,a

getadr          lda facsgn                              ; for this entry, value can't be < 0
                +lbmi fcerr                             ; function call error

getsad                                                  ; get a signed 2-byte value in (y,a), ///entry from sprcor
                lda facexp                              ; examine exponent
                cmp #145
                +lbcs fcerr                             ; function call error
                jsr qint                                ; integerize it
                lda facmo
                ldy facmo+1
                sty poker
                sta poker+1
                rts                                     ; it's all done

;.end