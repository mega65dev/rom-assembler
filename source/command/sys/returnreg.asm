


; RREG - Return values of 6502 registers following a SYS call.
;
; Syntax : RREG [.A variable [,[.X[...Z] variable] [,[.S variable] ]]]

rreg             lda #0
                 sta count

l65_1            jsr chrgot
                 beq l65_4                                ; reached end of statement- done
                 cmp #','                                 ; skip this arg?
                 beq l65_3                                ; branch if so
                 jsr ptrget                               ; get pointer to target variable
                 sta forpnt                               ; a little bit of set up so we can share LET code
                 sty forpnt+1
                 lda valtyp                               ; what kind of variable name did ptrget find?
                 +lbne chkerr                             ; type mismatch error if string

                 ldy count                                ; which register's value are we looking for?
                 lda _a_reg,y                             ; .A, .X, .Y, & .Z are contiguious
                 cpy #4
                 bne l65_2
                 lda _s_reg                               ; but .S isn't

l65_2            tay                                      ; low byte in .Y
                 lda #0                                   ; high byte of zero
                 jsr givayf                               ; go float it
                 lda intflg                               ; set conditions for type of var (int/float)
                 jsr qintgr                               ; ..and use part of LET to do the work

l65_3            inc count                                ; 5 registers to do
                 lda count
                 cmp #5
                 bcs l65_4
                 jsr chrgot                               ; was this e-o-statement?
                 beq l65_4
                 jsr chrget                               ; not e-o-s, skip over comma,
                 bne l65_1                                ; ..and go do next

l65_4            rts

;.end