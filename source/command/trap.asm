

trap
; jsr errdir ;why not????      [910925]
                jsr chrgot                              ; if no #, means 'turn off trap'
                beq l92_1
                jsr getwrd
                sty trapno
                !text $2c

l92_1           lda #$ff                                ; flag no trap
                sta trapno+1
                rts

;.end