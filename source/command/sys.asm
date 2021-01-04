


sys             jsr getwrd                              ; convert arg to integer value
                lda linnum                              ; set up arg's for call to 'long jsr'
                sta _pclo
                lda linnum+1
                sta _pchi
                lda current_bank
                sta _bank

                jsr optbyt                              ; (optional) .A reg arg
                bcc l63_1
                stx _a_reg

l63_1           jsr optbyt                              ; (optional) .X reg arg
                bcc l63_2
                stx _x_reg

l63_2           jsr optbyt                              ; (optional) .Y reg arg
                bcc l63_4
                stx _y_reg

l63_3           jsr optbyt                              ; (optional) .Z reg arg
                bcc l63_4
                stx _z_reg

l63_4           jsr optbyt                              ; (optional) .S reg arg
                bcc l63_5
                stx _s_reg

l63_5           jmp _jsr_far                            ; far, far away
;If returns, Kernel will update _reg's for us

;.end