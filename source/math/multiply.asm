
; Multiplication        FAC = ARG*FAC

fmultt_c65                                              ; [910402]
                lda argsgn
                eor facsgn
                sta arisgn                              ; resultant sign
                ldx facexp                              ; set signs on thing to multiply
                bra fmultt                              ; go multiply

fmult           jsr conupk                              ; unpack the constant into arg for use

fmultt          beq multrt                              ; if FAC=0, return.  FAC is set
                jsr muldiv                              ; fix up the exponents
                lda #0                                  ; to clear result
                sta resho
                sta resmoh
                sta resmo
                sta reslo
                lda facov
                jsr mltpl1                              ; *** THIS fixes the DBL-0 bug without causing other grief!  C128-04 FAB
                lda faclo                               ; multiply arg by faclo
                jsr mltply
                lda facmo                               ; multiply arg by facmo
                jsr mltply
                lda facmoh
                jsr mltpl1                              ; *** THIS fixes the DBL-0 bug without causing other grief!  C128-04 FAB
                lda facho                               ; multiply arg by facho
                jsr mltpl1
                +lbra movfr                             ; move result into FAC


mltply          +lbeq mulshf                            ; normalize result and return. shift result right 1 byte.  exits with .c=0
mltpl1          lsr
                ora #$80                                ; will flag end of shifting

l171_1          tay
                bcc l171_2                              ; if mult bit=0, just shift
                clc
                lda reslo
                adc arglo
                sta reslo
                lda resmo
                adc argmo
                sta resmo
                lda resmoh
                adc argmoh
                sta resmoh
                lda resho
                adc argho
                sta resho

l171_2          ror resho
                ror resmoh
                ror resmo
                ror reslo
                ror facov                               ; save for rounding
                tya
                lsr                                     ; clear msb so we get a closer to 0
                bne l171_1                              ; slow as a turtle

multrt          rts

