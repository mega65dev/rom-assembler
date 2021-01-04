; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      convert.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



inprt           jsr     _primm
                !text " IN ",0

curprt          lda     curlin+1
                ldx     curlin

linprt          sta     facho
                stx     facho+1
                ldx     #$90                            ; exponent of 16
                sec                                     ; number is positive
                jsr     floatc
                jsr     foutc
                +lbra   strout                          ; print and return


fout            ldy     #1
foutc           lda     #' '                            ; if positive, print space
                bbr7    facsgn,l181_1
                lda     #'-'                            ; if neg
l181_1          sta     fbuffr-1,y                      ; store the character
                sta     facsgn                          ; make FAC pos for QINT
                sty     fbufpt                          ; save for later
                iny
                lda     #'0'                            ; get zero to type if FAC=0
                ldx     facexp
                +lbeq   fout19

                lda     #0
                cpx     #$80                            ; is number < 1?
                beq     l181_2                          ; no
                bcs     l181_3

l181_2          lda     #<nmil                          ; mult by 10~6
                ldy     #>nmil
                jsr     rommlt
                lda     #$f7
l181_3          sta     deccnt                          ; save count or zero it

l181_4          lda     #<n9999
                ldy     #>n9999
                jsr     fcomp                           ; is number > 999999.499 or 999999999.5?
                beq     l181_9                          ; go to biggies
                bpl     l181_7                          ; yes, make it smaller

l181_5          lda     #<n0999
                ldy     #>n0999
                jsr     fcomp                           ; is number > 99999.9499 or 99999999.90625?
                beq     l181_6
                bpl     l181_8                          ; yes. done multiplying

l181_6          jsr     mul10                           ; make it bigger
                dec     deccnt
                bne     l181_5                          ; see if that does it (this always goes)

l181_7          jsr     div10                           ; make it smaller
                inc     deccnt
                bne     l181_4                          ; see if that does it (this always goes)

l181_8          jsr     faddh                           ; add a half to round up


l181_9          jsr     qint                            ; biggies.
                ldx     #1                              ; decimal point count
                lda     deccnt
                clc
                adc     #$0a                            ; should number be printed in E notation?  (ie, is number .lt. .01?)
                bmi     l181_10                         ; yes
                cmp     #$0b                            ; is it > 999999 or 9999999999?
                bcs     l181_11                         ; yes, use E notation
                adc     #$ff                            ; number of places before decimal point
                tax                                     ; put into accx
                lda     #2                              ; no E notation
l181_10         sec

l181_11         sbc     #2                              ; effectively add 5 to orig exp
                sta     tenexp                          ; that is the exponent to print
                stx     deccnt                          ; number of decimal places
                txa
                beq     l181_12
                bpl     l181_14                         ; some places before dec pnt

l181_12         ldy     fbufpt                          ; get pointer to output
                lda     #'.'                            ; put in "."
                iny
                sta     fbuffr-1,y
                txa
                beq     l181_13
                lda     #'0'                            ; get the ensuing zero
                iny
                sta     fbuffr-1,y

l181_13         sty     fbufpt                          ; save it for later

l181_14         ldy     #0

foutim          ldx     #$80                            ; first pass through, accb has msb set
fout2           lda     faclo
                clc
                adc     foutbl+3,y
                sta     faclo
                lda     facmo
                adc     foutbl+2,y
                sta     facmo
                lda     facmoh
                adc     foutbl+1,y
                sta     facmoh
                lda     facho
                adc     foutbl,y
                sta     facho
                inx                                     ; it was done yet another time
                bcs     l182_1
                bpl     fout2
                bmi     l182_2

l182_1          bmi     fout2
l182_2          txa
                bcc     l182_3                          ; can use (a) as is
                eor     #$ff                            ; find 11.(a)
                adc     #10                             ; c is still on to complete negation, and will always be on after

l182_3          adc     #'0'-1                          ; get a character to print
                iny
                iny
                iny
                iny
                sty     fdecpt
                ldy     fbufpt
                iny                                     ; point to place to store output
                tax
                and     #$7f                            ; get rid of msb
                sta     fbuffr-1,y
                dec     deccnt
                bne     l182_4                          ; not time for dp yet
                lda     #'.'
                iny
                sta     fbuffr-1,y                      ; store dp

l182_4          sty     fbufpt                          ; store pointer for later
                ldy     fdecpt
                txa                                     ; complement accb
                eor     #$ff                            ; complement acca
                and     #$80                            ; save only msb
                tax
                cpy     #fdcend-foutbl
; beq l182_5  ;for time converter ????   removed [901014]
; cpy #timend-foutbl
                bne     fout2                           ; continue with output

l182_5          ldy     fbufpt                          ; get back output pointer
l182_6          lda     fbuffr-1,y                      ; remove trailing blanks
                dey
                cmp     #'0'
                beq     l182_6
                cmp     #'.'
                beq     l182_7                          ; ran into dp,  stop
                iny                                     ; something else, save it

l182_7          lda     #'+'
                ldx     tenexp
                beq     fout17                          ; no exponent to output
                bpl     l182_8
                lda     #0
                sec
                sbc     tenexp
                tax
                lda     #'-'                            ; exponent is negative

l182_8          sta     fbuffr+1,y                      ; store sign of exponent
                lda     #'E'
                sta     fbuffr,y                        ; store the 'E' character
                txa

                ldx     #'0'-1
                sec
l182_9          inx                                     ; move closer to output value
                sbc     #10                             ; subtract 10
                bcs     l182_9                          ; not negative yet

                adc     #'9'+1                          ; get second output character
                sta     fbuffr+3,y                      ; store high digit
                txa
                sta     fbuffr+2,y                      ; store low digit
                lda     #0                              ; put in terminator
                sta     fbuffr+4,y
                bra     fout20                          ; return


fout19          sta     fbuffr-1,y                      ; store the character
fout17          lda     #0                              ; store the terminator
                sta     fbuffr,y

fout20          lda     #<fbuffr
                ldy     #>fbuffr
                rts                                     ; all done


; Exponentiation and Square Root Functions.
;
; square root function - sqr(a)
; use sqr(x) = x^.5

sqr             jsr     movaf                           ; move FAC into ARG
                lda     #<fhalf
                ldy     #>fhalf

fpwr            jsr     movfm                           ; put memory into FAC    ARG^MEM


; Last thing fetched is facexp into accx.
;
; Exponentiation --- x^y.
; n.b. 0^0=1
; First check if y=0, and if so the result is one.
; Next  check if x=0, and if so the result is zero.
; Then  check if x>0:
; if not check that y is an integer.
; if so negate x, so that lg doesn't give fcerr.
; If x is negative and y is odd, negate the result returned by exp.
; To compute the result use x^y = EXP((y*LOG(x))


fpwrt           beq     exp                             ; if FAC=0, just exponentiate that  ARG^FAC
                lda     argexp                          ; is x=0?
                +lbeq   zerof1                          ; zero FAC

                ldx     #<tempf3                        ; save it for later in a temp
                ldy     #>tempf3
                jsr     movmf                           ; FAC->MEM

                lda     argsgn                          ; note y=0 already. that's good, in case no one calls int.
                bpl     l183_1                          ; no problems if x>0
                jsr     int                             ; integerize the FAC
                lda     #<tempf3                        ; get addr of comperand
                ldy     #>tempf3
                jsr     fcomp                           ; equal?
                bne     l183_1                          ; leave x neg. log will blow him out
;a=-1 and y is irrelavant
                tya                                     ; negative x. make positive
                ldy     integr                          ; get evenness

l183_1          jsr     movfa1                          ; alternate entry point.    ARG->FAC
                phy                                     ; save evenness for later
                jsr     log                             ; find log
                lda     #<tempf3                        ; multiply FAC times LOG(x)
                ldy     #>tempf3
                jsr     fmult
                jsr     exp                             ; exponentiate the FAC
                pla
                lsr                                     ; is it even?
                bcc     negrts                          ; yes. or x>0
;negate the number in FAC


negop                                                   ; /// entry point
                lda     facexp
                beq     negrts
                lda     facsgn
                eor     #$ff
                sta     facsgn
negrts          rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
