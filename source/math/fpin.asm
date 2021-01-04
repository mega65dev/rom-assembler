; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      fpin.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


; Floating Point Input Routine.
;
; Number input is left in FAC.  At entry (TXTPTR) points to the first character
; in a text buffer.  The first character is also in (a).  FIN packs the digits
; into the FAC as an integer and keeps track of where the decimal point is.
; (DPTFLG) tells whether a dp has been seen.  (DECCNT) is the number of digits
; after the dp.  At the end (DECCNT) and the exponent are used to determine how
; many times to multiply or divide by ten to get the correct number.


fin             stx     fin_bank                        ; save bank number where string is stored

                ldy     #0                              ; zero facsgn, sgnflg
                ldx     #$0a                            ; zero exp and ho (and moh)
l177_1          sty     deccnt,x                        ; zero mo and lo
                dex                                     ; zero tenexp and expsgn
                bpl     l177_1                          ; zero deccnt, dptflg

                bcc     findgq                          ; flags still set from chrget
                cmp     #'-'                            ; a negative sign?
                bne     qplus                           ; no, try plus sign
                stx     sgnflg                          ; it's negative. (x=@377)
                bra     finc                            ; always branches


qplus           cmp     #'+'                            ; plus sign?
                bne     fin1                            ; yes, skip it

finc            jsr     fin_chrget

findgq          bcc     findig

fin1            cmp     #'.'                            ; the dp?
                beq     findp                           ; no kidding
                cmp     #'E'                            ; exponent follows
                bne     fine                            ; no

                jsr     fin_chrget                      ; yes, get another, to check sign of exponent
                bcc     fnedg1                          ; is it a digit. (easier than backing up pointer)
                cmp     #minus_token                    ; minus?
                beq     finec1                          ; negate
                cmp     #'-'                            ; minus sign?
                beq     finec1
                cmp     #plus_token                     ; plus?
                beq     finec
                cmp     #'+'                            ; plus sign?
                beq     finec
                bra     finec2

finec1          ror     expsgn                          ; turn it on

finec           jsr     fin_chrget                      ; get another

fnedg1          bcc     finedg                          ; it is a digit
finec2          bbr7    expsgn,fine
                lda     #0
                sec
                sbc     tenexp
                bra     fine1

findp           ror     dptflg
                bbr6    dptflg,finc

fine            lda     tenexp
fine1           sec
                sbc     deccnt                          ; get number of places to shift
                sta     tenexp
                beq     finqng                          ; negate?
                bpl     finmul                          ; positive, so multiply

findiv          jsr     div10
                inc     tenexp                          ; done?
                bne     findiv                          ; no
                bra     finqng                          ; yes


finmul          jsr     mul10
                dec     tenexp                          ; done?
                bne     finmul                          ; no
finqng          lda     sgnflg
                +lbmi   negop                           ; if negative, negate and return
                rts                                     ; if positive, return



findig          pha
                bbr7    dptflg,l178_1
                inc     deccnt
l178_1          jsr     mul10
                pla                                     ; get it back
                sec
                sbc     #'0'
                jsr     finlog                          ; add it in
                bra     finc



finlog          pha
                jsr     movaf                           ; save it for later
                pla
                jsr     float                           ; float the value in (a)

faddt_c65                                               ; [910402]
                lda     argsgn
                eor     facsgn
                sta     arisgn                          ; resultant sign
                ldx     facexp                          ; set signs on thing to add
                +lbra   faddt                           ; add together and return


; Pack in the next digit of the exponent.
; Multiply the old exp by 10 and add in the next digit.
; (note: does not check for exp overflow)

finedg          lda     tenexp                          ; get exp so far
                cmp     #10                             ; will result be >= 100?
                bcc     l179_1
                lda     #100
                bbs7    expsgn,l179_4                   ; if neg exp, no chk for overr
                +lbra   overr

l179_1          asl                                     ; max is 120
                asl                                     ; mult by 2 twice
                clc                                     ; possible shift out of high
                adc     tenexp                          ; like multiplying by five
                asl                                     ; and now by ten
                clc
                ldy     #0
                sta     syntmp

                lda     fin_bank                        ; text or string bank?
                bne     l179_2
                jsr     indtxt                          ; text
                bra     l179_3
l179_2          jsr     indin1_ram1                     ; string

l179_3          adc     syntmp
                sec
                sbc     #'0'
l179_4          sta     tenexp                          ; save result
                +lbra   finec


; Get a character from either text or string area, and set the flags
; in the manner performed by CHRGET.

fin_chrget
                lda     fin_bank                        ; text or string bank?
                +lbeq   chrget                          ; get byte from text bank via normal CHRGET mechanism

fin_chrget_1                                            ; get byte from string bank via modified CHRGET mechanism
                inw     index1
fin_chrget_2
                ldy     #0
                jsr     indin1_ram1
                cmp     #':'
                bcs     l180_1
                cmp     #' '
                beq     fin_chrget_1                    ; skip over spaces
                sec
                sbc     #'0'                            ; set up .c as CHRGET would
                sec
                sbc     #$d0
l180_1          rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
