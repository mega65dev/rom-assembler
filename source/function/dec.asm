; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      dec.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; DEC convert a hex string representing a 2-byte integer into decimal.

dcml            jsr     len1                            ; find length of string
                sta     index2                          ; len ret. in a
                ldy     #0
                sty     index2+1                        ; zero char counter
                sty     strng2+1                        ; zero out value
                sty     strng2

l139_1          cpy     index2                          ; evaluated all characters?
                beq     l139_4                          ; branch if so
                jsr     indin1_ram1                     ; get next character from string
                iny
                cmp     #' '                            ; ignore spaces
                beq     l139_1
                inc     index2+1
                ldx     index2+1
                cpx     #5
                bcs     decbad                          ; can't have more than 4 characters

                cmp     #'0'
                bcc     decbad                          ; bad if < 0
                cmp     #':'                            ; '9'+1
                bcc     l139_2                          ; ok if  = 0-9
                cmp     #'A'
                bcc     decbad                          ; bad if > 9  and < A
                cmp     #'G'
                bcs     decbad                          ; bad if > F

                sbc     #7                              ; adjust if A-F  (.c is clr)
l139_2          sbc     #$2f                            ; adjust to $00..$0f (.c is set)
                asl                                     ; shift low nibble to high
                asl
                asl
                asl

                ldx     #4                              ; mult. old val. by 16, add new
l139_3          asl
                rol     strng2
                rol     strng2+1
                dex
                bne     l139_3
                bra     l139_1

l139_4          ldy     strng2                          ; get lsb of value,
                lda     strng2+1                        ; & msb,
                +lbra   nosflt                          ; go float 2 byte unsigned integer


decbad
                +lbra   fcerr                           ; illegal qty error

;.end


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
