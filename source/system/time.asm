; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      time.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************
; TI. Convert 24-hour TOD into tenths of seconds.  901010 F.Bowen

Get_TI
                jsr     ReadSystemClock                 ; glance at the clock, get time as h:m:s:t
                stz     faclo                           ; init accumulator with tenths (0-9, so nothing to convert)
                ldz     #0
                stz     facmo
                stz     facmoh

                ldx     #3                              ; convert time (BCD) to tenths of seconds (binary) since midnight
l134_1          jsr     TimeMultiply
                clc
                adc     faclo
                sta     faclo
                lda     product+1
                adc     facmo
                sta     facmo
                lda     product+2
                adc     facmoh
                sta     facmoh                          ; (can't overflow since 23:59:59:9 -> 863999 ($0D2EFF)
                dex
                bne     l134_1                          ; next factor

                lda     #0                              ; float value in FAC
                sta     facho                           ; zero msb, facov, facsgn
                ldx     #160                            ; set facov for time
                sec                                     ; normal fac
                +lbra   floatb                          ; do it



ReadSystemClock
                jsr     _ReadTime                       ; get packed BCD, y=hrs, x=min, a=sec, z=tenths
; (assumes system clock was set properly!)
                stz     time                            ; tenths  0-9
                sta     time+1                          ; seconds  0-59
                stx     time+2                          ; minutes  0-59
                sty     time+3                          ; hours  0-59
                rts


; Unsigned Integer Multiply: Time * Factor  -> Tenths_of_Seconds
;     A   *  (B,C)  ->      (D,E,F)

TimeMultiply
                lda     time,x                          ; convert packed BCD to binary
                and     #$0f
                sta     facho
                lda     time,x                          ; 10x = 8x + 2x
                and     #$f0
                lsr                                     ; msd x 8
                sta     time,x
                lsr
                lsr                                     ; msd x 2
                clc
                adc     facho                           ; lsd
                adc     time,x
                sta     time,x                          ; can't overflow ($99->153)

                txa                                     ; make a word pointer from byte pointer
                asl
                tay
                lda     TimeFactor-2,y                  ; multiplicand = TimeFactor,y  (2 bytes)
                sta     multiplicand                    ; multiplier = Time,x x (1 byte)
                lda     TimeFactor-1,y                  ; -----------
                sta     multiplicand+1
                lda     #0                              ; product lo   (3 bytes)
                sta     product+1                       ; mid
                sta     product+2                       ; hi

                ldy     #16                             ; 16-bit multiplicand
l135_1          asl
                row     product+1
                row     multiplicand                    ; multiplier * multiplicand -> product
                bcc     l135_2
                clc
                adc     time,x
                bcc     l135_2
                inw     product+1                       ; (does no error check, since using time factors
l135_2          dey                                     ; in ROM and maximum time multiplier of 59 there
                bne     l135_1                          ; is no danger of overflow)

; sta product
                rts                                     ; (.X is preserved)


TimeFactor
                !word 10                                ; tenths per second  (max    59*10 =    590 ($0024E)
                !word 600                               ; per minute  (max   59*600 =  35400 ($08A48)
                !word 36000                             ; per hour    (max 23*36000 = 828000 ($CA260)


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
