


; TI$="hh:mm:ss.t" Allows optional colons to delimit parameters and
;   allows input to be abbrieviated (eg., TI$="h:mm" or
;   even TI$=""), defaulting to "00" for unspecified
;   parameters.  24-hour clock (00:00:00.0 to 23:59:59.9).
;   901010 F.Bowen

Set_TI_String
                jsr frefac                              ; we won't need it
                sta count                               ; save length

                ldy #0                                  ; our pointer into TI$ assignment
                sty time                                ; default time to zero, in case it's not fully specified
                sty time+1
                sty time+2
                sty time+3

                ldx #3                                  ; parameter pointer (3=hr,2=min,1=sec,0=tenths)
l131_1          jsr GetTimeDigit                        ; get first digit, convert to BCD
                bcs l131_2                              ; colon or eos
                sta time,x
                jsr GetTimeDigit                        ; get second digit, convert to BCD
                bcs l131_2                              ; colon or eos

                asl time,x                              ; move first digit to msd
                asl time,x
                asl time,x
                asl time,x
                ora time,x                              ; combine with second digit
                sta time,x                              ; now we have a time element of packed BCD

l131_2          lda time,x
                cmp MaxTimeValues,x                     ; check for parameter too big
                +lbcs fcerr                             ; hr>23, min>59, sec>59, tenths>9

                dex                                     ; check if done
                bmi l131_4                              ; yes- all parameters accounted for
                cpy count
                bcs l131_5                              ; yes- end of string

                jsr indin1_ram1                         ; check for optional colon (or period)   [910103]
                cmp #':'
                beq l131_3
                cmp #'.'
                bne l131_1                              ; not there
l131_3          iny                                     ; it's there- skip over it

                bra l131_1                              ; loop until done


l131_4          cpy count                               ; done
                +lbcc errlen                            ; error if string too long

l131_5          ldz time                                ; tenths  0-9
                lda time+1                              ; seconds 0-59
                ldx time+2                              ; minutes 0-59
                ldy time+3                              ; hours  0-23
                jmp _SetTime                            ; Go set time & exit


; Get an ASCII digit, make sure it's in range 0-9 or a colon.
; if no digit to get, default to '0'
;
; exit with .c=0 if okay  (.A contains BCD)
;    .c=1 if colon or eos (.A invalid)

GetTimeDigit
                lda #0                                  ; default to '0'
                cpy count
                bcs l132_1                              ; exit if at end of string (carry set)

                jsr indin1_ram1                         ; else get a character from string
                iny                                     ; point to next character
                cmp #'.'                                ; [910103]
                beq l132_1                              ; terminator (period) (carry set)
                cmp #'0'                                ; check character, only 0-9 allowed
                +lbcc fcerr                             ; too small
                cmp #':'
                bcc l132_1                              ; just right  (carry clear)
                +lbne fcerr                             ; too big
; falls through if colon (carry set)

l132_1          and #$0f                                ; make BCD
                rts



MaxTimeValues
                !text $10,$60,$60,$24                   ; t,s,m,h in packed BCD


; x$=TI$  Return a string of the form "hh:mm:ss.t", including colons.

Get_TI_String
                jsr ReadSystemClock                     ; get time as packed BCD

                lda #10                                 ; get string space for 10 characters
                jsr strspa
                tay                                     ; length
                dey                                     ; index

                lda time                                ; build TI$ string in 'fbuffr'
                ora #'0'                                ; (build string backwards, from last chr to first)
                ldx #dsctmp+1
                jsr sta_far_ram1                        ; put tenths (special case- only 1 digit)
                dey
                lda #'.'
                jsr sta_far_ram1                        ; put period (special case)   [910103]
                dey
                ldx #1
                bra l133_2

l133_1          phx                                     ; element pointer (1=secs, 2=mins, 3=hrs)
                ldx #dsctmp+1
                lda #':'
                jsr sta_far_ram1                        ; put colon
                dey
                plx

l133_2          lda time,x
                taz
                and #$0f                                ; do lsd first, since we're working backwards
                ora #'0'
                phx
                ldx #dsctmp+1
                jsr sta_far_ram1                        ; put lsd
                dey
                tza                                     ; then do msd
                lsr
                lsr
                lsr
                lsr
                ora #'0'
                jsr sta_far_ram1                        ; put msd
                plx
                inx                                     ; next packed element
                dey
                bpl l133_1                              ; loop until done

                lda #10                                 ; length
                jsr mvdone                              ; update frespc ????
                +lbra putnew                            ; make descriptor in dsctmp real

