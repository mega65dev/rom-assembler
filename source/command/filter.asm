


;******************************************************************
;
;  FILTER  sid, freq, lp, bp, hp, res   --  set values for filter
;
;     sid =  right (1), left (2)
;          freq =  filter frequency (0-1023)
;            lp =  low pass filter on (1) or off (0)
;            bp =  band pass filter on (1) or off (0)
;            hp =  high pass filter on (1) or off (0)
;           res =  resonance (0-15)
;
;******************************************************************

filter          jsr getbyt                              ; get left/right SID    [910612]
                dex
                cpx #2
                +lbcs fcerr
                lda filter_offset+2,x                   ; get filter offset for specified SID
                sta z_p_temp_1
                tax

                ldy #0
l112_1          lda filters1,x                          ; save current voice's filter params
                sta fltsav,y
                inx
                iny
                cpy #4
                bcc l112_1

                jsr optwrd                              ; get filter frequency
                bcc l112_2                              ; skip if no value given
                cmp #8                                  ; test m.s. byte
                +lbcs fcerr                             ; error if > 2047
                sty fltsav                              ; save lower byte

; Idea: shift lower 3 bits of upper byte into lower byte, forming bits 10-3

                sty fltsav+1
                lsr
                ror fltsav+1
                lsr
                ror fltsav+1                            ; save upper 7 bits (10-3)
                lsr
                ror fltsav+1

l112_2          lda #$10                                ; start at type=LP
                sta fltflg
                lda fltsav

l112_3          jsr optbyt                              ; get filter types (LP,BP,HP)
                bcc l112_6                              ; skip if no value input
                cpx #1                                  ; (set .c: 0=0, 1=1)
                bcc l112_4
                beq l112_4
                +lbra fcerr                             ; error if >1

l112_4          lda fltsav+3                            ; get filter flags byte
                ora fltflg                              ; set filter on
                bcs l112_5                              ; skip if it should be on
                eor fltflg                              ; turn filter off
l112_5          sta fltsav+3                            ; save value

l112_6          asl fltflg                              ; shift for next filter
                bpl l112_3                              ; loop 3 times

                jsr optbyt                              ; get resonance value
                bcc l112_7                              ; skip if no value given
; cpx #16
; bcs fcerr  ;error if >15
                jsr chknyb                              ; [910930]
                txa
                asl                                     ; shift to upper nibble
                asl
                asl
                asl
                sta nibble
                lda fltsav+2                            ; get current value
                and #$0f                                ; mask it out
                ora nibble                              ; add new value
                sta fltsav+2                            ; save it

l112_7          ldx z_p_temp_1                          ; hardware offset for this voice's filter [910612]
                ldy #0
l112_8          lda fltsav,y                            ; copy new filter params to hardware
                sta filters1,x
                inx
                iny
                cpy #4
                bcc l112_8
                rts

;.end
