


;*****************************************************************************
;*
;*  SOUND - Produce sound effects
;*
;* Syntax : SOUND v, f, d [,[dir] [,[m] [,[s] [,[w] [,p] ]]]]
;*
;* Where : v   = voice    (1..6)
;*  f   = frequency    (0..65535)
;*  d   = duration    (0..32767 jiffys)
;*  dir = step direction  (0(up) ,1(down) or 2(oscillate)) default=0
;*  m   = minimum frequency  (if sweep is used) (0..65535) default=0
;*  s   = step value for effects  (0..32767) default=0
;*  w   = waveform  (0=triangle,1=saw,2=square,3=noise) default=2
;*  p   = pulse width  (0..4095) default=2048 (50% duty cycle)
;*
;*****************************************************************************

sound           cmp #clr_token                          ; SOUND CLR: init sound/music environment [910717]
                +lbeq Sound_CLR                         ; yes

                jsr getbyt                              ; get voice number in .X
                dex                                     ; adjust 1..3 to 0..2
                cpx #6                                  ; [910612]
l115_1          +lbcs fcerr                             ; illegal value

l115_2          stx sound_voice

; Get frequency

                jsr comwrd                              ; eat comma, get frequency in y,a
                sty temp_max_lo                         ; save our copy of max, also set up as current
                sta temp_max_hi
                sty temp_freq_lo
                sta temp_freq_hi

; Get duration

                jsr comwrd                              ; eat comma, get number of jiffys to play
                cmp #$80
                bcs l115_1
                sty temp_time_lo
                sta temp_time_hi

; Get sweep direction

                jsr optzer                              ; get optional sweep (default = 0, up)
                cpx #3
                bcs l115_1
                txa
                sta temp_direction
                and #1                                  ; set .Z if sweep up or oscillate
                php                                     ; save .Z for step (below)

; Get minimum frequency value (sweep lo)

                jsr optwrd
                sty temp_min_lo
                sta temp_min_hi

; Get step value for sweep

                jsr optwrd                              ; get optional step, default is zero
                plp                                     ; get flags from direction
                beq l115_3                              ; branch if 'up' or oscillate
                pha                                     ; if 'down', make step 2's complement
                tya
                eor #$ff
                clc
                adc #1
                tay
                pla
                eor #$ff
                adc #0
l115_3          sta temp_step_hi
                tya
                sta temp_step_lo

; Get waveform

                ldx #2                                  ; get waveform. default is square (2)
                jsr optbyt
                cpx #4
                bcs l115_1                              ; illegal value
                lda sbits+4,x                           ; get bit pattern for selected waveform
                ora #1                                  ; add in the gate bit
                sta temp_waveform

; Get pulse width

                jsr optwrd                              ; get optional pulse width in y,a
                bcs l115_4
                lda #8                                  ; no arg's given, use default pulse width
                ldy #0
l115_4          cmp #16
                bcs l115_1
                sty temp_pulse_lo
                sta temp_pulse_hi

; All arg's in, time to get to work

                lda temp_time_lo
                ora temp_time_hi
                beq l115_9                              ; special case: time=0 means 'kill it NOW'

; Wait for all current uses of this voice to time out

                ldx sound_voice                         ; first test 'PLAY'
                txa                                     ; make an index into PLAY's tables
                asl
                tay
l115_5          lda voices+1,y
                bpl l115_5

l115_6          lda sound_time_hi,x                     ; now test 'SOUND'
                bpl l115_6

; All clear, now set up for current effect

                ldy #0                                  ; download max freq l&h, min freq l&h,
l115_7          lda temp_max_lo,y                       ; ..sweep direction, step value l&h, & freq l&h
                sta sound_max_lo,x
                inx
                inx
                inx
                inx                                     ; [910612] stereo
                inx
                inx
                iny
                cpy #9
                bcc l115_7

; Now set up SID

                ldx sound_voice
                ldy SID_offset,x                        ; get index to SID voices
; jsr put_io_in_map
; jsr go_slow  ;      [910716] 4567R7A

                lda #$08                                ; turn off SID gate
                sta sid1+4,y

                lda #0                                  ; set up attack & decay,
                sta sid1+5,y
                lda #$f0                                ; ..and sustain & release
                sta sid1+6,y

                ldx #0                                  ; set up freq (l&h), pulse width (l&h), & waveform
l115_8          lda temp_freq_lo,x
                sta sid1,y
                iny
                inx
                cpx #5
                bne l115_8
; jsr go_fast  ;      [910716] 4567R7A

; Now set up time to play

l115_9          ldx sound_voice
                ldy temp_time_lo
                lda temp_time_hi

                sei
                sta sound_time_hi,x
                tya
                sta sound_time_lo,x
                cli

                rts

;.end