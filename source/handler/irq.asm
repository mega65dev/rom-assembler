


basic_irq
; lda _vicIRQ  ;a VIC raster interrupt?
; and #%10000001  ; (used to update moving sprites & sound stuff)
; cmp #%10000001
; bne collision_irq ; no, go check other VIC interrupts

                lda irq_wrap_flag                       ; filter out wrapped IRQ calls (allows interruptable code)
                beq l251_1                              ; it's ok
                rts                                     ; exit- we're already handling one interrupt

l251_1          inc irq_wrap_flag                       ; shut the door
                cli                                     ; but leave the window open


; Test if there was a VIC collision/light pen interrupt

collision_irq
; sei
                lda _vicIRQ                             ; check VIC IRQ flags
                and #%00001110                          ; mask all but lp, s/s, and s/bgnd flags
                beq l252_5                              ; exit if none set
                trb _vicIRQ                             ; else reset flags we're going to handle
                lsr                                     ; shift out raster interrupt bit (not used)

; Test for 3 types of collision interrupts : sprite/sprite, sprite/bgnd, & light pen

                ldy #1                                  ; loop for sprite/bgnd and sprite/sprite collision check
l252_1          lsr
                bcc l252_4                              ; bit not set ==> not source of interrupt

                pha
                lda vic+30,y                            ; accumulate collision data (resets register)
                ora collisions,y
                sta collisions,y

                lda intval                              ; allowable interrupts
                cpy #0                                  ; examine selected bit
                beq l252_2
                lsr
l252_2          lsr
                bcc l252_3                              ; BASIC doesn't want this interrupt
                lda #$ff
                sta int_trip_flag,y                     ; turn on trip flag

l252_3          pla

l252_4          dey
                bpl l252_1


; Check light pen latch

                lsr
                bcc l252_5                              ; LightPen latch not valid

                ldx vic+49                              ; 4567R7 bug- must read LP_latches in Slow mode????
                lda #%01000000                          ; [910618]
                trb vic+49
                ldy vic+19                              ; save latched x position
                sty lightpen_xpos
                ldy vic+20                              ; save latched y position
                sty lightpen_ypos
                stx vic+49                              ; restore speed     [910618]

                lda intval                              ; is BASIC interested in our little find?
                and #4
                beq l252_5                              ; no, move on to next IRQ task
                lda #$ff
                sta int_trip_flag+2                     ; yes- let BASIC know we caught one

l252_5

; Update moving sprites

movspr_irq
                lda vic+21                              ; any sprites active?    [910212]
                +lbeq music_irq                         ; no- skip ahead

                ldy #7                                  ; check each of 8 sprites
l253_1          lda vic+21                              ; is this sprite is enabled?
                and sbits,y
                beq l253_5                              ; sprite not enabled

                ldx sproff,y                            ; get offset to sprite info from a table
                lda sprite_data,x                       ; is this sprite moving (speed >0 )?
                beq l253_5                              ; sprite not moving
                bpl l253_2                              ; sprite moving, no destination
                bsr movspr_to_irq                       ; sprite moving to a destination [910809]
                bra l253_5

l253_2          sta sprite_data+1,x                     ; set counter
l253_3          tya                                     ; convert sprite# to a VIC register pointer
                asl
                tay
                lda sprite_data+2,x                     ; get angle sign
                dec                                     ; subtract 1 for cosine
                inx
                inx
                iny
                jsr sprsub                              ; update y position
                dex
                dex
                dey
                lda sprite_data+2,x
                jsr sprsub                              ; update x position
                php
                tya
                lsr                                     ; restore index (.Y=sprite pointer)
                tay
                plp
                bcc l253_4                              ; skip if no overflow
                lda vic+16                              ; get x position msb bits ???vic_save
                eor sbits,y                             ; invert bit
                sta vic+16                              ; ???vic_save
l253_4          dec sprite_data+1,x
                bne l253_3                              ; loop until counter done

l253_5          dey                                     ; check next sprite
                bpl l253_1                              ; loop until done moving all sprites
                +lbra music_irq                         ; then continue with next IRQ task

movspr_to_irq                                           ; [910809]
                phy                                     ; sprite #
                and #$3f                                ; speed factor
                taz
                tya                                     ; vic sprite index
                asl
                tay

l254_1          sec                                     ; for i = 1 to abs(greatr)
                lda sprite_data+1,x
                sbc #1
                sta sprite_data+1,x
                bcs l254_2
                lda sprite_data+2,x
                sbc #0
                sta sprite_data+2,x
                bcs l254_2
                lda #0
                sta sprite_data,x                       ; done!  sprite is at its destination
                ply                                     ; remember sprite #
                rts

l254_2          lda sprite_data+3,x                     ; ptr(lesser)
                bit sprite_data+10,x
                bmi l254_3                              ; if e > 0
                bit sprite_data+3,x                     ; sgn(lesser) (b7=1=neg, b6=1=pos, else 0)
                jsr drwinc                              ; pos(lesser) = pos(lesser) + sgn(lesser)

                lda sprite_data+4,x                     ; ptr(greater)
l254_3          lsr                                     ; which f?
                bcs l254_4
                lda sprite_data+9,x                     ; e = e + f1
                adc sprite_data+5,x
                sta sprite_data+9,x
                lda sprite_data+10,x
                adc sprite_data+6,x
                sta sprite_data+10,x
                bra l254_5

l254_4          clc
                lda sprite_data+9,x                     ; e = e + f2
                adc sprite_data+7,x
                sta sprite_data+9,x
                lda sprite_data+10,x
                adc sprite_data+8,x
                sta sprite_data+10,x

l254_5          lda sprite_data+4,x                     ; ptr(greater)
                bit sprite_data+4,x                     ; sgn(greater) (b7=1=neg, b6=1=pos, else 0)
                jsr drwinc                              ; pos(greater) = pos(greater) + sgn(greater)

                dez                                     ; count
                bne l254_1
                ply                                     ; remember sprite #
                rts                                     ; done this frame


drwinc          php
                and #1                                  ; adjust .y for x or y position
                beq l255_1                              ; 0=x
                iny                                     ; 1=y
l255_1          plp
                bmi l255_2                              ; enter with b7=negative, b6=positive, else zero
                bvc l255_4

                lda vic,y                               ; positive direction
                inc
                sta vic,y
                bra l255_3

l255_2          lda vic,y                               ; negative direction
                dec
                sta vic,y
                cmp #$ff

l255_3          bne l255_4                              ; no wrap
                tya
                bit #1
                bne l255_4                              ; wrap in y okay
                lsr
                tay
                lda sbits,y                             ; wrap in x- toggle msb
                eor vic+16
                sta vic+16
                tya
                asl
                tay

l255_4          tya                                     ; restore y to sprite offset
                and #$fe
                tay
                rts


; Play music, if in progress

music_irq
                ldx #0
l256_1          ldy voices+1,x
                bmi l256_2                              ; skip if not active

                lda voices,x
                sec
                sbc tempo_rate                          ; decrement current value by current tempo
                sta voices,x
                bcs l256_2
                tya                                     ; lda voices+1,x
                sbc #0
                sta voices+1,x
                bcs l256_2                              ; ok, no underflow

                txa
                lsr                                     ; get offset to waveform
                tay
                lda waveform,y                          ; get waveform
                and #$fe                                ; mask out gate bit
                pha
                lda SID_offset,y                        ; get offset to correct oscillator
                tay
                pla
; jsr go_slow  ;      [910716] 4567R7A
                sta sid1+4,y                            ; turn off sound
; jsr go_fast  ;      [910716] 4567R7A

l256_2          inx
                inx
                cpx #6+6                                ; [910612]
                bcc l256_1                              ; loop for 6 voices
;then continue with next IRQ task

; Test if SOUND command wants anything

sound_irq
                ldy #6-1                                ; test six voices    [910612]
l257_1          lda sound_time_hi,y                     ; active if msb clear
                bpl l257_3
l257_2          dey
                bpl l257_1
                +lbra basic_irq_end

l257_3          clc                                     ; add step to frequency
                lda sound_freq_lo,y
                adc sound_step_lo,y
                sta sound_freq_lo,y
                lda sound_freq_hi,y
                adc sound_step_hi,y
                sta sound_freq_hi,y

                lda sound_direction,y                   ; test if this is up or down
                tax
                and #1
                beq l257_6                              ; branch if up

; If step direction is down, .C==0 OR freq < min  ==> reset value

                bcc l257_4                              ; underflow, reset
                sec
                lda sound_freq_lo,y
                sbc sound_min_lo,y
                lda sound_freq_hi,y
                sbc sound_min_hi,y
                bcs l257_9                              ; no borrow, don't reset

l257_4          cpx #2                                  ; is 'cycle' bit set?
                bcc l257_5                              ; no, keep direction 'down'

                jsr negate_step                         ; make step 2's comp
                lda #2                                  ; change direction to 'up'
                sta sound_direction,y
                bne l257_8                              ; go reset for 'up'

l257_5          lda sound_max_lo,y                      ; reset to max
                sta sound_freq_lo,y
                lda sound_max_hi,y
                sta sound_freq_hi,y
                bra l257_9                              ; go update SID frequency

; If step direction is up, overflow (.C==1) OR freq > max ==> reset frequency

l257_6          bcs l257_7                              ; overflow, must reset
                lda sound_freq_hi,y                     ; 16 bit compare (yech!)
                cmp sound_max_hi,y
                bcc l257_9                              ; freq < max, no reset
                bne l257_7                              ; freq > max, reset
                lda sound_freq_lo,y                     ; msb's the same, test lsb's
                cmp sound_max_lo,y
                bcc l257_9                              ; freq < max, no reset
                beq l257_9                              ; freq = max, no reset

l257_7          cpx #2                                  ; is this 'cycle'?
                bcc l257_8                              ; no, go reset for next 'up'

                jsr negate_step                         ; make step 2's comp
                lda #3                                  ; change direction to 'down'
                sta sound_direction,y
                bne l257_5                              ; go reset for next 'down'

l257_8          lda sound_min_lo,y                      ; set freq to minimum value
                sta sound_freq_lo,y
                lda sound_min_hi,y
                sta sound_freq_hi,y

; Update SID frequency registers

l257_9
; jsr go_slow  ;      [910716] 4567R7A
                ldx SID_offset,y                        ; get index to SID voices
                lda sound_freq_lo,y
                sta sid1,x
                lda sound_freq_hi,y
                sta sid1+1,x
; jsr go_fast  ;      [910716] 4567R7A

; Decrement total time - see if it's time to bring down the curtain

                tya
                tax
                lda sound_time_lo,x                     ; 16 bit decrement - not very pretty
                bne l257_10
                dec sound_time_hi,x
l257_10         dec sound_time_lo,x

                lda sound_time_hi,x                     ; underflow?
                +lbpl l257_2                            ; nope

; Time to turn off this voice

; jsr go_slow  ;      [910716] 4567R7A
                lda #$08
                ldx SID_offset,y
                sta sid1+4,x
; jsr go_fast  ;      [910716] 4567R7A
                +lbra l257_2


negate_step
                lda sound_step_lo,y
                eor #$ff
                clc
                adc #1
                sta sound_step_lo,y
                lda sound_step_hi,y
                eor #$ff
                adc #0
                sta sound_step_hi,y
                rts



; Here is where BASIC_IRQ exits

basic_irq_end
                dec irq_wrap_flag                       ; open the door to IRQ
                cli
                rts


; Update sprite position subroutine

sprsub          pha                                     ; save angle phase
                clc
                lda sprite_data+3,x                     ; add low bytes
                adc sprite_data+7,x
                sta sprite_data+7,x
                lda sprite_data+4,x                     ; add high bytes
                adc sprite_data+8,x
                sta sprite_data+8,x
                pla                                     ; get angle sign
                bcc l258_3                              ; skip if no carry - do not update position
                lsr
                lsr                                     ; test if positive or negative
                lda vic,y                               ; ???vic_save
                bcs l258_1                              ; skip if negative
                adc #1                                  ; increment position
                bra l258_2

l258_1          sbc #1                                  ; decrement position
                cmp #$ff                                ; set carry if underflow
l258_2          sta vic,y                               ; decrement position  ???vic_save
l258_3          rts

;.end