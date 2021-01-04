; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      play.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; C65 Music Interpreter
;
; Syntax : PLAY "music_string"
;
; Where : music_string is a string of characters composed of:
;
; A..G   : notes
; W,H,Q,I,S : set note lengths to whole,half,quarter,eighth,sixteenth
; U   : set volume level   (0-9)
; O   : set octave    (0-6)
; T   : set current envelope  (0-9)
; V   : select voice to play  (1-6: 1-3 right, 4-6 left)
; X   : filter    (0-1: 0=off, 1=on)
; M   : measure
; R   : rest
; .   : dot
; #   : sharp
; $   : flat


play            jsr     frmstr                          ; frmevl,frestr,return w/ .A=len, (index)=>string
; sta sw_rom_ram0  ;????
                sta     z_p_temp_1                      ; save number of characters
                jsr     clear_play_flags                ; set 'dot' and 'sharp' to 0. return with Acc=0
                sta     hulp                            ; zero counter

l103_1          ldy     hulp
                cpy     z_p_temp_1
                beq     play_rts                        ; done!
                jsr     indin1_ram1
                jsr     play_one_character
                inc     hulp
                bne     l103_1                          ; always
play_rts
                rts


play_one_character
                cmp     #' '                            ; spaces are a 'no-op'
                beq     play_rts

l104_1          cmp     #'A'                            ; note name a-g?
                bcc     l104_2
                cmp     #'H'
                +lbcc   play_note                       ; yes...play it

l104_2          ldx     #4                              ; test for notes,'w,h,q,i,s'
l104_3          cmp     notes,x
                +lbeq   set_note_length
                dex
                bpl     l104_3

                cmp     #'R'                            ; rest?
                +lbeq   play_rest
                cmp     #'.'                            ; dotted note?
                +lbeq   play_dot

                ldx     #5                              ; test for v,o,t,x,u,m commands
l104_4          cmp     mutabl,x
                +lbeq   play_command
                dex
                bpl     l104_4                          ; test all 5 characters in table

                cmp     #'#'                            ; sharp?
                +lbeq   play_sharp
                cmp     #'$'                            ; flat?
                +lbeq   play_flat


; Must be a digit here for Octave, Voice, envelope (T), filter (X), or volume (U)

                sec
                sbc     #'0'                            ; mask nybble
                cmp     #10                             ; must be in range 0..9
                +lbcs   play_bad_value

                asl     flag                            ; octave, voice, envelope, filter, or volume?
                bcs     set_voice
                asl     flag                            ; octave, envelope, filter, or volume?
                bcs     set_octave
                asl     flag                            ; envelope, filter, or volume?
                bcs     set_envelope
                asl     flag                            ; filter or volume?
                bcc     set_volume

set_filter
                jsr     wait_for_all_quiet              ; [910722]
                cmp     #2
                +lbcs   play_bad_value                  ; value too large
                lsr                                     ; .c=on/off
                ldy     voice                           ; 0-5
                ldx     filter_offset,y                 ; 0 0 0 4 4 4
                lda     filters1+2,x                    ; get current filter data for this SID  [910612]
                ora     vbits,y                         ; update filter voice bit
                bcs     l105_1                          ; branch to turn filter on
                eor     vbits,y                         ; else, turn filter off   [910612]

l105_1          sta     filters1+2,x
; lda filters1+3,x ;why????     [910612]
; sta filters1+4,x ;save new filter-type/volume

; jsr put_io_in_map
                lda     SID_offset,y                    ; get hardware offset for current voice
                and     #$f0                            ; $00 or $20
                tay
; jsr go_slow  ;      [910716] 4567R7A
                ldz     #3
l105_2          lda     filters1,x                      ; update the hardware
                sta     sid1+21,y
                inx
                iny
                dez
                bpl     l105_2
; jsr go_fast  ;      [910716] 4567R7A
                bra     clear_flag                      ; always


set_voice
                dec
                cmp     #6                              ; stereo SIDs: 0-2=right, 3-5=left  [910612]
                +lbcs   play_bad_value
                sta     voice                           ; 0-5
                bra     clear_flag                      ; always


set_octave
                cmp     #7
                +lbcs   play_bad_value                  ; too big octave
                sta     octave                          ; set octave
                bra     clear_flag                      ; always


set_envelope
                jsr     wait_for_quiet                  ; [910626]
                tax
set_envelope_1                                          ; entry for initialization code
; jsr put_io_in_map
                ldy     voice
                lda     wavtab,x
                sta     waveform,y                      ; set waveform
                lda     SID_offset,y                    ; get hardware offset for this voice
                tay
; jsr go_slow  ;      [910716] 4567R7A
                lda     atktab,x
                sta     sid1+5,y                        ; set attack/decay
                lda     sustab,x
                sta     sid1+6,y                        ; set sustain/release
                lda     pulslw,x
                sta     sid1+2,y                        ; set pulse width - low byte
                lda     pulshi,x
                sta     sid1+3,y                        ; set pulse width - high byte
; jsr go_fast  ;      [910716] 4567R7A
                bra     clear_flag


set_volume
                jsr     wait_for_all_quiet              ; [910626]
                tax
                ldy     voice                           ; [910612]
                lda     filter_offset,y                 ; get filter offset for this voice
                tay
                lda     filters1+3,y                    ; get mode/volume for this SID
                and     #$f0                            ; mask out old volume
                ora     voltab,x                        ; add new volume
                sta     filters1+3,y                    ; save for filter change
; lda filters1+4,y ;get current filter-type/volume ????why  [910612]
; and #$f0
; ora voltab,x
                tax
                ldy     voice
                lda     SID_offset,y                    ; get hardware offset for current voice
                and     #$f0                            ; $00 or $20
                tay
; jsr go_slow  ;      [910716] 4567R7A
                stx     sid1+24,y                       ; set new volume
; jsr go_fast  ;      [910716] 4567R7A
;fall into clear_flag

clear_flag
                lda     #0
                sta     flag
                rts


go_fast
                lda     sid_speed_flag
                tsb     vic+49
                rts


go_slow
                pha
                lda     #$40
                and     vic+49
                trb     vic+49
                sta     sid_speed_flag
                pla
                rts


wait_for_quiet                                          ; Wait for current voice to be quiet  [910626]
                ldy     voice
                ldx     times2,y                        ; voice*2
l106_1          bit     voices+1,x                      ; test if voice is active   [910617]
                bpl     l106_1                          ; loop until inactive (IRQ)
                rts


wait_for_all_quiet                                        ; Wait for all voices on this SID to be quiet [910626]
                ldy     #3
                ldx     voice
                cpx     #3                              ; determine left/right SID
                bcs     l107_1
                ldy     #0
l107_1          ldz     #3                              ; for each of 3 voices
l107_2          ldx     times2,y
l107_3          bit     voices+1,x                      ; wait for voice to be inactive (IRQ)
                bpl     l107_3
                iny                                     ; next voice
                dez
                bne     l107_2                          ; until done 3 voices
                rts


play_bad_value
                jsr     clear_flag
                +lbra   fcerr                           ; illegal quantity

play_dot
                sta     dnote
                rts



set_note_length
; ldy #<beats  ;found note (.x), divide beats accordingly
; sty ntime
; ldy #>beats
; sty ntime+1

                bit     _pal_ntsc                       ; determine if PAL or NTSC system  [910724]
                bmi     l108_1                          ; ...branch if PAL
                ldz     #<beats_ntsc                    ; (whole note 4/4 time = 2 sec)
                ldy     #>beats_ntsc
                bra     l108_2
l108_1          ldz     #<beats_pal
                ldy     #>beats_pal
l108_2          stz     ntime
                sty     ntime+1

l108_3          dex
                bmi     l108_4                          ; finished dividing, exit
                lsr     ntime+1
                ror     ntime
                bra     l108_3

l108_4          rts


play_note
                sec
                sbc     #'A'
                tax
                lda     scalen,x                        ; note #0-11
                tax
                lda     #6
                sec
                sbc     octave
                tay
                txa
                clc
                adc     sharp
                bpl     l109_1                          ; added sharp or nat
                lda     #11                             ; underflow
                iny                                     ; bump octave down
l109_1          cmp     #12                             ; overflow?
                bcc     l109_2                          ; no...
                lda     #0
                dey                                     ; bump octave up
l109_2          tax
                lda     scalel,x
                sta     pitch

                bit     _pal_ntsc                       ; determine if PAL or NTSC system
                bmi     l109_3                          ; ...branch if PAL
                lda     scaleh,x                        ; continue as before patch
                bra     l109_4

l109_3          lda     scalelp,x                       ; load from PAL tables
                sta     pitch
                lda     scalehp,x

l109_4          dey
                bmi     play_note_1                     ; go play note
                lsr
                ror     pitch
                bra     l109_4


play_command
                cmp     #'M'                            ; measure?
                beq     l110_1

                lda     rbits,x                         ; all others, set flag for next number
                sta     flag
                rts

; Wait for msb of all 3 voice counters to underflow

;l110_1 ldy #5
;l110_2 lda voices,y
; bpl l110_2
; dey
; dey
; bpl l110_2
; rts

l110_1          ldy     #5                              ; [910626]
l110_2          ldx     times2,y
l110_3          bit     voices+1,x                      ; wait for voice to be inactive (IRQ)
                bpl     l110_3
                dey                                     ; next voice
                bpl     l110_2                          ; until done 6 voices
                rts



play_sharp
                lda     #1
                !text $2c
play_flat
                lda     #$ff
                sta     sharp
                rts


play_note_1                                             ; play a note
                sta     pitch+1
                lda     #0                              ; flag 'not rest'
                !text $2c                               ; hop
play_rest
                lda     #$ff                            ; flag 'rest'
                pha                                     ; save flag
                ldx     voice
                ldy     times2,x                        ; y=x*2
l111_1          lda     voices+1,y                      ; test if there is a note playing
                bpl     l111_1                          ; and loop if so

                sei
                lda     ntime                           ; load counter for current length
                sta     voices,y
                lda     ntime+1
                sta     voices+1,y
                lda     dnote                           ; test if this is a dotted note
                beq     l111_2                          ; no
                lda     ntime+1
                lsr                                     ; duration is 1.5 x current length
                pha
                lda     ntime
                ror
                clc
                adc     voices,y
                sta     voices,y
                pla
                adc     voices+1,y
                sta     voices+1,y

l111_2          pla                                     ; test if this is a rest
                bmi     l111_3                          ; and branch if so- clear play flags and exit [910722]

; jsr put_io_in_map
; jsr go_slow  ;      [910716] 4567R7A
                ldy     SID_offset,x                    ; get offset to voice hardware
                lda     pitch
                sta     sid1,y
                lda     pitch+1
                sta     sid1+1,y
                lda     #$08                            ; reset this voice
                sta     sid1+4,y
                lda     waveform,x                      ; and finally, turn on gate
                sta     sid1+4,y
; jsr go_fast  ;      [910716] 4567R7A
l111_3          cli


clear_play_flags
                lda     #0
                sta     sharp                           ; clear flags
                sta     dnote
                cli
                rts


tempo           jsr     getbyt                          ; duration of whole note 4/4 time = 24/rate
                txa
                +lbeq   fcerr                           ; can't be zero- illegal quantity error
                stx     tempo_rate
                rts


times2          !text 0,2,4,6,8,10                      ; [910612] stereo

notes           !text "WHQIS"                           ; sixteenth,eigth,quarter,half,and whole notes

mutabl          !text "VOTXUM"                          ; voice,octave,envelope,filter,volume,& measure

scalen          !text 9,11,0,2,4,5,7                    ; a,b,c,d,e,f,g

scalel          !text $0f,$0c,$46,$bf,$7d,$83           ; c,c#,d,d#,e,f,f#,g,g#,a,a#,b (NTSC, octave 6)
                !text $d6,$7a,$73,$c8,$7c,$97           ; [910729]

scaleh          !text $43,$47,$4b,$4f,$54,$59           ; c,c#,d,d#,e,f,f#,g,g#,a,a#,b (NTSC, octave 6)
                !text $5e,$64,$6a,$70,$77,$7e           ; [910729]

scalelp         !text $87,$8b,$cc,$4e,$14,$24           ; c,c#,d,d#,e,f,f#,g,g#,a,a#,b (PAL,  octave 6)
                !text $80,$2d,$32,$91,$52,$7a           ; [910729]

scalehp         !text $43,$47,$4b,$50,$55,$5a           ; c,c#,d,d#,e,f,f#,g,g#,a,a#,b (PAL,  octave 6)
                !text $5f,$65,$6b,$71,$78,$7f           ; [910729]

;  Music envelope tables, default values downloaded to RAM:
;
; 0: piano   1: accordion    2: calliope  3: drum     4: flute
; 5: guitar  6: harpsichord  7: organ     8: trumpet  9: xylophone

;  Attack/decay rates

atkmus          !text $09,$c0,$00,$05,$94,$09,$09,$09,$89,$09

;  Sustain/release rates

susmus          !text $00,$c0,$f0,$50,$40,$21,$00,$90,$41,$00

;  Waveform table

wavmus          !text $41,$21,$11,$81,$11,$21,$41,$41,$41,$11

;  Pulse width hi table

pwhmus          !text $06,$00,$00,$00,$00,$00,$02,$08,$02,$00

;  Offset tables

SID_offset
                !text $00,$07,$0e,$20,$27,$2e           ; [910612] stereo
filter_offset
                !text 0,0,0,4,4,4

;  Volume levels

voltab          !text 0,1,3,5,7,8,10,12,14,15

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
