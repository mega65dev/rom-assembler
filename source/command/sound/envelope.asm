; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      envelope.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;****************************************************************
;
;  ENVELOPE n, attack, decay, sustain, release, waveform, pulse width
;        set music envelope
;                n = envelope number (0-9)
;            wave  =   0 : triangle waveform
;                      1 : sawtooth waveform
;                      2 : pulse waveform
;                      3 : noise waveform
;                      4 : ring modulation
;            pulse = pulse width if pulse waveform is selected (0-4095)
;
;******************************************************************

envelope
                jsr     getbyt                          ; get envelope number
                cpx     #10
                +lbcs   fcerr                           ; exit - invalid tone number
                stx     tonnum                          ; save number
                lda     atktab,x                        ; get attack/decay rates
                sta     tonval
                lda     sustab,x                        ; get sustain/release rates
                sta     tonval+1
                lda     wavtab,x                        ; get waveform and filter
                sta     tonval+2

                ldx     #0
l113_1          stx     parcnt
                jsr     optbyt                          ; get parameter - attack or sustain
                bcc     l113_2                          ; skip if no input
                txa
                asl
                asl                                     ; shift to upper nibble
                asl
                asl
                sta     nibble                          ; save it
                ldx     parcnt
                lda     tonval,x                        ; get current value
                and     #$0f                            ; mask it out
                ora     nibble                          ; add new value
                sta     tonval,x                        ; save it

l113_2          jsr     optbyt                          ; get decay or release rate
                bcc     l113_3                          ; skip if no input
                txa
                and     #$0f                            ; use only lower nibble
                sta     nibble                          ; save it
                ldx     parcnt
                lda     tonval,x                        ; get current value
                and     #$f0                            ; mask it out
                ora     nibble                          ; add new value
                sta     tonval,x                        ; save it

l113_3          ldx     parcnt
                inx
                cpx     #1
                beq     l113_1                          ; loop to do sustain/release rates
                jsr     optbyt                          ; get waveform
                bcc     l113_5                          ; skip if no value
                lda     #$15                            ; assume ring modulation
                cpx     #4
                beq     l113_4                          ; skip if correct
                +lbcs   fcerr                           ; error if >4
                lda     sbits+4,x                       ; get waveform bit
                ora     #1                              ; set gate bit

l113_4          sta     tonval+2                        ; save waveform

l113_5          jsr     optwrd                          ; is there a pulse width arg?
                bcc     l113_6                          ; nope, done

                tax                                     ; save msb
                lda     tonval+2                        ; get waveform
                and     #$40
                beq     l113_6                          ; skip if not pulse waveform
                txa
                ldx     tonnum                          ; get envelope number
                sta     pulshi,x                        ; save high byte of pulse width
                tya
                sta     pulslw,x                        ; save low byte

l113_6          ldx     tonnum
                lda     tonval                          ; set inputted values
                sta     atktab,x
                lda     tonval+1
                sta     sustab,x
                lda     tonval+2
                sta     wavtab,x
volrts
                rts

;.end


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
