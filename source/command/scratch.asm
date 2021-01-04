
; SCRATCH sdfn  aliases: DELETE, ERASE

scratch         jsr dospar                              ; parse the line
                jsr chk1
                jsr are_you_sure                        ; confirm if in direct mode
                bne l228_4                              ; branch if 'no' response given

                ldy #fscr                               ; offset
                lda #4                                  ; length
                bit dosflags                            ; scratch or recover?
                bvc l228_1                              ; scratch
                ldy #frscr                              ; recover
                lda #6
l228_1          jsr trans                               ; transmit scratch command
                jsr Read_DS                             ; read error channel & update DS$

                bbs7 runmod,l228_4                      ; branch if not direct mode
                jsr crdo                                ; output cr

                ldy #0                                  ; display 'files scratched' DOS message
l228_2          lda #dsdesc+1
                jsr lda_far_ram1                        ; lda (dsdesc+1),y
                beq l228_3                              ; if end of error message
                jsr outch                               ; print it
                iny
                bpl l228_2                              ; always (bpl=failsafe)

l228_3          jsr crdo                                ; done

l228_4          rts

