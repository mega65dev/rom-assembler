

errd             jsr sign                                 ; get sign
                 bmi l142_1                               ; (allow err$(er) when er=-1)
                 jsr conint                               ; get integer arg in x
                 dex
                 txa                                      ; error # (0 to max-1)
                 cmp #last_error_message                  ; check range
                 bcc l142_2                               ; ok
                 ldx #0                                   ; too high, return null
                 !text $2c

l142_1           ldx #2                                   ; no error, return "ok"    [910911]
                 lda #<ok_error_message
                 ldy #>ok_error_message
                 sta index2
                 sty index2+1
                 bra l142_5                               ; pass it

l142_2           jsr erstup                               ; look up the error, set up a pointer to it
                 ldy #$ff                                 ; determine how long it is
                 ldx #0
l142_3           inx                                      ; count printing characters
l142_4           iny
                 lda (index2),y                           ; (rom: ind.ok)
                 bmi l142_5                               ; msb set means last
                 cmp #' '
                 bcc l142_4                               ; don't count non-printers
                 bra l142_3                               ; count all others

l142_5           txa                                      ; message length
                 jsr strspa                               ; get space
                 tax
                 beq l142_7                               ; null

; sta sw_rom_ram1  ;set up string bank????
                 ldx #0
                 ldy #$ff
l142_6           iny                                      ; copy message into memory
                 lda (index2),y                           ; (rom: ind.ok)
                 cmp #' '
                 bcc l142_6                               ; skip non-printers

                 pha
                 and #$7f
                 phy                                      ; swap x&y
                 phx
                 ply
                 ldx #dsctmp+1
                 jsr sta_far_ram1                         ; sta (dsctmp+1),y to RAM1
                 phy                                      ; swap x&y
                 plx
                 ply
                 inx
                 pla                                      ; test if msb was set
                 bpl l142_6

l142_7           +lbra chrd1                              ; pla,pla,jmp putnew


;.end