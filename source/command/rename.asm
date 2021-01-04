
; RENAME rdddfn=sdsfn

rename           lda #$e4                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk5
                 ldy #fren                                ; offset
                 lda #8                                   ; length
                 jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any



; BACKUP D<destination_drive>=D<source_drive>
;
; where destination|source_drive is [0...9]

backup           lda #$c7                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 and #$30                                 ; required parameters
                 cmp #$30
                 +lbne snerr
                 jsr are_you_sure
                 beq l232_1                               ; if run mode or not 'yes'
                 rts

l232_1           jsr dclall                               ; close disk
                 ldy #fbak
                 lda #4                                   ; length
                 jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any
