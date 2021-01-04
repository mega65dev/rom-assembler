; COLLECT v<drive#>

collect          jsr dospar                               ; parse the line
                 jsr chk3                                 ; check optional parameters
                 jsr _clall                               ; close all files
                 ldy #fcoll                               ; tabld offset
                 lda #1                                   ; length
                 bbr4 parsts,l230_1
                 inc                                      ; include drive
l230_1           jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any

