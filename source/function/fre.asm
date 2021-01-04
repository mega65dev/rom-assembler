


;*****************************************************************************
; FRE(n) Function
;
; Where: n=0 returns amount of free RAM in bank 0. This is the area
;  between top of text (TEXT_TOP) and top of RAM (MAX_MEM_0).
;
;  n=1 returns amount of free ram in bank 1. This is the area
;  between top of arrays (STREND) and bottom of strings (FRETOP).
;
;  n=2 returns the amount (???? presence) of expansion RAM.
;
;*****************************************************************************

fre             jsr conint                              ; get integer argument in .x
                cpx #1                                  ; which bank?
                beq l138_1                              ; go do bank one
                cpx #2                                  ; go do expansion banks   [910107]
                beq l138_2                              ; else it must be bank zero
                +lbcs fcerr                             ; any other is unpleasant to talk about

                sec                                     ; FRE(text_bank)
                lda max_mem_0
                sbc text_top
                tay                                     ; set up result for nosflt
                lda max_mem_0+1
                sbc text_top+1
                bra l138_3                              ; assumes text_top < max_mem


l138_1          jsr garba2                              ; FRE(var_bank) do garbage collect first
                sec
                lda fretop
                sbc strend
                tay
                lda fretop+1
                sbc strend+1
                bra l138_3

l138_2          ldy _expansion                          ; FRE(expansion banks)    [910107]
                lda #0

l138_3          +lbra nosflt                            ; go float the number (y,a)=(lo,hi)

;.end