
; C65 BASIC Indirect Load Subroutines


inddef
                lda #defpnt
                bra lda_far_ram1

indfrm
                lda #form
                bra lda_far_ram1

inddpt
                lda #dscpnt
                bra lda_far_ram1

;indhtr
; lda #hightr
; bra lda_far_ram0

indhtr_ram1
                lda #hightr
                bra lda_far_ram1

indfmo
                lda #facmo
                bra lda_far_ram1

indlow
                lda #lowtr
                bra lda_far_ram0

indst1
                lda #strng1
                bra lda_far_ram0

indst1_ram1
                lda #strng1
                bra lda_far_ram1

indgrb
                lda #grbpnt
                bra lda_far_ram1

indlow_ram1
                lda #lowtr
                bra lda_far_ram1

indin1
                lda #index1
                bra lda_far_ram0

;indin2
; lda #index2
; bra lda_far_ram0

indtxt
                lda #txtptr
; bra lda_far_ram0


; C65 BASIC Indirect Load Subroutines

lda_far_ram0
                phz                                     ; save registers
                phx
                tax                                     ; pointer
                ldz text_bank                           ; RAM0
                jsr _lda_far                            ; LDA (.x),Y from bank .z
                plx
                plz
                and #$ff                                ; set processor status per byte fetched
                rts



indin1_ram1
                lda #index1
; bra lda_far_ram1

lda_far_ram1
                php                                     ; save .c
                phz                                     ; save registers
                phx
                tax                                     ; pointer
                ldz var_bank                            ; RAM1
                lda 1,x                                 ; check to see if pointer points to "common"
                cmp #$20
                bcs l6_1                                ; branch if not
                ldz text_bank                           ; else select RAM0

l6_1            jsr _lda_far                            ; LDA (.x),Y from bank .z
                plx
                plz
                plp                                     ; restore .c
                and #$ff                                ; set processor status per byte fetched
                rts


; C65 BASIC Indirect Save Subroutines

sta_far_ram1
                php                                     ; save registers
                phz
                pha
                ldz var_bank                            ; RAM1
                lda 1,x                                 ; check to see if pointer points to "common"
                cmp #$20
                bcs l7_1                                ; branch if not
                ldz text_bank                           ; else select RAM0

l7_1            pla
                jsr _sta_far                            ; STA (.x),Y to bank .z
                plz
                plp
                rts


sta_far_in1                                             ; [910624]
                ldx #index1
                bra sta_far_ram0

sta_far_txt
                ldx #txtptr

sta_far_ram0
                php                                     ; save registers
                phz
                ldz text_bank                           ; RAM0
                jsr _sta_far                            ; STA (.x),Y to bank .z
                plz
                plp
                rts


indcmp_in1                                              ; [910620]
                ldx #index1
                ldz text_bank                           ; RAM0
                jmp _cmp_far                            ; STA (.x),Y to bank .z

;.end


