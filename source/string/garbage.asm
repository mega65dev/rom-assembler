


;  Get space for a string, perhaps forcing garbage collection.
;
;  Entry:  a = # of chars
;  Exit:   (x,y) pointer to space, otherwise
;          blows off to 'out of string space' error
;          (also preserves .a and sets frespc= y,x = -> at space.)


getspa          lsr garbfl                              ; signal no garbage collection yet

tryag2          tax                                     ; save in x also
                beq getrts                              ; length of 0 no go...
                pha                                     ; save a (length) on stack
                lda fretop                              ; lo byte
                sec                                     ; for subtract
                sbc #2                                  ; minus 2 (link bytes)
                ldy fretop+1
                bcs l161_1
                dey
l161_1          sta index1                              ; save for later
                sty index1+1
                txa
                eor #$ff
                sec
                adc index1
                bcs l161_2
                dey
l161_2          cpy strend+1
                bcc garbag
                bne strfre
                cmp strend
                bcc garbag                              ; clean up


strfre          sta frespc
                sty frespc+1
                ldy #1                                  ; flag string as garbage
                lda #$ff
                phx                                     ; set up string bank
                ldx #index1
                jsr sta_far_ram1 ;sta (index1),y        ; flag
                plx
                dey
                pla                                     ; length
                phx                                     ; set up string bank
                ldx #index1
                jsr sta_far_ram1 ;sta (index1),y        ; length
                plx
                ldx frespc
                ldy frespc+1
                stx fretop
                sty fretop+1                            ; save new (fretop)
getrts          rts


garbag          lda garbfl
                +lbmi omerr                             ; if out of memory
                jsr garba2
                sec
                ror garbfl
                pla                                     ; get back string length
                bra tryag2                              ; always branches



; Routine looks for and squashes out any unused string space it finds, thus
; returning the space for future use by the string routines.  GARBA2 is called
; only when BASIC needs space or the FRE() function is used.


garba2          ldx temppt                              ; ptr to temp. strings
l162_1          cpx #tempst                             ; any out there?
                beq l162_2                              ; none
                jsr slr1                                ; setup ptr (tempf2) to temp. string's bkptr
                beq l162_1                              ; (skip if null string!)
                txa                                     ; .x = lsb of ptr to descriptor
                phx                                     ; set up string bank
                ldx #tempf2
                ldy #0
                jsr sta_far_ram1 ;(tempf2),y            ; place backpointer on string to temp. descr
                tya                                     ; .a = msb of ptr (0)
                iny
                jsr sta_far_ram1                        ; (tempf2),y
                plx
                bra l162_1                              ; always


l162_2          ldy #0                                  ; set up flag
                sty highds
                ldx max_mem_1
                ldy max_mem_1+1
                stx grbtop                              ; set both pointers
                stx grbpnt
                stx frespc
                sty grbtop+1
                sty grbpnt+1
                sty frespc+1
                txa


; do while (grbpnt <= fretop)

gloop           jsr chkgrb                              ; check garbage string
                bne l163_2                              ; if not garbage

l163_1          dey                                     ; back up to length
                jsr indgrb
                jsr movpnt                              ; move grbpnt to next
                sec
                ror highds                              ; indicate garbage string found
                bra gloop                               ; always

l163_2          bit highds
                bpl l163_6                              ; if garbage string not found
                ldx #0
                stx highds                              ; clear indicator

                lda #2                                  ; skip pointers past

; Move a string over garbage

l163_3          phx
                ldx #grbtop
                ldy #1                                  ; move the link bytes
                jsr indgrb
                jsr sta_far_ram1                        ; sta (grbtop),y
                dey
                jsr indgrb
                jsr sta_far_ram1                        ; sta (grbtop),y
                plx

                jsr indin1_ram1
                tax
                jsr movtop                              ; move top pointer
                sta frespc                              ; save in frespc
                sty frespc+1
                txa
                jsr movpnt                              ; move grbpnt
                txa                                     ; put length-1 in .y
                tay

l163_4          dey
                jsr indgrb
                phx
                ldx #grbtop
                jsr sta_far_ram1                        ; sta (grbtop),y
                plx
                dex
                bne l163_4

                ldy #2                                  ; fix the descriptor
                phx
                ldx #index1
l163_5          lda grbtop-1,y
                jsr sta_far_ram1                        ; sta (index1),y
                dey
                bne l163_5
                plx

                lda grbpnt                              ; check pointer
                ldy grbpnt+1
                jsr chkgrb                              ; check garbage string
                beq l163_1                              ; if garbage found
                bne l163_3                              ; always

l163_6          ldy #0                                  ; skip over good strings
                jsr indin1_ram1
                tax
                jsr movtop
                sta frespc
                sty frespc+1
                txa
                jsr movpnt
                bra gloop
