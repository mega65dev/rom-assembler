; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      utils.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

; Subroutines used for garbage collection.
;
;  Compare for (y,a) = fretop.
; Entry  (y,a) = address of current string descriptor.
; Exits to caller if (y,a) = fretop, else z flag set if garbage string.
;      z flag clear if not garbage string.
; In either case pointers are setup for next loop and string movement.
; If carry clear (y,a) <= fretop


chkgrb          cpy     fretop+1                        ; end of strings?
                bcc     l164_5
                bne     l164_1                          ; if not equal
                cmp     fretop
                beq     l164_5
                bcc     l164_5

l164_1          bit     highds                          ; check flag
                bmi     l164_2                          ; if empty string found
                lda     #2                              ; skip pointers past
                jsr     movtop                          ; move top pointer

l164_2          lda     #2                              ; skip pointers past
                jsr     movpnt                          ; move pointers
                ldy     #1
                jsr     indgrb                          ; garbage?
                cmp     #$ff
                beq     l164_4                          ; yes

l164_3          jsr     indgrb                          ; to link bytes
                sta     index1,y
                dey
                bpl     l164_3                          ; if two bytes not moved
l164_4          rts


l164_5          ldx     temppt                          ; ptr to temp. strings

l164_6          cpx     #tempst                         ; any out there?
                beq     l164_7                          ; no
                jsr     slr1                            ; setup ptr (tempf2) to temp. string's bkptr.
                beq     l164_6                          ; (skip if null string!)
                phx
                ldx     #tempf2
                ldy     #0                              ; .a = string length
                jsr     sta_far_ram1 ;sta (tempf2),y    ; remove backpointer built at garba2
                iny
                lda     #$ff
                jsr     sta_far_ram1 ;sta (tempf2),y    ; and mark as garbage
                plx
                bra     l164_6                          ; always

l164_7          pla                                     ; throw away return address
                pla
                lda     frespc                          ; fix fretop and frespc
                ldy     frespc+1
                sta     fretop
                sty     fretop+1
                rts


movpnt          eor     #$ff                            ; comp and add
                sec
                adc     grbpnt
                ldy     grbpnt+1
                bcs     l165_1
                dey
l165_1          sta     grbpnt
                sty     grbpnt+1
                rts



movtop          eor     #$ff                            ; comp and add
                sec
                adc     grbtop
                ldy     grbtop+1
                bcs     l166_1
                dey
l166_1          sta     grbtop
                sty     grbtop+1
                rts



slr1            dex                                     ; .x = ptr to temp. string descriptor
                lda     0,x                             ; msb of ptr to string
                sta     tempf2+1
                dex
                lda     0,x                             ; lsb of ptr to string
                sta     tempf2
                dex
                lda     0,x                             ; string length
                pha                                     ; save for later test
                clc
                adc     tempf2                          ; want ptr to string's backpointer
                sta     tempf2
                bcc     l167_1
                inc     tempf2+1
l167_1          pla     ;.a=len & set z flag            ; .x=next desc. ptr
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
