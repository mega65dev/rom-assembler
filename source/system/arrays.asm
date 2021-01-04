; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      arrays.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; The format of arrays in core:
;
; Descriptor: low  byte = first character
;   high byte = second character (msb is string flag)
; Length of array in memory in bytes (includes everything).
; Number of dimensions.
; For each dimension starting with the first a list (2 bytes each)
; of the max indice+1.
; The values.


is_array
                lda     dimflg
                ora     intflg
                pha                                     ; save DIMFLG for recursion
                lda     valtyp
                pha                                     ; save VALTYP for recursion
                ldy     #0                              ; set number of dimensions to zero

l127_1          phy                                     ; save number of dims
                lda     varnam+1
                pha
                lda     varnam
                pha                                     ; save looks
                jsr     intidx                          ; evaluate indice into facmo&lo
                pla
                sta     varnam
                pla
                sta     varnam+1                        ; get back all...we're home
                ply                                     ; (# of units)
                tsx
                lda     258,x
                pha                                     ; push DIMFLG and VALTYP further
                lda     257,x
                pha
                lda     indice                          ; put indice onto stack
                sta     258,x                           ; under DIMFLG and VALTYP
                lda     indice+1
                sta     257,x
                iny                                     ; y counts # of subscripts
                sty     count                           ; protect y from chrget
                jsr     chrgot                          ; get terminating character
                ldy     count
                cmp     #','                            ; more subscripts?
                beq     l127_1                          ; yes


                jsr     chkcls                          ; must be closed paren
                pla
                sta     valtyp                          ; get VALTYP and
                pla
                sta     intflg
                and     #$7f
                sta     dimflg                          ; DIMFLG off stack
                ldx     arytab                          ; place to start search
                lda     arytab+1


l127_2          stx     lowtr
                sta     lowtr+1
                cmp     strend+1                        ; end of arrays?
                bne     l127_3
                cpx     strend
                beq     notfdd                          ; a fine thing! no array!

l127_3          ldy     #0
                jsr     indlow_ram1                     ; get high of name from array bank (ram1)
                iny
                cmp     varnam                          ; compare high orders.
                bne     l127_4                          ; no way is it this. get the bite outta here
                jsr     indlow_ram1
                cmp     varnam+1                        ; low orders?
                beq     gotary                          ; well here it is

l127_4          iny
                jsr     indlow_ram1                     ; get length
                clc
                adc     lowtr
                tax
                iny
                jsr     indlow_ram1
                adc     lowtr+1
                bcc     l127_2                          ; always branches


bserr           ldx     #errbs                          ; get bad sub error number
                !text $2c

fcerr           ldx     #errfc                          ; too big. Illegal Quantity error
                +lbra   error



gotary          ldx     #errdd                          ; perhaps a "re-dimension" error
                lda     dimflg                          ; test the DIMFLG
                +lbne   error
                jsr     fmaptr
                ldy     #4
                jsr     indlow_ram1
                sta     syntmp
                lda     count                           ; get number of dims input.
                cmp     syntmp                          ; # of dims the same?
                bne     bserr                           ; same so get definition.
                +lbra   getdef


; Come here when variable is not found in the array table to build an entry.
;
; Put down the descriptor.
; Setup number of dimensions.
; Make sure there is room for the new entry.
; Remember VARPNT.
; Tally=4.
; Skip two locs for later fill in of size.
; LOOP: Get an indice.
;  Put down number+1 and increment VARPTR.
;  Tally=tally*number+1
;  Decrement number of dims.
;  Bne LOOP
; Call REASON with (a,b) reflecting last loc of variable.
; Update STREND
; Zero all.
; Make tally include maxdims and descriptor.
; Put down tally
; If called by dimension, return.
;  Else index into the variable as if it were found on the initial search.

notfdd
                jsr     fmaptr                          ; form ARYPNT
                jsr     reason
                ldy     #0
                sty     curtol+1
                ldx     #5
                lda     varnam
                php
                phx
                ldx     #lowtr                          ; point to string/array bank
                jsr     sta_far_ram1                    ; sta (lowtr),y
                plx
                plp
                bpl     l128_1
                dex

l128_1          iny                                     ; notflt.
                lda     varnam+1
                php
                phx
                ldx     #lowtr                          ; point to string/array bank
                jsr     sta_far_ram1                    ; sta (lowtr),y
                plx
                plp
                bpl     l128_2
                dex
                dex

l128_2          stx     curtol
                lda     count                           ; save number of dimensions
                iny
                iny
                iny
                ldx     #lowtr                          ; point to string/array bank
                jsr     sta_far_ram1                    ; sta (lowtr),y

l128_3          ldx     #11                             ; loppta. default size
                lda     #0
                bbr6    dimflg,l128_4                   ; not in a dim statement
                pla                                     ; get low order of indice
                clc
                adc     #1
                tax
                pla                                     ; get high order of indice
                adc     #0

l128_4          iny                                     ; notdim.
                phx
                ldx     #lowtr
                jsr     sta_far_ram1 ;sta (lowtr),y     ; store high part of indice
                plx
                iny
                txa
                phx
                ldx     #lowtr
                jsr     sta_far_ram1 ;sta (lowtr),y     ; store low part of indice
                plx
                jsr     umult                           ; (a,x)+(curtol)*(lowtr,y)
                stx     curtol                          ; save new tally
                sta     curtol+1
                ldy     index
                dec     count                           ; any more indices left?
                bne     l128_3                          ; yes
                adc     arypnt+1
                +lbcs   omerr                           ; overflow
                sta     arypnt+1                        ; compute where to zero
                tay
                txa
                adc     arypnt
                bcc     l128_5
                iny
                +lbeq   omerr

l128_5          jsr     reason                          ; grease.  get room
                sta     strend
                sty     strend+1                        ; new end of storage
                lda     #0                              ; storing (a) is faster than clear
                inc     curtol+1
                ldy     curtol
                beq     l128_7

l128_6          dey                                     ; zero out new entry
                php
                phx
                ldx     #arypnt
                jsr     sta_far_ram1                    ; sta (arypnt),y
                plx
                plp
                bne     l128_6                          ; no. continue

l128_7          dec     arypnt+1                        ; deccur.
                dec     curtol+1
                bne     l128_6                          ; do another block
                inc     arypnt+1                        ; bump back up. will use later
                sec
                lda     strend                          ; restore (a)
                sbc     lowtr                           ; determine length
                ldy     #2
                phx
                ldx     #lowtr
                jsr     sta_far_ram1 ;sta (lowtr),y     ; low
                lda     strend+1
                iny
                sbc     lowtr+1
                jsr     sta_far_ram1 ;sta (lowtr),y     ; high
                plx
                lda     dimflg                          ; quit here if this is a DIM statement
                bne     dimrts                          ; bye!
                iny


; At this point (LOWTR,y) points beyond the size to the number of dimensions.
; Strategy:
;  NUMDIM = number of dimensions
;  curtol = 0
;  INLPNM: Get a new indice
;   Make sure indice is not too big
;   Multiply CURTOL by CURMAX
;   Add indice to CURTOL
;   NUMDIM=NUMDIM-1
;   bne INLPNM
;  Use (CURTOL)*4 as offset


getdef          jsr     indlow_ram1                     ; get # of dim's from string bank
                sta     count                           ; save a counter
                lda     #0                              ; zero (curtol)
                sta     curtol

inlpnm          sta     curtol+1
                plx                                     ; get low indice
                stx     indice
                iny
                jsr     indlow_ram1
                sta     syntmp
                pla                                     ; and the high part
                sta     indice+1
                cmp     syntmp                          ; compare with max indice
                bcc     inlpn2
                bne     bserr7                          ; if greater, "bad subscript" error
                iny
                jsr     indlow_ram1
                sta     syntmp
                cpx     syntmp
                bcc     inlpn1

bserr7          +lbra   bserr


inlpn2          iny
inlpn1          lda     curtol+1                        ; don't multiply if curtol=0
                ora     curtol
                clc                                     ; prepare to get indice back
                beq     l129_1                          ; get high part of indice back
                jsr     umult                           ; multiply (curtol) by (5&6,lowtr)
                txa
                adc     indice                          ; add in (indice)
                tax
                tya
                ldy     index1

l129_1          adc     indice+1
                stx     curtol
                dec     count                           ; any more?
                bne     inlpnm                          ; yes
                sta     curtol+1
                ldx     #5
                lda     varnam
                bpl     l129_2
                dex
l129_2          lda     varnam+1
                bpl     l129_3
                dex
                dex
l129_3          stx     addend
                lda     #0
                jsr     umultd                          ; on rts, a & y = hi. x = lo.
                txa
                adc     arypnt
                sta     varpnt
                tya
                adc     arypnt+1
                sta     varpnt+1
                tay
                lda     varpnt
dimrts          rts


; Integer arithmetic routines.
;
; Two byte unsigned integer multiply.
; This is for multiply dimensioned arrays.
; (a,b)=(curtol)*(5&6,x).

umult
                sty     index
                jsr     indlow_ram1
                sta     addend                          ; low, then high
                dey
                jsr     indlow_ram1                     ; put (5&6,lowtr) in faster memory

umultd          sta     addend+1
                lda     #16
                sta     deccnt
                ldx     #0                              ; clear the accs
                ldy     #0                              ; result initially zero

umultc          txa
                asl                                     ; multiply by two
                tax
                tya
                rol
                tay
                +lbcs   omerr                           ; to much!
                asl     curtol
                rol     curtol+1
                bcc     umlcnt                          ; nothing in this position to multiply
                clc
                txa
                adc     addend
                tax
                tya
                adc     addend+1
                tay
                +lbcs   omerr                           ; man, just too much!

umlcnt          dec     deccnt                          ; done?
                bne     umultc                          ; keep it up
                rts                                     ; yes, all done


fmaptr          lda     count
                asl
                adc     #5                              ; point to entries. ((c) cleared by asl)
                adc     lowtr
                ldy     lowtr+1
                bcc     l130_1
                iny
l130_1          sta     arypnt
                sty     arypnt+1
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
