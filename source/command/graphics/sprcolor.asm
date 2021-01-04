; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      sprcolor.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;**************************************************************
;*
;*   SPRCOLOR - Set sprite multicolor registers
;*
;* syntax : SPRCOLOR [multicolor_1] [,multicolor_2]
;*
;**************************************************************

sprcolor
                cmp     #','                            ; is there a first arg?
                beq     l294_1                          ; nope, skip to second

                jsr     getnyb                          ; get 1 byte arg in .X, range 0-15
; jsr put_io_in_map
                stx     vic+37

l294_1          jsr     optbyt                          ; get (optional) 1 byte arg in .X
                bcc     l294_2
                jsr     chknyb                          ; range 0-15
; jsr put_io_in_map
                stx     vic+38

l294_2          rts

;.end



;***************************************************************
;  SPRSAV ( n1 / s1$ ) , ( n2 / s2$ )
;   - move string(s1) or sprite(n1) to string(s2) or sprite(n2)
;               n1 & n2 = a sprite number (1-8)
;                   s1$ = a string variable or expression
;                   s2$ = a string variable
;***************************************************************

sprsav          jsr     savinp                          ; evaluate 1st expression
                bcs     l295_2                          ; skip if source is a string
                sta     forpnt
                sty     forpnt+1                        ; save sprite address
                ldy     #62

l295_1          lda     (forpnt),y                      ; move sprite def to save area
                sta     savram,y
                dey
                bpl     l295_1

                iny                                     ; (0)
                sty     savram+64                       ; save sprite column length
                sty     savram+66                       ; save sprite row length
                lda     #23
                sta     savram+63
                lda     #20
                sta     savram+65
                ldx     #<savram                        ; set ptr to start of sprite def
                ldy     #>savram
                stx     strng1                          ; **
                sty     strng1+1                        ; **

                lda     #67                             ; set sprite length including lengths
                jsr     strlit_1                        ; **get string space, copy savram to it
                jsr     desc_free                       ; **free up temp descriptor

l295_2          stx     savsiz                          ; save source length
                sta     savsiz+1
                sty     savsiz+2                        ; save source start address

                jsr     chkcom                          ; check for a comma
                lda     txtptr                          ; save basic text pointer
                sta     sprtmp_1
                lda     txtptr+1
                sta     sprtmp_2
                jsr     savinp                          ; get next destination parameter
                bcs     savs50                          ; skip if string

                sta     grapnt
                sty     grapnt+1                        ; save sprite address
                lda     savsiz+1
                sta     forpnt                          ; get source address
                lda     savsiz+2
                sta     forpnt+1
                ldy     #0
l295_3          cpy     savsiz                          ; test index vs source length
                beq     l295_4                          ; exit if source depleted
                lda     #forpnt                         ; move source byte to sprite
                jsr     lda_far_ram1                    ; (from ram bank 1)
; sta sw_rom_ram0
                sta     (grapnt),y                      ; (to sprite area in bank 0)????
                iny
                cpy     #63
                bne     l295_3
l295_4          rts


savs50          lda     sprtmp_1                        ; restore basic text pointer
                sta     txtptr
                lda     sprtmp_2
                sta     txtptr+1
                jsr     ptrget                          ; get symbol table descriptor for string dest.
                sta     forpnt
                sty     forpnt+1                        ; save symbol table address
                lda     #<savsiz
                sta     facmo                           ; save descriptor address of source
                lda     #>savsiz
                sta     facmo+1
                +lbra   inpcom                          ; move source to dest, do rts (snerr if not eol)


savinp          jsr     frmevl                          ; evaluate expression
                bbs7    valtyp,desc_free                ; exit if a string
                jsr     conint                          ; get one byte integer in .X
; dex    ;adjust sprite 1..8 to 0..7  [910220]
                cpx     #8
                +lbcs   fcerr                           ; bad value
                txa                                     ; move sprite number to .A
                lsr
                ror
                ror                                     ; get sprite address
                ldy     #>sprite_base
                bcc     l296_1
                iny
l296_1          clc                                     ; flag 'sprite' (as opposed to 'string')
                rts


desc_free                                               ; free temporary descriptor, set up pointers to string.
                lda     facmo                           ; get address of temp descriptor
                ldy     facmo+1
                jsr     fretms
                ldy     #0                              ; get len, addr of string
                jsr     indfmo
                tax
                iny
                jsr     indfmo
                pha
                iny
                jsr     indfmo
                tay
                pla
                sec                                     ; flag 'string found'
                rts                                     ; return w/ x=len, (a,y)==> string

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
