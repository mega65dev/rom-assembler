; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      next.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; Next routine
;
; 'FOR' entry on the stack has the following format:
;
; Low address
;  token (for_token) 1 byte
;  a pointer to the loop variable 2 bytes
;  the step 5 bytes
;  a byte reflecting the sign of the incr. 2 bytes
;  the upper value (packed) 5 bytes
;  the line number of the FOR statement 2 bytes
;  a text pointer into the FOR statement 2 bytes
; High address
;
; (total 16 bytes)

next            bne     l62_2                           ; hop if 'next' variable given
                ldy     #$ff                            ; flag no specific 'for' variable
                bra     l62_3                           ; always

l62_1           ldy     #lenfor                         ; done, clean up stack
                jsr     rlsstk                          ; release (y) items from stack
                jsr     chrgot
                cmp     #','                            ; ie., NEXT j,k
                bne     l62_7
                jsr     chrget

l62_2           jsr     ptrget                          ; get pointer to variable in (a,y)
                sta     forpnt

l62_3           sty     forpnt+1
                lda     #for_token
                jsr     search                          ; look for FOR entry in run-time stack
                beq     l62_4                           ; branch if found
                ldx     #errnf                          ; otherwise 'error, not found'
                +lbra   error


; Set up to move STEP value to FAC

l62_4           jsr     movfnd                          ; (fndpnt) => (tos)
                lda     fndpnt
                clc
                adc     #3                              ; offset to step value
                ldy     fndpnt+1
                bcc     l62_5
                iny

l62_5           jsr     movfm                           ; actually "move from ROM", but sys stack is in "common"
                ldy     #8                              ; MOVFM doesn't move sign.  Get it
                lda     (fndpnt),y
                sta     facsgn

; Get pointer to FOR variable

                ldy     #1
                lda     (fndpnt),y                      ; get lsb
                pha
                tax
                iny
                lda     (fndpnt),y                      ; get msb
                pha
                tay                                     ; msb in y
                txa                                     ; lsb in a
                jsr     fadd                            ; add STEP value to FOR variable (fadd gets from bank 1)
                ply                                     ; msb in y
                plx                                     ; lsb in x
                jsr     movmf_ram1                      ; put result back into FOR variable in var bank

; Make (a,y) point at TO value in stack

                lda     fndpnt
                clc
                adc     #9
                ldy     fndpnt+1
                bcc     l62_6
                iny

; Test if loop done

l62_6
; sta sw_rom_ram0 ;????
                jsr     fcomp                           ; compare FAC to value pointed to by (a,y)
                ldy     #8
                sec
                sbc     (fndpnt),y                      ; (common area????)
                beq     l62_1                           ; branch taken if done

                ldy     #17                             ; not done, set pointers to re-execute loop
                lda     (fndpnt),y                      ; (common area????)
                sta     txtptr
                dey
                lda     (fndpnt),y
                sta     txtptr+1
                dey
                lda     (fndpnt),y
                sta     curlin+1
                dey
                lda     (fndpnt),y
                sta     curlin
l62_7           rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
