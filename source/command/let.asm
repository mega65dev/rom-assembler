


;****************************************************************
;*
;*  [LET] variable = expression
;*
;****************************************************************

let              jsr ptrget                               ; get pntr to variable into "varpnt"
                 sta forpnt                               ; preserve pointer
                 sty forpnt+1
                 lda #equal_token
                 jsr synchr                               ; "=" is necessary

                 lda intflg                               ; save type for later
                 pha
                 lda valtyp                               ; retain the variable's value type too
                 pha

                 jsr frmevl                               ; get value of formula into FAC
                 pla
                 rol                                      ; carry set for string, off for numeric
                 jsr chkval                               ; make sure VALTYP matches carry
;and set zero flag for numeric
                 bne copstr                               ; if numeric, copy it
                 pla                                      ; get number type

qintgr           bpl copflt                               ; store a floating point number
                 jsr round                                ; round integer
                 jsr ayint                                ; make two-byte number
                 ldy #0
                 lda facmo                                ; get high
                 phx
                 ldx #forpnt
                 jsr sta_far_ram1 ;sta (forpnt),y         ; store it
                 iny
                 lda faclo                                ; get low
                 jsr sta_far_ram1                         ; sta (forpnt),y
                 plx
                 rts



copflt           ldx forpnt
                 ldy forpnt+1
                 +lbra movmf_ram1                         ; put number @forpnt in var bank



copstr           pla                                      ; if string, no INTFLG

inpcom           ldy forpnt+1                             ; TI$?
                 cpy #>zero                               ; (only TI$ can be this on assign)
                 +lbeq Set_TI_String                      ; yes
                 bra getspt                               ; no


dskx1            pla
                 iny

dskx2            cmp fretop+1
                 bcc l51_2
                 bne l51_1
                 dey
                 jsr indfmo
                 cmp fretop
                 bcc l51_2

l51_1            ldy faclo                                ; qvaria
                 cpy vartab+1                             ; if (vartab) > (facmo), don't copy
                 bcc l51_2
                 bne copy                                 ; it is less
                 lda facmo
                 cmp vartab                               ; compare low orders
                 bcs copy

l51_2            lda facmo                                ; dntcpy
                 ldy facmo+1
                 bra copyc


getspt           ldy #2                                   ; get pntr to descriptor
                 jsr indfmo
                 cmp dsdesc+2                             ; check for DS$ hi
                 bne dskx2                                ; nope
                 pha
                 dey
                 jsr indfmo
                 cmp dsdesc+1                             ; check for DS$ lo
                 bne dskx1                                ; nope
                 lda dsdesc                               ; check if len=0
                 beq dskx1                                ; yup
                 pla                                      ; fall through to copy


copy             ldy #0
                 jsr indfmo
                 jsr strini                               ; get room to copy string into
                 lda dscpnt                               ; get pointer to old descriptor, so
                 ldy dscpnt+1
                 sta strng1                               ; movins can find string
                 sty strng1+1
                 jsr movins                               ; copy it

                 lda strng1                               ; fix to free get strings
                 ldy strng1+1
                 jsr fretms                               ; free the string, if it is a temp

                 lda #<dsctmp
                 ldy #>dsctmp

copyc            sta dscpnt
                 sty dscpnt+1
                 sta index                                ; index points to new descriptor
                 sty index+1
                 jsr fretms


;   Fix the strings by flagging the old string as garbage and the new
;   string by pointing it to its new descriptor.

                 jsr stradj                               ; set up new string
                 bcc l52_1                                ; leave it alone
                 ldy #0
                 lda forpnt                               ; put in backwards link
                 phx
                 ldx #index
                 jsr sta_far_ram1
                 iny
                 lda forpnt+1
                 jsr sta_far_ram1
                 plx

l52_1            lda forpnt                               ; fix old string
                 sta index
                 lda forpnt+1
                 sta index+1
                 jsr stradj                               ; point to old string
                 bcc l52_2                                ; in text do not fix
                 dey                                      ; restore y
                 phx
                 ldx #index
                 lda #$ff                                 ; garbage flag
                 jsr sta_far_ram1
                 dey
                 pla                                      ; (was txa)
                 pha
                 jsr sta_far_ram1                         ; store length
                 plx

l52_2            ldy #2                                   ; set the descriptor
                 phx
                 ldx #forpnt
l52_3            lda #dscpnt
                 jsr lda_far_ram1                         ; lda (dscpnt),y from RAM1
                 jsr sta_far_ram1                         ; sta (forpnt),y to   RAM1
                 dey
                 bpl l52_3
                 plx
                 rts


;   STRADJ takes the pointer index which points to a descriptor and
;   indexes to the desciptor's string data.  If the string is not in
;   string space (no action to take) we return with carry clear, else
;   we return with the pointer set to the link bytes in the string, the
;   length in .a and the carry set.

stradj           ldy #0
                 jsr indin1_ram1                          ; push length on stack
                 pha
                 beq l53_5                                ; if length=0 do nothing
                 iny
                 jsr indin1_ram1                          ; get low byte (into .x)
                 tax
                 iny
                 jsr indin1_ram1                          ; get high byte
                 cmp max_mem_1+1
                 bcc l53_1                                ; ok
                 bne l53_5                                ; if above top of memory
                 cpx max_mem_1                            ; msb the same, test lsb
                 bcs l53_5                                ; if above top of memory

l53_1            cmp fretop+1
                 bcc l53_5                                ; if below fretop
                 bne l53_2
                 cpx fretop
                 bcc l53_5                                ; if below fretop

l53_2            cmp dsdesc+2
                 bne l53_3                                ; fix
                 cpx dsdesc+1
                 beq l53_5

l53_3            stx index                                ; ok set pointer
                 sta index+1
                 pla                                      ; get back length
                 tax                                      ; into x also
                 clc
                 adc index
                 sta index
                 bcc l53_4
                 inc index+1
l53_4            sec                                      ; carry set
                 rts

l53_5            pla                                      ; clean up stack
                 clc
                 rts

;.end