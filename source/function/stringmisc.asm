



; The STR$() function takes a number and gives a string with
; the characters the output of the number would have given.

strd             jsr chknum                               ; arg has to be numeric
                 ldy #0
                 jsr foutc                                ; do its output
                 pla
                 pla

timstr           lda #<lofbuf
                 ldy #>lofbuf
                 +lbra strlit


; CHR$() creates a string which contains as its only character the PETSCII
; equivalent of the integer argument (#) which must be < 256.

chrd             jsr conint                               ; get integer in range
                 phx
                 lda #1                                   ; one-character string
                 jsr strspa                               ; get space for string
                 ldy #0
                 pla
; phx   ;set up string bank
                 ldx #dsctmp+1
                 jsr sta_far_ram1                         ; sta (dsctmp+1),y
; plx

chrd1            pla                                      ; get rid of "chknum" return address
                 pla
                 +lbra putnew                             ; setup FAC to point to desc


; The following is the LEFT$($,#) function.  It takes the leftmost # characters
; of the string.  If # > len of the string, it returns the whole string.

leftd            jsr pream                                ; test parameters
                 pha                                      ; # arg
                 jsr inddpt                               ; string len
                 sta syntmp
                 pla
                 cmp syntmp
                 tya                                      ; that's all there is to LEFT$

rleft            bcc l152_1
                 jsr inddpt
                 tax                                      ; put length into x
                 tya                                      ; zero (a), the offset
l152_1           pha                                      ; save offset
rleft2           txa
rleft3           pha                                      ; save length
                 jsr strspa                               ; get space
                 lda dscpnt
                 ldy dscpnt+1
                 jsr fretmp
                 ply
                 pla
                 clc
                 adc index                                ; compute where to copy
                 sta index
                 bcc l153_1
                 inc index+1
l153_1           tya
                 jsr movdo                                ; go move it
                 +lbra putnew



rightd           jsr pream
                 pha
                 jsr inddpt
                 sta syntmp
                 pla
                 clc                                      ; (length des'd)-(length)-1
                 sbc syntmp
                 eor #$ff                                 ; negate
                 bra rleft


; MID$($,#) returns string with chars from # position onward. If # > LEN($)
; then return null string.  MID($,#,#) returns string with characters from
; # position for #2 characters.  If #2 goes past end of string return as much
; as possible.

midd             lda #255                                 ; default
                 sta faclo                                ; save for later compare
                 jsr chrgot                               ; get current character
                 cmp #')'                                 ; is it a right paren )?
                 beq l154_1                               ; no third paren.
; jsr chkcom  ;must have comma
; jsr getbyt  ;get the length into "faclo"
                 jsr combyt                               ; [910820]

l154_1           jsr pream                                ; check it out
                 +lbeq fcerr                              ; illegal qty error
                 dex                                      ; compute offset
                 phx
                 phx                                      ; preserve a while (2 copies)
                 ldx #0
                 jsr inddpt                               ; get length of what's left
                 sta syntmp
                 pla
                 clc
                 sbc syntmp
                 bcs rleft2                               ; give null string
                 eor #$ff                                 ; in sub c was 0 so just complement
                 cmp faclo                                ; greater than what's desired
                 bcc rleft3                               ; no, just copy that much
                 lda faclo                                ; get length of what's desired
                 bcs rleft3                               ; copy it




; Common routine used by RIGHT$, LEFT$, MID$, for parameter chk and setup.

pream            jsr chkcls                               ; param list should end
                 ply
                 pla
                 sta jmper+1                              ; get return address
                 pla                                      ; get rid of fingo's jsr ret addr
                 pla
                 plx                                      ; get length
                 pla
                 sta dscpnt
                 pla
                 sta dscpnt+1
                 lda jmper+1
                 pha
                 phy
                 ldy #0
                 txa
                 rts



; The function LEN$() returns the length of the string passed as an argument.

len              bsr len1
                 +lbra sngflt

len1             jsr frestr                               ; free up string
                 ldx #0
                 stx valtyp                               ; force numeric
                 tay                                      ; set condition codes
                 rts                                      ; done





; The following is the ASC$() function.  It returns an integer which is the
; decimal equivalent of the PETSCII string argument.

asc              jsr len1
                 beq l155_1                               ; it was null (zero length)
                 ldy #0
                 jsr indin1_ram1                          ; get 1st character
                 tay
l155_1           +lbra sngflt

;.end





; STRINI gets string space for the creation of a string and creates
; a descriptor for it in DSCTMP.

strini
                 ldx facmo                                ; get facmo to store in dscpnt
                 ldy facmo+1
                 stx dscpnt                               ; retain the descriptor pointer
                 sty dscpnt+1

strspa           jsr getspa                               ; get string space
                 stx dsctmp+1                             ; save location
                 sty dsctmp+2
                 sta dsctmp                               ; save length
                 rts                                      ; done


; STRLT2 takes the string literal whose first character is pointed to by
; (xreg)+1 and builds a descriptor for it.  The descriptor is initially
; built in DSCTMP, but PUTNEW transfers it into a temporary and leaves a
; pointer to the temporary in FACMO & FACLO.  The characters other than the
; zero that terminates the string should be set up in CHARAC and ENDCHR.
; If the terminator is a quote, the quote is skipped over.  Leading quotes
; should be skipped before call.  On return, the character after the string
; literal is pointed to by (strng2).


strlit           ldx #'"'                                 ; assume string ends on quote
                 stx charac
                 stx endchr

strlt2           sta strng1                               ; save pointer to string
                 sty strng1+1
                 sta dsctmp+1                             ; in case no strcpy
                 sty dsctmp+2

                 ldy #255                                 ; initialize character count
strget           iny
                 jsr indst1                               ; get character
                 beq l156_2                               ; if zero
                 cmp charac                               ; this terminator?
                 beq l156_1                               ; yes
                 cmp endchr
                 bne strget                               ; look further

l156_1           cmp #'"'                                 ; strfin.  quote?
                 beq l156_3

l156_2           clc
l156_3           sty dsctmp                               ; no, back up. retain count
                 tya
                 adc strng1                               ; wishing to set (txtptr)
                 sta strng2
                 ldx strng1+1
                 bcc l156_4
                 inx
l156_4           stx strng2+1
                 tya


strlit_1                                                  ; //// entry from SPRSAV
                 jsr strini
                 tay
                 beq putnew                               ; length=0, don't bother copying
                 pha                                      ; save length
                 phx
                 ldx #frespc
l157_1           dey
                 jsr indst1                               ; lda (strng1),y in bank 0
                 jsr sta_far_ram1                         ; sta (frespc),y in bank 1
                 tya
                 bne l157_1
                 plx
                 pla                                      ; restore length
                 jsr mvdone                               ; finish up by updating frespc


; Some string function is returning a result in DSCTMP.  Set up a temp
; descriptor with DSCTMP in it.  Put a pointer to the descriptor in FACMO&LO
; and flag the result as a string type.

putnew           ldx temppt                               ; pointer to first free temp
                 cpx #tempst+strsiz+strsiz+strsiz
                 +lbeq sterr                              ; string temporary error

                 lda dsctmp                               ; length
                 sta 0,x
                 lda dsctmp+1                             ; pointer to string lo
                 sta 1,x
                 lda dsctmp+2                             ; hi
                 sta 2,x

                 ldy #0                                   ; pointer to temp. descriptor
                 stx facmo                                ; lo
                 sty facmo+1                              ; hi
                 sty facov
                 dey                                      ; ($ff)
                 sty valtyp                               ; type is string
                 stx lastpt                               ; set pointer to last-used temp

                 inx
                 inx
                 inx                                      ; point further
                 stx temppt                               ; save pointer to next temp, if any
                 rts                                      ; all done


; The following routine concatenates two strings.  At this point, the FAC
; contains the first one and (txtptr) points to the + sign.

cat              lda faclo                                ; push high order onto stack
                 pha
                 lda facmo                                ; and the low
                 pha
                 jsr eval                                 ; can come back here since operator is known
                 jsr chkstr                               ; must be string
                 pla
                 sta strng1                               ; get high order of old descriptor
                 pla
                 sta strng1+1
                 ldy #0
                 jsr indst1_ram1                          ; get length of old string
                 sta syntmp
                 jsr indfmo
                 clc
                 adc syntmp
                 +lbcs errlen                             ; result >255, error "long string"

                 jsr strini                               ; sizeok.  initialize string
                 jsr movins                               ; move it
                 lda dscpnt                               ; get pointer to second
                 ldy dscpnt+1
                 jsr fretmp                               ; free it
                 jsr movdo                                ; move second string
                 lda strng1
                 ldy strng1+1
                 jsr fretmp
                 jsr putnew
                 +lbra tstop                              ; "cat" reenters frmevl from tstop


movins           ldy #0                                   ; get address of string
                 jsr indst1_ram1
                 pha
                 iny
                 jsr indst1_ram1
                 tax
                 iny
                 jsr indst1_ram1
                 tay
                 pla

movstr           stx index                                ; adr in (x,y), len in a
                 sty index+1

movdo            tay
                 beq mvdone

                 pha
                 phx
                 ldx #frespc
l158_1           dey
                 jsr indin1_ram1
                 jsr sta_far_ram1                         ; sta (frespc),y
                 tya
                 bne l158_1
                 plx
                 pla

mvdone           clc                                      ; update frespc pointer
                 adc frespc
                 sta frespc
                 bcc l159_1
                 inc frespc+1
l159_1           rts

