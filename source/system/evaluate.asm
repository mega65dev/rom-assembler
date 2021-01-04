; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      evaluate.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


; Formula Evaluator Routine
;
; The formula evaluator starts with (txtptr) pointing to the first character
; in the formula.  At the end (txtptr) points to the terminator.
; The result is left in the FAC.  On return (a) does not reflect the terminator.
;
; The formula evaluator uses the operator (optab) to determine precedence and
; dispatch addresses for each operator.
; A temporary result on the stack has the following format:
;
;     * The address of the operator routine.
;     * The floating point temporary result.
;     * The precedence of the operator.


frmevl          dew     txtptr                          ; txtptr points to 1st char. in formula
                ldx     #0                              ; dummy precedence = 0
                !text $89

lpoper          pha                                     ; save precedence
                phx
                tsx                                     ; confirm enough system stack available (recursive calls)
                cpx     #<sysstk+44                     ; bottom of stack + room for error handling
                bcc     sterr                           ; formula too complex
                jsr     eval
                lda     #0
                sta     opmask

tstop           jsr     chrgot                          ; last char
loprel          sec                                     ; prepare to subtract
                sbc     #greater_token                  ; is current character a relation?
                bcc     endrel                          ; no, relations all through
                cmp     #less_token-greater_token+1
                bcs     endrel                          ; really relational?  no, just big
                cmp     #1                              ; reset carry for zero only
                rol                                     ; 0 to 1, 1 to 2, 2 to 4
                eor     #1
                eor     opmask                          ; bring in the old bits
                cmp     opmask                          ; make sure that the new mask is bigger
                +lbcc   snerr                           ; syntax error, because two of the same
                sta     opmask                          ; save mask
                jsr     chrget
                bra     loprel                          ; get the next candidate


endrel          ldx     opmask                          ; were there any?
                bne     finrel                          ; yes, handle as special op
                +lbcs   qop                             ; not an operator
                adc     #greater_token-plus_token
                +lbcc   qop                             ; not an operator
                adc     valtyp                          ; (c)=1
                +lbeq   cat                             ; only if (a)=0 and VALTYP=$FF (a string)

                adc     #$ff                            ; get back original (a)
                sta     index1
                asl                                     ; multiply by two
                adc     index1                          ; by three
                tay                                     ; set up for later

qprec           pla                                     ; get previous precedence
                cmp     optab,y                         ; is old precedence greater or equal?
                bcs     qchnum                          ; yes, go operate
                jsr     chknum                          ; can't be string here

doprec          pha                                     ; save old precedence

negprc          jsr     dopre1                          ; save a return for op
                pla                                     ; pull off previous precedence
                ldy     opptr                           ; get pointer to op
                bpl     qprec1                          ; that's a real operator
                tax                                     ; done?
                beq     qopgo                           ; done!
                bra     pulstk


finrel          lsr     valtyp                          ; get value type into (c)
                txa
                rol                                     ; put VALTYP into low order bit of mask
                dew     txtptr                          ; decrement text pointer
                ldy     #ptdorl-optab                   ; make (y) point at operator entry
                sta     opmask                          ; save the operation mask
                bra     qprec                           ; branch always


qprec1                                                  ; note b7(VALTYP)=0 so CHKNUM call is ok
                cmp     optab,y                         ; last precedence is greater?
                bcs     pulstk                          ; yes, go operate
                bcc     doprec                          ; no, save argument and get other operand


dopre1          lda     optab+2,y
                pha                                     ; disp addr goes on stack
                lda     optab+1,y
                pha
                jsr     pushf1                          ; save FAC on stack unpacked, precedence in (x)
                lda     opmask                          ; (a) may be mask for rel
                bra     lpoper


pushf1                                                  ; save FAC on stack unpacked
                pla                                     ; first grab return address off stack
                sta     index1
                pla
                sta     index1+1
                inw     index1

                ldx     optab,y                         ; precedence
                ldy     facsgn
                phy
                jsr     round                           ; put rounded FAC on stack
                lda     faclo
                pha
                lda     facmo
                pha
                lda     facmoh
                pha
                lda     facho
                pha
                lda     facexp
                pha
                jmp     (index1)                        ; return


pullf1                                                  ; retrieve FAC from stack unpacked  [910402]
                pla                                     ; first grab return address off stack
                sta     index1
                pla
                sta     index1+1
                inw     index1

                lda     #0                              ; it's been rounded
                sta     facov
                pla
                sta     facexp
                pla
                sta     facho
                pla
                sta     facmoh
                pla
                sta     facmo
                pla
                sta     faclo
                pla
                sta     facsgn
                jmp     (index1)                        ; return


qop             ldy     #255
                pla                                     ; get high precedence of last op
qopgo           beq     qoprts                          ; done!

qchnum          cmp     #100                            ; relational operator?
                beq     unpstk                          ; yes, don't check operand
                jsr     chknum                          ; must be number

unpstk          sty     opptr                           ; save operator's pointer for next time
pulstk          pla                                     ; get mask for rel op if it is one
                lsr                                     ; setup .c for dorel's chkval
                sta     domask                          ; save for "docmp"
                pla                                     ; unpack stack into arg
                sta     argexp
                pla
                sta     argho
                pla
                sta     argmoh
                pla
                sta     argmo
                pla
                sta     arglo
                pla
                sta     argsgn
                eor     facsgn                          ; get probable result sign
                sta     arisgn                          ; sign used by add, sub, mul, div

qoprts          lda     facexp                          ; get it and set codes
                rts                                     ; return

eval            jmp     (ieval)

neval           lda     #0                              ; assume numeric
                sta     valtyp

eval0           jsr     chrget                          ; get a character
                bcs     eval2
eval1           ldx     #0                              ; flag 'bank 0' (text bank)
                +lbra   fin                             ; it is a number

eval2           jsr     isletc                          ; variable name?
                bcs     is_variable                     ; yes.
                cmp     #pi                             ; pi?
                bne     qdot
                lda     #<pival
                ldy     #>pival
                jsr     movfm                           ; put value in for p1.
                jmp     chrget


qdot            cmp     #'.'                            ; constant?
                beq     eval1
                cmp     #minus_token                    ; negation?
                beq     domin                           ; yes.
                cmp     #plus_token
                beq     eval0
                cmp     #'"'                            ; string?
                bne     eval3

strtxt          lda     txtptr
                ldy     txtptr+1
                adc     #0                              ; c=1
                bcc     strtx2
                iny
strtx2          jsr     strlit                          ; process string

st2txt          ldx     strng2
                ldy     strng2+1
                stx     txtptr
                sty     txtptr+1
                rts


eval3           cmp     #not_token                      ; not?
                bne     eval4
                ldy     #24
                bne     gonprc                          ; branch always


notop           jsr     ayint                           ; integerize
                lda     faclo                           ; get argument
                eor     #$ff
                tay
                lda     facmo
                eor     #$ff

givayf          jsr     stoint                          ; integer to float routine
                +lbra   floats


eval4           cmp     #fn_token                       ; user defined function?
                +lbeq   fndoer                          ; yes
                cmp     #first_function_token           ; function name?
                +lbcs   isfun                           ; yes
; (functions are the highest numbered
; tokens so no need to check further)

parchk          jsr     chkopn                          ; only thing left is formula in parens
                jsr     frmevl                          ; a formula in parens

chkcls          lda     #')'                            ; close paren?
                !text $2c

chkopn          lda     #'('                            ; open paren?
                !text $2c

chkcom          lda     #','                            ; comma?


; SYNCHR looks at the current character to make sure it is the specific
; thing loaded into (a) just before the call to SYNCHR.  If not, it calls
; the "syntax error" routine.  Otherwise it gobbles the next char and returns.
;
; (a)=new char and TXTPTR is advanced by CHRGET.


synchr          ldy     #0
                sta     syntmp
                jsr     indtxt
                cmp     syntmp
                +lbne   snerr
                jmp     chrget                          ; ok



domin
l117_1          =negtab-optab                           ; negoff
                ldy     #l117_1                         ; precedence below '-'

gonprc          pla                                     ; get rid of rts addr.
                pla
                +lbra   negprc                          ; do negation

;.end



is_variable
                jsr     ptrget                          ; parse variable name, put name in varnam

isvret          sta     facmo                           ; save pointer to variable
                sty     facmo+1
                ldx     varnam
                ldy     varnam+1
                lda     valtyp
                beq     is_numeric                      ; branch if numeric

                lda     #0
                sta     facov
                cpx     #'T'                            ; TI$ is a special case. look for it
                bne     isvds                           ; no- go test for DS$
                cpy     #'I'+$80                        ; shifted I?
                bne     ds_rts                          ; no- and it's not DS$ either

; Variable name is TI$.  To see if this is 'the' TI$ and not an
; array TI$(), test to see if it has a pointer to the zero in "ROM".
; If it is an array item, its pointer will be to a real value, or
; a real zero.  If it isn't an array item, its pointer will point
; to a dummy zero in "ROM".

                lda     facmo+1
                cmp     #>zero
                bne     ds_rts                          ; not TI$, not DS$
                lda     facmo
                cmp     #<zero
                bne     ds_rts
                +lbra   Get_TI_String                   ; the one and only TI$


isvds           cpx     #'D'                            ; is this DS$?
                bne     ds_rts                          ; no
                cpy     #'S'+$80                        ; shifted S?
                bne     ds_rts                          ; no

                jsr     Check_DS                        ; yes- check DS$ allocation,
;  and get it if not in memory
                ldy     #$ff
l118_1          iny                                     ; copy DS$ to a temp.
                lda     #dsdesc+1                       ; must first determine how big it is
                jsr     lda_far_ram1                    ; lda (dsdesc+1),y
                bne     l118_1                          ; loop until terminating null found

                tya                                     ; length of string required
                jsr     strini                          ; get temp. string space & descriptor
                tay
                beq     l118_3                          ; (don't bother copying if length is 0)

                phx

                ldx     #dsctmp+1                       ; ???? was ldx #frespc
l118_2          lda     #dsdesc+1                       ; copy DS$ into temp
                dey
                jsr     lda_far_ram1                    ; lda (dsdesc+1),y
                jsr     sta_far_ram1                    ; sta (dsctmp+1),y
                tya
                bne     l118_2
                plx
                lda     dsdesc                          ; a=length     [901014] FAB
                jsr     mvdone                          ; ???? (does nothing on C128 - bug or oversight?)

l118_3          +lbra   putnew

ds_rts          rts


is_numeric
                bbr7    intflg,is_floating              ; branch if not an integer
                ldy     #0
                jsr     indfmo                          ; fetch high
                tax
                iny
                jsr     indfmo                          ; fetch low
                tay                                     ; put low in y
                txa                                     ; get high in a
                +lbra   givayf                          ; float and return


; Screen out TI, ST, ER, and EL, and assign values to them.  First test
; if the pointer points to "ROM" zero.  If not, it can't be any of the above.

is_floating
                lda     facmo+1
                cmp     #>zero
                bne     gomovf                          ; not TI, etc.
                lda     facmo
                cmp     #<zero
                bne     gomovf                          ; not TI, etc.


; The pointer does point to the ROM zero.  Now it is necessary to
; examine the actual variable name case by case.

                cpx     #'T'                            ; TI?
                bne     qstatv                          ; no
                cpy     #'I'
                bne     gomovf                          ; no, and it can't be ST either
                +lbeq   Get_TI


qstatv          cpx     #'S'                            ; ST?
                bne     qdsav                           ; no, go test DS
                cpy     #'T'
                bne     gomovf
                jsr     _readst                         ; (???? system bank for rs232 st)
                +lbra   float


qdsav           cpx     #'D'                            ; DS?
                bne     qerlin                          ; no, go test ER & EL
                cpy     #'S'
                bne     gomovf

; Get disk status - make the first two characters of DS$ string into a number.

                jsr     Check_DS                        ; get a DS$ string if one doesn't exist already
                ldy     #0
                lda     #dsdesc+1
                jsr     lda_far_ram1                    ; lda (dsdesc+1),y
                and     #$0f
                asl
                sta     garbfl
                asl
                asl
                adc     garbfl
                sta     garbfl
                iny
                lda     #dsdesc+1
                jsr     lda_far_ram1                    ; lda (dsdesc+1),y
                and     #$0f
                adc     garbfl
                +lbra   float


qerlin          cpx     #'E'                            ; ER or EL?
                bne     gomovf
                cpy     #'R'
                beq     qnumer
                cpy     #'L'
                bne     gomovf

                lda     errlin+1                        ; want EL (last error line #)
                ldy     errlin
                +lbra   nosflt

qnumer          lda     errnum                          ; want ER (number of last error)
                +lbra   float


gomovf          lda     facmo
                ldy     facmo+1

movfrm          sta     index1                          ; move value from RAM
                sty     index1+1

                ldy     #0
                jsr     indin1_ram1
                sta     facexp
                sty     facov

                iny                                     ; (1)
                jsr     indin1_ram1
                sta     facsgn
                ora     #$80
                sta     facho

                iny                                     ; (2)
                jsr     indin1_ram1
                sta     facmoh

                iny                                     ; (3)
                jsr     indin1_ram1
                sta     facmo

                iny                                     ; (4)
                jsr     indin1_ram1
                sta     faclo
                rts

;.end



;  Read the variable name at the current text position and put a pointer
;  to its value in VARPNT.   TXTPTR points to the terminating character.
;  Note that evaluating subscripts in a variable name can cause recursive
;  calls to PTRGET, so all values must be stored on the stack.

ptrget          ldx     #0
                jsr     chrgot
ptrgt1          stx     dimflg                          ; store flag away
ptrgt2          sta     varnam
                jsr     chrgot                          ; get current character
                jsr     isletc                          ; check for a letter
                +lbcc   snerr                           ; not a letter

                ldx     #0                              ; assume no second character
                stx     valtyp                          ; default is numeric
                stx     intflg                          ; assume floating
                jsr     chrget                          ; get following character
                bcc     l119_1                          ; branch if numeric
                jsr     isletc                          ; is it alpha?
                bcc     l119_3                          ; no, no second character. branch
l119_1          tax                                     ; issec. save second character of name

l119_2          jsr     chrget                          ; skip over remainder of name. we only care about 2 chars.
                bcc     l119_2                          ; ..eat numbers,
                jsr     isletc
                bcs     l119_2                          ; ..and alphas, too!

l119_3          cmp     #'$'                            ; nosec. is this a string?
                bne     l119_4                          ; if not, VALTYP = 0
                lda     #$ff
                sta     valtyp                          ; ..else, flag 'string'
                bra     l119_5

l119_4          cmp     #'%'                            ; notstr. isn't string. is it integer?
                bne     l119_6                          ; branch if not
                lda     subflg
; bne snerr ; syntax error if integers disabled
                +lbne   chkerr                          ; integers disallowed- type mismatch error  [910114]
                lda     #$80                            ; flag integer by turning on both high bits
                sta     intflg
                tsb     varnam

l119_5          txa                                     ; turnon. turn on msb of second character
                ora     #$80
                tax
                jsr     chrget                          ; get character after $ or %

l119_6          stx     varnam+1                        ; strnam. store away second character
                sec
                ora     subflg                          ; add flag whether to allow arrays
                sbc     #'('
                +lbeq   is_array                        ; note: won't match if SUBFLG set

                ldy     #0
                sty     subflg                          ; allow subscripts again
                lda     vartab                          ; place to start search
                ldx     vartab+1

l119_7          stx     lowtr+1                         ; stxfnd.
l119_8          sta     lowtr
                cpx     arytab+1                        ; at end of table yet?
                bne     l119_9
                cmp     arytab
                beq     notfns                          ; yes, we couldn't find it

l119_9          jsr     indlow_ram1                     ; lda (lowtr),y
                cmp     varnam                          ; compare high orders
                bne     l119_10
                iny
                jsr     indlow_ram1
                cmp     varnam+1                        ; and the low part?
                +lbeq   finptr                          ; !!that's it!!

                dey
l119_10         clc
                lda     lowtr
                adc     #7                              ; makes no difference among types
                bcc     l119_8
                inx
                bra     l119_7                          ; branch always




; Test for a letter: (c)=0 not a letter
;   (c)=1 a letter

isletc          cmp     #'A'
                bcc     l120_1                          ; if less than "a", return
                sbc     #'Z'+1                          ; $5a + 1
                sec
                sbc     #$a5                            ; reset carry if (a) .gt. "z".
l120_1          rts


notfns          tsx                                     ; check who's calling????
                lda     $102,x                          ; sniff processor stack
                cmp     #>pointer_ret
                beq     ldzr                            ; special case if called by pointer function

l121_1          = isvret-1
                cmp     #>l121_1                        ; is eval calling????
                bne     notevl                          ; no, carry on

ldzr            lda     #<zero                          ; set up pointer to simulated zero
                ldy     #>zero
                rts                                     ; for strings or numeric


qst001          cpy     #'I'+$80                        ; we know first is T, is second <shift>I (TI$)?
                beq     ldzr
                cpy     #'I'                            ; or I (TI)?
                bne     varok
                beq     gobadv


qst004          cpy     #'S'+$80                        ; check for DS$
                beq     gobadv
                cpy     #'S'                            ; check for DS
                bne     varok
                beq     gobadv


qst002          cpy     #'T'                            ; check for ST
                bne     varok
                beq     gobadv


qst003          cpy     #'R'                            ; check for ER
                beq     gobadv
                cpy     #'L'                            ; check for EL
                bne     varok


gobadv          +lbra   snerr



notevl          lda     varnam
                ldy     varnam+1
                cmp     #'T'                            ; screen out TI
                beq     qst001
                cmp     #'S'                            ; ...and ST
                beq     qst002
                cmp     #'E'                            ; ...and ER and EL
                beq     qst003
                cmp     #'D'                            ; ...and DS
                beq     qst004


varok           lda     arytab
                ldy     arytab+1
                sta     lowtr
                sty     lowtr+1
                lda     strend
                ldy     strend+1
                sta     hightr
                sty     hightr+1
                clc
                adc     #7
                bcc     l122_1                          ; not even
                iny

l122_1          sta     highds
                sty     highds+1
                jsr     bltu
                lda     highds
                ldy     highds+1
                iny
                sta     arytab
                sty     arytab+1


; Scan thru array entries for string arrays.  If any are found it will be
; necessary to adjust the back-links of the strings in that array, since
; the array descriptor block itself was moved.

                sta     arypnt                          ; set pointer to arrays
                sty     arypnt+1

aryva2          lda     arypnt
                ldx     arypnt+1

aryva3          cpx     strend+1                        ; end of arrays?
                bne     aryvgo
                cmp     strend
                beq     arydon                          ; ...finished


aryvgo          sta     index1
                stx     index1+1
                ldy     #0
                jsr     indin1_ram1                     ; look at array name
                tax
                iny
                jsr     indin1_ram1                     ; name 2nd char
                php                                     ; save status reg
                iny
                jsr     indin1_ram1                     ; point to offset to next array
                adc     arypnt
                sta     arypnt                          ; save start of next array in arypnt
                iny
                jsr     indin1_ram1
                adc     arypnt+1
                sta     arypnt+1
                plp                                     ; restore status
                bpl     aryva2                          ; not a string type
                txa
                bmi     aryva2                          ; not a string array
                iny                                     ; ok we have a string array
                jsr     indin1_ram1                     ; get number of dimensions
                ldy     #0
                asl                                     ; move index to ptr to 1st string (add 2*number of dims + 5)
                adc     #5
                adc     index1
                sta     index1
                bcc     aryget
                inc     index1+1

aryget          ldx     index1+1
                cpx     arypnt+1                        ; done with this array?
                bne     l123_1
                cmp     arypnt
                beq     aryva3                          ; yes

l123_1          ldy     #0                              ; process string pointer
                jsr     indin1_ram1                     ; get length of string
                beq     dvarts                          ; skip if null string
                sta     syntmp
                iny
                jsr     indin1_ram1                     ; get lo byte of string ptr
                clc
                adc     syntmp                          ; and add string length
                sta     hightr
                iny
                jsr     indin1_ram1                     ; get hi byte of string ptr
                adc     #0                              ; adjust high byte
                sta     hightr+1

; Fix backwards pointer by adding move length to it.

                phx
                ldx     #hightr
                ldy     #0
                jsr     indhtr_ram1                     ; lda (hightr),y
                adc     #7                              ; carry clear (careful!)
                jsr     sta_far_ram1                    ; sta (hightr),y
                iny
                jsr     indhtr_ram1                     ; lda (hightr),y
                adc     #0
                jsr     sta_far_ram1                    ; sta (hightr),y
                plx                                     ; done with this string

; Fix the next string in the array

dvarts          lda     #strsiz
                clc
                adc     index1
                sta     index1
                bcc     aryget
                inc     index1+1
                bra     aryget                          ; branch always


arydon          phx
                ldx     #lowtr
                ldy     #0
                lda     varnam
                jsr     sta_far_ram1                    ; sta (lowtr),y
                iny                                     ; .y=1
                lda     varnam+1
                jsr     sta_far_ram1                    ; sta (lowtr),y
                lda     #0
l124_1          iny
                jsr     sta_far_ram1                    ; sta (lowtr),y
                cpy     #6
                bne     l124_1
                plx

finptr          lda     lowtr
                clc
                adc     #2
                ldy     lowtr+1
                bcc     l125_1
                iny
l125_1          sta     varpnt
                sty     varpnt+1
                rts




bltu            jsr     reason
                sta     strend
                sty     strend+1
                sec
                lda     hightr
                sbc     lowtr
                sta     index
                tay
                lda     hightr+1
                sbc     lowtr+1
                tax
                inx
                tya
                beq     decblt
                lda     hightr
                sec
                sbc     index
                sta     hightr
                bcs     l126_1
                dec     hightr+1
                sec
l126_1          lda     highds
                sbc     index
                sta     highds
                bcs     moren1
                dec     highds+1
                bcc     moren1

bltlp           jsr     indhtr_ram1                     ; lda (hightr),y
                phx
                ldx     #highds
                jsr     sta_far_ram1                    ; sta (highds),y
                plx

moren1          dey
                bne     bltlp
                jsr     indhtr_ram1                     ; lda (hightr),y
                phx
                ldx     #highds
                jsr     sta_far_ram1                    ; sta (highds),y
                plx

decblt          dec     hightr+1
                dec     highds+1
                dex
                bne     moren1
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
