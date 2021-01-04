

;  FFORM - output a number to format
;
;  Number is in fbuffr,  format checked by anaf

fform
; sta sw_rom_ram0 ;????
                 lda pufill
                 sta blfd                                 ; set working fill char
                 lda #$ff
ana              sta sno                                  ; save blank or '-' in sno
                 bra insy


stp              stx point                                ; set point pointer
insy             cpy hulp                                 ; end of no reached?
                 beq eoa                                  ; branch if yes
                 lda fbuffr,y                             ; get character of no
                 iny
                 cmp #' '                                 ; blank?
                 beq insy                                 ; yes...span
                 cmp #'-'                                 ; sign no negative
                 beq ana                                  ; yes...
                 cmp #'.'                                 ; decimal point?
                 beq stp                                  ; yes...
                 cmp #'E'                                 ; is char 'e'?
                 beq lsg                                  ; yes...
                 sta fbuffr,x                             ; move number
                 stx enr                                  ; update end-no pointer
                 inx
                 bit point                                ; point pointer set?
                 bpl insy                                 ; yes...
                 inc vn                                   ; count digits before point
                 bra insy


lsg              lda fbuffr,y
                 cmp #'-'                                 ; sign of exponent negative
                 bne l201_1                               ; no...
                 ror usgn                                 ; make sign negative
l201_1           iny
                 sty uexp                                 ; set exponent pointer

eoa              lda point                                ; decimal found?
                 bpl l202_1                               ; yes...
                 stx point                                ; no...add point

l202_1           jsr anaf                                 ; analyze format
                 lda vf
                 cmp #$ff
                 beq l202_3                               ; field overflow
                 lda fesp                                 ; exponent in field
                 beq cff                                  ; convert to f format if not
                 lda uexp                                 ; exponent in number?
                 bne l202_2                               ; yes...
                 ldx enr
                 jsr et2                                  ; add exponent to number
                 dec fbuffr+2,x
                 inx
                 stx uexp
                 jsr alg                                  ; delete leading zeros
                 beq hup                                  ; all zero

l202_2           ldy posp                                 ; + or - in format?
                 bne sswe                                 ; yes...
                 ldy sno                                  ; +?
                 bmi sswe                                 ; yes...
                 lda vf

l202_3           beq errf                                 ; no room for sign
                 dec vf                                   ; reserve room
                 bne l202_4
                 lda nf                                   ; one #?
                 beq errf                                 ; yes...error

l202_4           inc swe

sswe             jsr shpn                                 ; shift decimal point
                 jsr uround                               ; round number
                 jsr shpn                                 ; shift again if necessary

hup              +lbra chout                              ; output number



cff              ldy uexp                                 ; exponent in no?
                 beq l203_2                               ; no...
                 sta hulp                                 ; delete exponent
                 sec                                      ; adjust decimal point
                 ror etof                                 ; set e-to-f flag
                 ldy point
                 lda usgn                                 ; exec nos3 or nos4
                 bpl l203_1                               ; depends on sign of exp
                 jsr nos3
                 bra l203_3

l203_1           jsr nos4

l203_2           ldy point                                ; at start of no?
                 beq l203_3                               ; yes...
                 jsr cho                                  ; no = 0 ?
                 beq l203_4                               ; yes...no round

l203_3           jsr uround
                 bra l203_5

l203_4           dec vn                                   ; adjust...no was 0
l203_5           sec
                 lda vf
                 sbc vn
                 bcc errf                                 ; no fit...error
                 sta swe                                  ; save difference
                 ldy posp                                 ; + or -?
                 bne ahp                                  ; yes...
                 ldy sno                                  ; get sign
                 bmi ahp                                  ; positive...
                 tay
                 beq errf                                 ; no room for sign
                 dey
                 bne ldvn                                 ; swe<>1
                 lda nf
                 ora vn                                   ; both zero?
                 bne hup                                  ; no...


errf             lda #'*'                                 ; format error
stout            jsr cdout                                ; fill field with stars
                 bne stout
                 rts


ahp              tay                                      ; output no if swe=0
                 beq hup
ldvn             lda vn
                 bne hup                                  ; vn<>0
                 dec swe                                  ; add extra 0
                 inc z_p_temp_1                           ; before decimal point
                 bra hup


; Using- shift decimal point

shpn             sec
                 lda vf
                 sbc vn
                 beq rdy                                  ; format o.k
                 ldy point
                 bcc pntl                                 ; vf<vn
                 sta hulp

incy             cpy enr                                  ; end of no reached?
                 beq ity
                 bcs nos1                                 ; yes...

ity              iny
nos1             inc vn
nos4             jsr eado                                 ; adjust exponent
                 dec hulp                                 ; ready?
                 bne incy                                 ; no...
                 beq poit

pntl             eor #$ff
                 adc #1
                 sta hulp                                 ; =vn-vf

decy             cpy bnr                                  ; begin of no?
                 beq inz1                                 ; yes...
                 dey
                 dec vn
                 bra inz2


inz1             inc z_p_temp_1                           ; add leading zeros
inz2             lda #$80
nos3             jsr eadj                                 ; adjust exponent
                 dec hulp                                 ; ready?
                 bne decy                                 ; no...

poit             sty point                                ; decimal point pointer
rdy              rts


; Using- adjust exponent

sexp             bne retrn                                ; no over/underflow
                 eor #$09                                 ; .a is 0 or 9
                 sta fbuffr,x                             ; digit back in exp
                 dex                                      ; = 0 or 9
                 cpx uexp
                 rts


eado             lda #0
eadj             ldx uexp
                 inx
                 bit etof                                 ; e-to-f flag on?
                 bmi l204_2                               ; yes...
                 eor usgn
                 beq l204_2                               ; ++ or --

l204_1           jsr tag3                                 ; inc exp, overflow?
                 jsr sexp                                 ; digit 0 if yes
                 bcs l204_1                               ; try second digit
                 +lbra overr                              ; exp>99

l204_2           lda fbuffr,x
                 dec fbuffr,x                             ; decrement exp
                 cmp #'0'                                 ; underflow on digit?
                 jsr sexp                                 ; set digit=9 if yes...
                 bcs l204_2                               ; try 2nd digit
                 bit etof                                 ; flag off?
                 bpl et3                                  ; yes...
                 sty point                                ; decimal point pointer

retrn            pla
                 pla
                 rts                                      ; return to fform/main


et3              lda usgn
                 eor #$80                                 ; reverse sign exp
et2              sta usgn
                 lda #'0'
                 sta fbuffr+1,x                           ; exp<0 here
                 lda #'1'
                 sta fbuffr+2,x
                 rts


tag3             lda fbuffr,x                             ; get digit of exp
                 inc fbuffr,x                             ; increment digit
                 cmp #'9'                                 ; overflow
                 rts                                      ; return .z set


; Using- ansub: load format field char in .a

ansub            clc
                 iny                                      ; begin format?
                 beq l205_1                               ; yes...
                 cpy lfor                                 ; end?
                 bcc l205_2                               ; no...

l205_1           ldy z_p_temp_1                           ; <>0?
                 bne retrn                                ; yes...

l205_2           jsr indfrm
                 inc cform                                ; pointer to field
                 rts


;  Using- ini: init counters and flags

ini              jsr frefac                               ; free temp descriptor
                 sta hulp                                 ; length string
                 ldx #$0a                                 ; printed in hulp
                 lda #0

l206_1           sta swe,x                                ; init working registers
                 dex
                 bpl l206_1
                 stx flag                                 ; comma flag =ff
                 stx point                                ; point pointer=ff
                 stx dolr                                 ; dollar flag=ff
                 tax                                      ; x=y=0
                 tay
                 rts


; Using- round number

uround           clc
                 lda point
                 adc nf                                   ; overflow?
                 bcs rrts                                 ; yes...
                 sec
                 sbc z_p_temp_1                           ; underflow?
                 bcc rrts                                 ; yes...
                 cmp enr                                  ; anything to round?
                 beq l207_1                               ; yes...
                 bcs rrts                                 ; no...

l207_1           cmp bnr                                  ; again...
                 bcc rrts                                 ; no...
                 tax
                 lda fbuffr,x                             ; get digit
                 cmp #'5'                                 ; <5 ?
                 bcc rrts                                 ; yes...no round

l207_2           cpx bnr                                  ; begin of no reached?
                 beq l207_3                               ; yes..add 1
                 dex
                 jsr tag3                                 ; increment digit
                 stx enr                                  ; new end of no pointer
                 beq l207_2                               ; branch on overflow
                 rts

l207_3           lda #'1'
                 sta fbuffr,x
                 inx
                 stx point
                 dec z_p_temp_1                           ; # of 0's before '.'
                 bpl rrts                                 ; no underflow
                 inc z_p_temp_1                           ; underflow...back to 0
                 inc vn
rrts             rts


; Using- alg: delete leading zeros of no

alg              ldy point                                ; start with a .?
                 beq szer                                 ; yes...

cho              ldy bnr
cmo              lda fbuffr,y                             ; start with a 0?
                 cmp #'0'
                 rts


nbr              inc point                                ; adjust point
                 jsr eado                                 ; and exponent
                 inc bnr                                  ; and pointer to begin of no
                 cpy enr                                  ; end of number?
                 beq rrts                                 ; yes...
                 iny

szer             jsr cmo                                  ; zero in no?
                 beq nbr                                  ; yes...
                 rts                                      ; no...


; Using- chout: print number

chout            lda dolr                                 ; dollar flag set?
                 bmi l208_1                               ; no...
                 inc z_p_temp_1                           ; make room for $

l208_1           ldx bnr                                  ; start of #
                 dex
                 ldy begfd                                ; begin of field

afrm             jsr indfrm
                 iny
                 cmp #','                                 ; comma?
                 bne punt                                 ; no...
                 bit flag                                 ; comma flag on?
                 bmi bout                                 ; yes, do a fill char
; sta sw_rom_ram0 ;????
                 lda pucoma
                 bra out                                  ; no,...output a comma char


bout             lda blfd
                 bra out


punt             cmp #'.'                                 ; decimal point?
                 bne afplus
; sta sw_rom_ram0 ;????
                 lda pudot                                ; yes...
                 bra out


afplus           cmp #'+'                                 ; plus?
                 beq ispl                                 ; yes...
                 cmp #'-'                                 ; minus?
                 beq ispl1                                ; yes...
                 cmp #'^'                                 ; up arrow?
                 bne pndd                                 ; no...
                 lda #'E'
                 jsr cdout
                 ldy uexp
                 jsr cmo                                  ; first dig of exp zero?
                 bne l209_1                               ; no...
                 iny
                 jsr cmo                                  ; second digit?
                 beq l209_2                               ; yes

l209_1           lda #'-'
                 bit usgn
                 bmi l209_3

l209_2           lda #'+'
l209_3           jsr cdout                                ; output sign exp
                 ldx uexp
                 lda fbuffr,x
                 jsr cdout                                ; output first dig exp
                 ldy endfd
                 bra telx

ispl1            lda sno                                  ; positive?
                 bmi bout                                 ; yes...out blank or *
ispl             lda sno                                  ; output sgn
                 bra out


zerot            lda z_p_temp_1                           ; # of zeros
                 bne zerot1
                 cpx enr                                  ; end of # reached?
                 beq zout                                 ; yes...output zero

telx             inx
                 lda fbuffr,x                             ; get digit
                 !text $2c                                ; skip

zout             lda #'0'                                 ; output zero

outs             lsr flag                                 ; clear comma flag

out              jsr cdout                                ; output character
                 +lbeq rrts
                 bra afrm                                 ; not ready...


zerot1           dec z_p_temp_1                           ; count leading zeros
                 lda dolr                                 ; $ flag set?
                 bmi zout                                 ; no...output zero

                 jsr indfrm                               ; take a peek at the next character in the format string
                 cmp #','                                 ; if it's a comma, we got problems
                 bne l210_1                               ; ...branch if no comma & resume normal processing

                 lda blfd                                 ; here's the "$,999.99" bug fix:
                 jsr cdout                                ; print a 'fill' character instead of the '$'
                 iny                                      ; and increment format string pointer past comma

l210_1           sec                                      ; resume normal processing
                 ror dolr                                 ; clear the dollar flag & go on to print '$'
; sta sw_rom_ram0 ;????
                 lda pumony
                 bra outs                                 ; output money symbol


pndd             lda swe                                  ; # of blanks
                 beq zerot
                 dec swe                                  ; count !

l211_1           +lbne bout                               ; out blank or *
                 lda posp                                 ; + or - in field?
                 bmi l211_1                               ; yes...out blank or *

l211_2           jsr indfrm
                 cmp #','                                 ; comma?
                 bne ispl1                                ; no...out sign
                 lda blfd                                 ; yes...
                 jsr cdout                                ; out blank or *
                 iny
                 bra l211_2



cdout            jsr outch ;outdo                         ; char out
                 dec cform                                ; count it
                 rts


; Using- anaf: analyze format

anaf             ldy endfd                                ; format pointer
gfor             jsr ansub
                 jsr comp                                 ; check special chars
                 bne pchar                                ; no match...
                 sty begfd                                ; save beginning of field
                 bcc ffoun                                ; # was found
                 tax                                      ; save char

sfur             jsr ansub                                ; get next format char
                 bcs l212_1                               ; stop on wrap-around
                 jsr com1                                 ; compare specials
                 beq foun1                                ; found some...

l212_1           ldy begfd
                 txa
pchar            jsr outch ;outdo                         ; out character
                 bra gfor


foun1            bcs sfur                                 ; no #...keep looking
                 ldy begfd
ffoun            ldx z_p_temp_1
                 bne trts
                 stx cform
                 dey

hyo2             dec cform                                ; correct counter
hyo              jsr ansub                                ; get next format char
                 bcs efo                                  ; end of format
                 cmp #','                                 ; comma?
                 beq hyo                                  ; yes...ignore it
                 jsr isp                                  ; + or - in format?
                 bcc hyo2                                 ; yes...
                 cmp #'.'                                 ; decimal point?
                 bne avf1                                 ; no...
                 inx
                 cpx #2                                   ; more than 1 decimal?
                 bcc hyo                                  ; no...
ero              +lbra snerr                              ; yes...syntax error


avf1             jsr com2                                 ; =, >, or # in field
                 bne llar                                 ; no...
                 bcc hyo1                                 ; was #
                 sta chsn                                 ; was > or =

hyo1             inc vf,x
                 bra hyo

llar             cmp #'$'                                 ; dollar?
                 bne expo                                 ; no...
                 bit dolr                                 ; test flag
                 bpl hyo1                                 ; no...
                 clc
                 ror dolr                                 ; set flag
                 dec vf
                 bra hyo1

expo             cmp #'^'                                 ; up arrow?
                 bne isp                                  ; no...
                 ldx #$02
l213_1           jsr ansub                                ; must be 4 up arrows
                 bcs ero
                 cmp #'^'                                 ; up arrow?
                 bne ero
                 dex
                 bpl l213_1
                 inc fesp                                 ; set exp flag
                 jsr ansub                                ; next format char
                 bcs efo                                  ; end of format

isp              cmp #'+'                                 ; plus?
                 bne chom                                 ; no...
                 lda sno                                  ; sign neg?
                 bpl spos                                 ; yes...
                 lda #'+'
                 sta sno

spos             lda posp                                 ; + or - already?
                 bne ero
                 ror posp                                 ; make posp neg/clc
                 sty endfd                                ; end field pointer
                 inc cform                                ; correct counter
trts             rts


chom             cmp #'-'                                 ; minus?
                 beq spos                                 ; yes...
                 sec
efo              sty endfd                                ; end field pointer
                 dec endfd
                 rts


; Using- comp: compare .a with symbols

comp             cmp #'+'
                 beq rt
                 cmp #'-'
                 beq rt

com1             cmp #'.'
                 beq rt

com2             cmp #'='
                 beq rt
                 cmp #'>'
                 beq rt
                 cmp #'#'
                 bne rt
                 clc
rt               rts

;.end

