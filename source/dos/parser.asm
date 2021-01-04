


; This is the DOS parser routine which looks at lines passed to it and
; verifies that the syntax is proper.  -mgm 7/23/79-
;
; Entry  (dosprs)
; a = parsts bit which must be zero.
;
; Exit  a = parsts as follows
;
;    ---------------------------------
;    | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
;    ---------------------------------
;      ^   ^   ^   ^   ^   ^   ^   ^
;      |   |   |   |   |   |   |   |_____ fn1 set for the first filename
;      |   |   |   |   |   |   |_______ fn2 set for second filename
;      |   |   |   |   |   |_________ la set when #lfn parsed
;      |   |   |   |   |___________ fa set for device number
;      |   |   |   |_____________ d1 set for first disk unit
;      |   |   |_______________ d2 set for second disk unit
;      |   |_________________ dosrcl set for record size
;      |___________________ @ set when @ encountered
;
; The following are the vaild bit patterns for parsts after parsing for the
; various keywords:
;
;       7 6 5 4  3 2 1 0
;
;     header  0 0 0 *  * 0 0 1
;     collect 0 0 0 *  * 0 0 0
;     dclear  0 0 0 *  * 0 0 0
;     backup  0 0 1 1  * 0 0 0
;     copy    0 0 1 1  * 0 0 0
;      or..   0 0 * *  * 0 1 1
;     concat  0 0 * *  * 0 1 1
;     bsave   * 0 0 *  * 0 0 1
;     dsave   * 0 0 *  * 0 0 1
;     bload   0 0 0 *  * 0 0 1
;     dload   0 0 0 *  * 0 0 1
;     dverify 0 0 0 *  * 0 0 1
;     catalog 0 0 0 *  * 0 0 *
;     rename  0 0 0 *  * 0 1 1
;     append  0 0 0 *  * 1 0 1
;     scratch 0 0 0 *  * 0 0 1
;     dopen   * * 0 *  * 1 0 1
;     dclose  0 0 0 0  * * 0 0
;             ^ ^ ^ ^  ^ ^ ^ ^
;             @ l d d  f l f f
;             r r 2 1  a a n n
;             p e          2 1
;             l l
;
;      "0" bits are required to be clear
;      "1" bits are required to be set
;      "*" bits are optional parameters

; Entry (dosprs):
;  parstx shall be set to prevent any auxiliary options to be specified.
;
; Entry (dosprx):
;  .x = parstx bits which must be zero.
;
; Exit:  .x = parstx as follows:
;
;     ---------------------------------
;     | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
;     ---------------------------------
;       ^   ^   ^   ^   ^   ^   ^   ^____ bnk is set for bank option
;       |   |   |   |   |   |   | ______ offl set for 1st address
;       |   |   |   |   |   |_________ offh set for 2nd address
;       |   |   |   |   |___________ unused
;       |   |   |   |_____________ unused
;       |   |   |_______________ unused
;       |   |_________________ unused
;       |___________________ unused
;
; The following are the valid bit patterns for parstx after parsing for the
; various keywords.  These options are allowed for load/save commands only.
;
;    7 6 5 4  3 2 1 0
;
;   bsave   0 0 0 0  0 1 1 *
;   bload   0 0 0 0  0 0 * *
;   bverify 0 0 0 0  0 0 * *
;   [auto]boot 0 0 0 0  0 0 * *
;    ^ ^ ^ ^  ^ ^ ^ ^
;    ? ? ? ?  ? o o b
;        f f n
;        h l k
;
;      "0" bits are required to be clear
;      "1" bits are required to be set
;      "*" bits are optional parameters

dostbl           !word $ffff,$ffff                        ; default start/end address
                 !text doslfn,dosffn,$6f                  ; default la/fa/sa


dospar           lda #0                                   ; DOS Parser

dosprs                                                    ; special error flag entry
                 ldx #$ff                                 ; no aux options!

dosprx                                                    ; spec aux error flag entry
                 pha                                      ; save error flags
                 phx
                 lda #0
                 sta parsts                               ; reset parser status/option words
                 sta parstx

                 ldx #dosspc                              ; clear DOS scratch area   [900522]
l233_1           sta xcnt-1,x
                 dex                                      ; no filenames, null lengths
                 bne l233_1

                 ldx #dossa-dosofl                        ; set some defaults from table
l233_2           lda dostbl,x
                 sta dosofl,x                             ; start/end adr = $FFFF, la/fa/sa
                 dex
                 bpl l233_2

                 ldx _default_drive                       ; set default device   [900522]
                 stx dosfa
                 ldx current_bank                         ; set current bank
                 stx dosbnk

                 jsr chrgot                               ; get next character from command string
                 bne parse1                               ; if eol stick with defaults, else begin parsing


; Done parsing, check for errors, return if everything okay

done             pla                                      ; get aux error flag
                 and parstx                               ; repeated or illegal params?
                 +lbne snerr                              ; yes- report syntax error
                 pla                                      ; get error flags
                 jsr prmrpt
                 lda parsts
                 ldx parstx
                 rts


; Parse given parameters.  what it is  example
;     -------------------- ---------
parse1           cmp #'"'
                 +lbeq name1                              ; explicit filename "file"
                 cmp #'('
                 +lbeq name1                              ; evaluate filename (f$)
                 cmp #'#'
                 beq logadr                               ; logical file number #1
                 cmp #'U'
                 beq unit1                                ; unit number  U8
                 cmp #'D'
                 beq drv1                                 ; drive number  D0
                 cmp #'P'
                 +lbeq doffl                              ; load/save address P1234
                 cmp #'B'
                 beq dbank1                               ; load/save bank   B0
                 cmp #'W'
                 beq reclen                               ; write mode  W
                 cmp #'L'
                 beq reclen                               ; record length  L80
                 cmp #'R'
                 +lbeq recover                            ; recover mode  R
                 cmp #'I'
                 beq ident                                ; ID   Ixx
                 cmp #on_token
; beq on1   ; ON token  ON

                 +lbne snerr                              ; none of these, syntax error


on1              jsr on
                 +lbra del1


unit1            jsr unit                                 ; do unit# parsing
                 +lbra del1                               ; always


dbank1           jsr dbank
                 +lbra del1                               ; always


logadr           lda #4
                 jsr prmrpt                               ; check for repeated parameter
                 jsr gtbytc                               ; getval
                 txa                                      ; cpx #0
                 +lbeq fcerr                              ; if illegal value
                 stx dosla
                 lda #4                                   ; set logical address flag
                 +lbra del1                               ; get next parameter


reclen           tax                                      ; save char
                 lda #$40
                 jsr prmrpt                               ; check for repeated parameter
                 cpx #'W'
                 bne l234_1
                 jsr chrget
                 bra l234_4                               ; set parsts

l234_1           ldx #1                                   ; a kludge to allow  DOPEN#lf,"relfile",L  [911024]
                 jsr chrget
                 beq l234_2                               ; eol? open existing rel file
                 jsr getbyt                               ; get reclen (was getval)
l234_2           stx dosrcl                               ; store parcel
                 txa                                      ; cpx #0
                 beq l234_3                               ; zero illegal dosrcl
                 inx                                      ; cpx #255
l234_3           +lbeq fcerr                              ; illegal dosrcl

l234_4           lda #$40                                 ; set dosrcl flag &
                 +lbra del1


drv1             lda #$10
                 jsr prmrpt                               ; check for repeated parameter
                 jsr gtbytc                               ; getval
                 cpx #10
                 +lbcs fcerr                              ; illegal drv# if >9 [allow 0: to 9: ?????]
                 stx dosds1
                 stx dosds2
                 lda #$10
                 +lbra del1


ident            lda #$80                                 ; set ID flag
                 tsb dosflags
                 +lbne snerr                              ; repeated parameter
                 jsr chrget                               ; get next character
                 cmp #'('                                 ; c65: allow I(ID$) syntax  [900710]
                 bne l235_1
                 jsr frmstr                               ; get ID from var
                 cmp #2
                 +lbcc err_mfn                            ; if length < 2, error
                 ldy #0
                 jsr indin1_ram1                          ; else grab first two characters
                 sta dosdid
                 iny
                 jsr indin1_ram1
                 sta dosdid+1
                 bra delim1                               ; continue

l235_1           sta dosdid                               ; m(txtptr => dosdid
                 jsr chrget
                 sta dosdid+1
                 jsr chrget                               ; continue
                 bra delim2


doffl            lda #$02                                 ; check aux status
                 jsr prxrpt
                 jsr getoff                               ; get offset value
                 sty dosofl
                 sta dosofl+1
                 lda #$02
dlimx1           ora parstx                               ; set aux status bits
                 sta parstx
                 bne delim1                               ; try for next param


doffh            lda #$04
                 jsr prxrpt
                 jsr getoff
                 sty dosofh
                 sta dosofh+1
                 lda #$04
                 bra dlimx1                               ; set aux status


recover          lda #$40
                 tsb dosflags                             ; set 'recover' bit
                 +lbne snerr                              ; if repeated parameter
                 jsr chrget                               ; continue
                 bra delim2


name1            lda #1                                   ; name1 allowed only once
                 jsr newnam                               ; do name parsing
                 sta dosf1l

                 ldy #0
l236_1           jsr indin1_ram1
                 sta savram,y                             ; copy name into buffer
                 iny
                 cpy dosf1l
                 bcc l236_1                               ; ...copy all of it
                 lda #1                                   ; set name1 flag


del1             tsb parsts

delim1           jsr chrgot
delim2           bne nxxx
                 +lbra done                               ; <cr>/<> => done


next6            cmp #on_token
                 +lbeq on1
                 cmp #to_token                            ; "to" token
                 +lbne snerr                              ; syntax error

;  If "to" is not followed by an offset param, then do file2 params.
;  Otherwise, do high offset and continue with file0 options.

                 jsr chrget
                 cmp #'P'
                 bne pars22
                 beq doffh


nxxx             cmp #','
                 bne next6
                 jsr chrget
                 +lbra parse1


parse2           jsr chrget
pars22           cmp #'D'
                 beq l237_1
                 cmp #on_token                            ; "on" token
                 beq on2
                 cmp #'U'
                 beq unit2
                 cmp #'"'
                 beq name2
                 cmp #'('
                 beq name2

l237_1           lda #$20
                 jsr prmrpt                               ; check for repeated parameter
                 jsr gtbytc                               ; getval
                 cpx #10
                 +lbcs fcerr                              ; illegal drive #  [allow 0: to 9: ????]
                 stx dosds2
                 lda #$20
                 bra del2

on2              jsr on
                 bra del2


unit2            jsr unit                                 ; do unit# parsing
                 bra del2                                 ; always

name2            lda #2                                   ; name2 allowed only once
                 jsr newnam
                 sta dosf2l
                 stx dosf2a
                 sty dosf2a+1

                 lda #2                                   ; set filename2 flag &
del2             tsb parsts                               ; set flag in status
                 jsr chrgot
                 +lbeq done                               ; done on <cr>/<>
                 cmp #','
                 beq parse2
                 cmp #on_token                            ; "on" token
                 beq on2
                 cmp #'U'
                 beq unit2
                 +lbra snerr


on               jsr chrget
                 cmp #'B'
                 beq dbank
                 cmp #'U'
                 +lbne snerr


unit             jsr gtbytc                               ; getval
                 cpx #31
                 bcs err_ild                              ; error if >30
                 cpx #1                                   ; drive 1 = use system default drive  [910221]
                 bne l238_1
                 ldx _default_drive
                 bra l238_2
l238_1           cpx #4
                 bcc err_ild                              ; error if <4
l238_2           stx dosfa
                 lda #$08                                 ; set parser's unit flag
                 rts


dbank            lda #$01                                 ; repeated param?
                 jsr prxrpt
                 jsr gtbytc                               ; getval
; cpx #16  ;bank too large?
; bcs fcerr ;illegal qty
                 stx dosbnk
                 lda #$01
                 tsb parstx                               ; set bnk bit in aux status
                 lda #0                                   ; .a=std status wrd, no bits to set
                 rts


newnam
                 pha                                      ; save nam1,2 for subdir check later  [901115]
                 jsr prmrpt                               ; check for repeated parameter
                 jsr frmstr
                 tax                                      ; save length of string
                 beq err_mfn                              ; if length = 0
                 ldy #0
                 jsr indin1_ram1
                 cmp #'@'                                 ; Replace file convention?
                 bne l239_1                               ; no
                 lda #$80                                 ; yes- check for repeated param
                 jsr prmrpt
                 smb7 parsts                              ; set "@" flag
                 dex                                      ; decrement length
                 inw index1                               ; increment past "@"
                 bra lenchk

l239_1           cmp #'/'                                 ; Subdirectory (partition)?   [901115]
                 bne lenchk                               ; no
                 pla                                      ; yes- recall nam1 or nam2
                 tsb dosflags                             ; set appropriate '/' flag (.a=1 or 2)
                 dex                                      ; decrement length
                 inw index1                               ; increment past "@"
                 !text $89


lenchk           pla                                      ; [901115]
                 txa                                      ; Check filename length
                 beq err_mfn                              ; too small, missing filename  [901115]
; cmp #17  ;  ???? (problem if name has ',P')
; bcs errlen ; too long
                 ldx index1
                 ldy index1+1                             ; ok- return pointer to filename
                 rts


err_mfn          ldx #err_missing_fname
                 !text $2c

err_ild          ldx #err_illegal_device
                 !text $2c

errlen           ldx #errls                               ; string or filename too long
                 +lbra error



; Get next value routine (use gtbytc instead!)

;getval jsr chrget ;get nxt chr
; beq snerr ;if end of statement
; bra getbyt ;       [911024]

; bcc getbyt ;can be numeric, go evaluate it  why this crap ????
; jsr chkopn ;or a "("
; jsr getbyt ;anything else is an error
; bra chkcls ;need closing ")"



; Get next 2-byte expression.  Exit: .a,.y (high,low) value

getoff           jsr chrget                               ; get nxt chr
                 +lbeq snerr                              ; if end of statement
                 +lbcc getwrd                             ; can be num. const, go evaluate it
                 jsr chkopn                               ; or a "("
                 jsr getwrd                               ; expr
                 jsr chkcls                               ; need closing ")"
                 ldy poker
                 lda poker+1
                 rts



; Prmrpt checks for a repeated parameter.
;
; Entry: .a contains parsts flag to check


prmrpt           and parsts                               ; compare mask with status
                 +lbne snerr                              ; error if bit previously set
                 rts



; Prxrpt checks for a repeated parameter.
;
; Entry: .a contains parstx flag to check


prxrpt           and parstx                               ; and with parstx
                 +lbne snerr                              ; if bit previously set
                 rts

;.end



; CBM-2001 BASIC-4 disk verb processors.  -- rsr 7-24-79 --
;
; Token Table Definitions

xsca             =$c2                                     ; send dossca
xid              =$d0                                     ; sends disk id
xd1              =$d1                                     ; sends dosds1
xd2              =$d2                                     ; sends dosds2
xrec             =$e0                                     ; sends S for SEQ, or dosrcl
xwrt             =$e1                                     ; sends W or L
xrcl             =$e2                                     ; send low ((poker))
xfat             =$f0                                     ; sends "@" or "/" if specified
xfn1             =$f1                                     ; sends filename1
xfn2             =$f2                                     ; sends filename2


; Tabld - used to build disk command strings

tabld
finit            = $ff                                    ; Dclear
                 !text "I",xd1

fdir             = *-tabld-1                              ; Dir
                 !text "$",xd1,":",xfn1

fdirr            = *-tabld-1                              ; Recover Dir  [901024]
                 !text "_",xd1,":",xfn1                   ; ('_'=CBM backarrow, $5F)

fopn             = *-tabld-1                              ; Dopen
                 !text xfat,xd1,":",xfn1,",",xwrt,",",xrec

fopnseq          = *-tabld-1                              ; Type   [900801]
                 !text xfat,xd1,":",xfn1,",S"

fsavseq          = *-tabld-1                              ; EDIT_SAVE  [910620]
                 !text xfat,xd1,":",xfn1,",S,W"

fconc            = *-tabld-1                              ; Concat
                 !text "C",xd2,":",xfn2,"=",xd2,":",xfn2,","

fapn             = *-tabld-1                              ; Append
                 !text xd1,":",xfn1,",A"

fhed             = *-tabld-1                              ; Header
                 !text "N",xd1,":",xfn1,",",xid

fcoll            = *-tabld-1                              ; Collect
                 !text "V",xd1

fbak             = *-tabld-1                              ; Backup
                 !text "D",xd2,"=",xd1

fcopy            = *-tabld-1                              ; Copy
                 !text "C",xd2,":",xfn2,"=",xd1,":",xfn1

fren             = *-tabld-1                              ; Rename
                 !text "R",xd1,":",xfn2,"=",xd1,":",xfn1

fscr             = *-tabld-1                              ; Scratch
                 !text "S",xd1,":",xfn1

frscr            = *-tabld-1                              ; Recover Del  [901024]
                 !text "F-R",xd1,":",xfn1

fdisk            = *-tabld-1                              ; Disk command  [910123]
                 !text xfat,xfn1                          ; pass '/' [911030]

frec             = *-tabld-1                              ; Record
                 !text "P",xsca,xrcl,xrec

