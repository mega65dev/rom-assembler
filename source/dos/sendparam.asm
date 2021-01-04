; Send parameters to device
;
; Entry: .a = number of bytes in format
;  .y = pointer to TABLD entry

sendp            sta xcnt                                 ; save number of string bytes
                 phy
                 jsr Clear_DS                             ; clear old status

                 ldx #0
sdp1             pla
                 dec xcnt
                 bmi tranr
                 tay
                 iny                                      ; move down table
                 phy
                 lda tabld,y                              ; get next entry
                 bpl sdp5                                 ; if not escape code
                 cmp #xsca                                ; if not secondary address
                 beq rsca
                 cmp #xid
                 beq rid                                  ; if disk id
                 cmp #xrcl
                 +lbeq rdcn                               ; if record number
                 cmp #xwrt
                 beq rwrt                                 ; if W or L
                 cmp #xfat
                 beq rfat                                 ; if "@" symbol request
                 cmp #xfn1
                 +lbeq rsfn                               ; if filename 1
                 cmp #xfn2
                 +lbeq rdfn                               ; if filename 2
                 cmp #xrec
                 bne sdp2                                 ; if not record type
                 lda dosrcl                               ; get rec #
                 cmp #1                                   ; kludge to allow DOPEN#lf,"relfile",L  [911024]
                 bne sdp5                                 ; (note RECORD byte 0 = byte 1 anyhow)
                 dec
                 bra sdp5                                 ; always branch

sdp2             cmp #xd1
                 bne sdp3                                 ; if not drive 1
                 lda dosds1
                 bra sdp4                                 ; always branch

sdp3             cmp #xd2
                 bne sdp1                                 ; if not drive 2, continue
                 lda dosds2
sdp4             ora #'0'                                 ; change # to PETSCII

sdp5             sta dosstr,x                             ; else into buffer
                 inx
                 bra sdp1                                 ; always


tranr            txa                                      ; length to a
                 pha
                 ldx #<dosstr                             ; set filename
                 ldy #>dosstr
                 jsr _setnam

                 lda dosla                                ; set channel
                 ldx dosfa
                 ldy dossa
                 jsr _setlfs

                 pla
                 rts




rsca             lda dossa_temp                           ; secondary address (record)
                 bra sdp5                                 ; always


rfat             bbr7 parsts,l240_1                       ; if "@" not encountered
                 lda #'@'
                 bra sdp5                                 ; always

l240_1           lda dosflags
                 lsr
                 bcc sdp1                                 ; if "/" not encountered
                 lda #'/'
                 bra sdp5


; ID subroutine

rid              lda dosdid                               ; include id
                 sta dosstr,x
                 inx
                 lda dosdid+1
                 bra sdp5                                 ; always


rwrt             lda dosrcl                               ; check for L or W
                 beq l241_1                               ; zero then write
                 lda #'L'
                 bra sdp5                                 ; always

l241_1           lda #'S'                                 ; send W,S
                 sta dosrcl
                 lda #'W'
                 bra sdp5                                 ; always



; Move record number

rdcn             lda poker
                 sta dosstr,x
                 lda poker+1
                 inx
                 bra sdp5                                 ; always


; Move file names

rsfn             ldy dosf1l                               ; file name 1: get length
                 beq rdrt0                                ; if null string

                 ldy #0                                   ; move name to dosstr
l242_1           lda savram,y
                 sta dosstr,x
                 inx
                 iny
                 cpy dosf1l
                 bne l242_1                               ; if move not complete
                 bra rdrt1                                ; always


rdfn             lda dosf2a
                 sta index1
                 lda dosf2a+1
                 sta index1+1
                 ldy dosf2l
                 beq rdrt0                                ; if null string

                 ldy #0                                   ; move name to dosstr
l243_1           jsr indin1_ram1
                 sta dosstr,x
                 inx
                 iny
                 cpy dosf2l
                 bne l243_1                               ; if move not complete
                 !text $89                                ; hop

rdrt0            dex                                      ; case cdd=sd
rdrt1            +lbra sdp1                               ; get next symbol


; Syntax checker DOS write

chk1             and #$e6                                 ; for HEADER, DLOAD, SCRATCH, TYPE, LIST
                 +lbne snerr

chk2             lda parsts                               ; for DSAVE
                 and #1
                 cmp #1                                   ; check required parameters
                 +lbne snerr                              ; error if 1 missing
                 lda parsts                               ; reload for return
                 rts


chk3             and #$e7                                 ; for COLLECT
                 +lbne snerr                              ; check optional parameters
                 rts


chk4             and #$c4                                 ; for COPY, CONCAT
                 +lbne snerr                              ; check optional parameters
                 lda parsts
chk5             and #3                                   ; for RENAME
                 cmp #3                                   ; check required parameters
                 +lbne snerr
                 lda parsts                               ; reload for return
                 rts


chk6             and #5                                   ; for APPEND, DOPEN
                 cmp #5                                   ; check required parameters
                 +lbne snerr
                 lda parsts                               ; reload for rts
                 rts

;.end



; Allocate DS$ if nesessary, but use old DS$ string otherwise
; Called by DS$ and DS

Check_DS                                                  ; chkds.
                 lda dsdesc
                 beq Read_DS_1                            ; branch if DS$ is not in memory
                 rts                                      ; else return & use old one


; Allocate DS$ if necessary & Read DOS error channel

Read_DS                                                   ; errchl.
                 lda dsdesc                               ; has DS$ space been allocated?
                 bne Read_DS_2                            ; yes

Read_DS_1
                 lda #40                                  ; no- get 40 char string
                 sta dsdesc
                 jsr getspa                               ; allocate space for DS$
                 stx dsdesc+1                             ; low address of string
                 sty dsdesc+2                             ; high   "    "    "
                 ldx #dsdesc+1                            ; set up string back pointer to dsdesc
                 ldy #40
                 lda #<dsdesc
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y
                 iny
                 lda #>dsdesc
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y

Read_DS_2
                 ldx dosfa                                ; fa
                 cpx #2
                 bcs l244_1                               ; if =0 or 1 use default  [910429]
                 ldx _default_drive                       ; (was dosffn)   [900710]
                 stx dosfa
l244_1           lda #doslfn                              ; la (reserved la)
                 ldy #$6f                                 ; sa (command channel)
                 jsr _setlfs
                 lda #0                                   ; no name (so no setbank)
                 jsr _setnam
                 jsr _open                                ; get command channel
                 ldx #doslfn
                 jsr _chkin
                 bcs l244_4                               ; a problem (file already open??)

                 ldy #$ff
l244_2           iny                                      ; read disk error message
                 jsr _basin
                 cmp #cr
                 beq l244_3                               ; if eol
                 ldx #dsdesc+1
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y copy to DS$
                 cpy #40
                 bcc l244_2                               ; loop unless too long

l244_3           lda #0                                   ; errend.
                 ldx #dsdesc+1                            ; terminate DS$ with a null
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y

                 jsr _clrch                               ; shut down command channel
                 lda #doslfn
                 sec                                      ; not a real close
                 jmp _close                               ; close it and rts

l244_4           pha                                      ; errbad.
                 jsr l244_3
                 jsr Clear_DS                             ; flag 'no DS available'
                 plx                                      ; get error
                 +lbra error


; Clear_DS subroutine - forget current DS$ message, if any
;

Clear_DS                                                  ; oldclr.
                 lda dsdesc                               ; check for allocation
                 beq l245_1                               ; branch if not allocated

                 phy                                      ; mark current DS$ string as garbage
                 phx
; lda #40   ;   standard DS$ allocation
                 tay
                 ldx #dsdesc+1
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y length of garbage
                 iny
                 lda #$ff
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y garbage flagged
                 inc
                 sta dsdesc                               ; (0)    kill DS$
                 plx
                 ply

l245_1           rts


; Read DOS error message, but don't care what it is.  Want to stop disk LED blink.
;

Suck_DS
                 ldx dosfa                                ; fa
                 lda #doslfn                              ; la (reserved la)
                 ldy #$6f                                 ; sa (command channel)
                 jsr _setlfs
                 lda #0                                   ; no name (so no setbank)
                 jsr _setnam
                 jsr _open                                ; get command channel
                 ldx #doslfn
                 jsr _chkin
                 bcs l246_2                               ; skip input if problem

l246_1           jsr _basin                               ; read disk error message
                 cmp #cr
                 bne l246_1                               ; loop until eol

l246_2           jsr _clrch                               ; shut down command channel
                 lda #doslfn
                 sec                                      ; not a real close
                 jmp _close                               ; close it


; R-U-sure subroutine

are_you_sure

                 bbs7 runmod,response_fake                ; branch if not direct mode

                 jsr _primm                               ; else prompt user for y/n answer
                 !text "ARE YOU SURE? ", 0


response_get
                 jsr _clrch                               ; clear channel for basin
                 jsr _basin                               ; next char
                 pha                                      ; save first char of reply

l247_1           cmp #cr                                  ; eat chars until end of line
                 beq l247_2                               ; if cr received, exit
                 jsr _basin
                 bne l247_1                               ; continue to ignore

l247_2           jsr _bsout                               ; new line     [910212] FAB
                 pla
                 cmp #'Y'                                 ; z set means ans=y.....
                 rts


response_fake
                 lda #0                                   ; ...or not in direct mode
                 rts

;.end



;*****************************************************************
;  OPTWRD - get an optional, unsigned 2-byte value in y,a.
;
;      case 1 : pointer at end of line:
;   return a=y=0, clear c to flag 'default'
;      case 2 : pointer is at comma, next non-blank is also a comma:
;   return a=y=0, clear c to flag 'default'
;      case 3 : pointer is at comma, next non-blank is not a comma:
;   get word in y,a, set c to flag 'non-default'
;*****************************************************************

optwrd           jsr chrgot
                 beq l248_1
                 jsr chkcom
                 cmp #','
                 beq l248_1
                 jsr getwrd
                 sec
                 rts

l248_1           lda #0
                 tay

optw99           clc
                 rts


comsad           jsr chkcom                               ; get a comma & signed 2-byte arg in y,a   [910307]
                 +lbra sadwrd



optsad           jsr chrgot                               ; get a comma & optional, signed 2-byte arg in y,a [910307]
                 beq l249_1                               ; eol, therefore this arg is not specified
                 jsr chkcom                               ; eat comma
                 cmp #','                                 ; is next a comma too?
                 beq l249_1                               ; yes, therefore this arg is not specified
                 jsr sadwrd                               ; get signed word
                 sec
                 rts

l249_1           lda #0                                   ; default optional arg to zero
                 tay
                 clc
                 rts


;*****************************************************************
;  OPTBYT - get an optional 1 byte value in x.
;
;  Enter with default value in x.
;
;      case 1 : pointer at end of line:
;   return default x.
;      case 2 : pointer is at comma, next non-blank is also a comma:
;   return default x.
;      case 3 : pointer is at comma, next non-blank is not a comma:
;   get byte in x.
;*****************************************************************

optzer           ldx #0                                   ; optional byte, with default=0

optbyt           jsr chrgot
                 beq optw99                               ; EOL: clc/rts
                 jsr chkcom
                 cmp #','
                 beq optw99                               ; Comma: clc/rts
                 jsr getbyt
                 sec
                 rts


prtdec
                 phy
                 phx
                 tax                                      ; prints decimal value of chr in .a
                 lda #0
                 jsr linprt
                 plx
                 ply
                 rts



retpat                                                    ; f.bowen
                 dey                                      ; [910828]
                 lda (fndpnt),y                           ; restore pointers
                 sta txtptr+1
                 dey
                 lda (fndpnt),y
                 sta txtptr
                 dey
                 lda (fndpnt),y

                 sta curlin+1                             ; fixes a problem when RETURNing to a GOSUB in direct mode
                 dey                                      ; or LOOPing to a DO in direct mode. 'curlin+1' must not be
                 tax                                      ; restored to $ff without also resetting 'runmod'
                 inx
                 bne l250_1                               ; branch if GOSUB or DO was from a program
                 lda #%11000000
                 trb runmod                               ; else force return to direct mode

l250_1           lda (fndpnt),y
                 sta curlin
                 rts


vbits            !text $01,$02,$04,$01,$02,$04            ; for stereo filter, volume bit setting
sbits            !text $01,$02,$04,$08,$10,$20,$40,$80
rbits            !text $80,$40,$20,$10,$08,$04,$02,$01

;.end