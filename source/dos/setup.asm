


; These routines take tokens and values after the following BASIC keywords:
;
;  DOPEN,   DCLOSE,  APPEND,  CONCAT, RECORD
;  DLOAD,   DSAVE,   DVERIFY, BLOAD,  BSAVE
;  FORMAT,  COLLECT, BACKUP,  COPY
;  CATALOG, RENAME,  SCRATCH, DCLEAR
;
; It then parses the following line and finds syntax errors, checks for values
; out of range, and sets variables in the zero-page to be passed to the disk
; message generator.


directory                                                 ; display disk directory (catalog)
                 jsr chrgot                               ; get current chr
                 cmp #esc_command_token                   ; eat dirECTORY kludge if it's there
                 bne l216_1
                 jsr chrget                               ; (esc token + another)
                 cmp #ectory_token
                 +lbne snerr
                 jsr chrget                               ; yes- get next good char

l216_1           jsr dospar                               ; parse the line
                 lda parsts                               ; check options
                 and #$e6
                 +lbne snerr

                 ldy #fdir                                ; table offset for directory
                 bit dosflags                             ; want recoverable files? [901024]
                 bvc l216_2                               ; no
                 ldy #fdirr                               ; yes
l216_2           ldx #1                                   ; just $
                 lda parsts                               ; check for default
                 and #$11                                 ; no drive?
                 beq l216_4
                 lsr
                 bcc l216_3                               ; just drive
                 inx                                      ; drive and filename
                 inx
l216_3           inx
l216_4           txa                                      ; a now has length
                 jsr sendp                                ; build

                 ldx #sys_bank                            ; set banks????  fname in system space, bank0 [910620]
                 txa                                      ; (load bank is don't care- we're not actually loading)
                 jsr _setbank

                 ldy #$60                                 ; sa, load floppy
                 ldx dosfa
                 lda #doslfn                              ; lfn
                 jsr _setlfs                              ; set file parameters
                 jsr _open                                ; open it...
                 bcc l216_5                               ; ...ok
                 pha
                 jsr dcat11                               ; ...error, shut down and report
                 plx
                 sec
                 +lbra error

l216_5           lda channl                               ; determine DIR vs LDIR
                 bne ldir                                 ; if output channel not default (screen)
; use LDIR

; Get length in blocks

dir              ldx #doslfn
                 jsr _chkin
                 bcs dcat11                               ; if problem??
                 jsr _readst                              ; check status
                 bne dcat11                               ; exit if bad status
                 jsr crdo                                 ; output new line
                 ldy #3                                   ; loop counter

dcat3            sty t3                                   ; save counter

l217_1           jsr _basin                               ; get char
                 sta t4
                 jsr _basin                               ; get char
                 sta t4+1
                 jsr _readst                              ; check status
                 bne dcat11                               ; exit if eof or bad status
                 dec t3
                 bne l217_1                               ; if not done

; Output blocks number

                 ldx t4
                 lda t4+1
                 jsr linprt                               ; output number
                 lda #' '
                 jsr _bsout                               ; output a space

; Get name & output it

dcat4            jsr _readst                              ; get status
                 bne dcat10                               ; if bad status
                 jsr _basin                               ; get char
                 beq dcat5                                ; if eol
                 jsr _bsout                               ; echo char
                 bra dcat4                                ; continue to process name until eol or err

; Here on end of name

dcat5            jsr crdo                                 ; output new line
                 jsr _stop                                ; check stop key
                 beq dcat11                               ; exit if stop request

; Process next

                 ldy #2                                   ; perform 2 times
                 bra dcat3


; Exit directory

dcat10           jsr crdo                                 ; flush current line
dcat11           jsr release_channels                     ; release cmd channel, restore terminal
                 lda #doslfn
                 +lbra close_out                          ; [900725]
