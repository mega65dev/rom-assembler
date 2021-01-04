
; LDIR  same as DIR, except it buffers each line to reduce
;       talker/listener turnaround time.  Even so, it is still
; unacceptably slow for normal screen output, which is
; why it was split out from the original DIRECTORY routine.
;

; Read block count

ldir
                 lda #$c0                                 ; serial bus kludge for open4,4:cmd4:dir ????
                 and $d609
                 trb $d609                                ; disable fast serial bus
                 sta sid_speed_flag                       ; but save enables so we can restore them

                 ldy #3                                   ; loop counter (3=skip fake load adr & link bytes)
l218_1           sty t3                                   ; save counter
                 ldx #doslfn
                 jsr _chkin
                 bcs ldir_end                             ; problem??

l218_2           jsr _readst                              ; check status
                 bne ldir_end                             ; exit if bad status
                 jsr _basin                               ; get block count
                 sta dosstr                               ; buffer it
                 jsr _basin
                 sta dosstr+1
                 dec t3
                 bne l218_2                               ; continue eating bytes until we have block count

; Read filename

                 ldx #1                                   ; buffer index-1
l218_3           inx
                 jsr _readst                              ; check status
                 bne ldir_end                             ; exit if eof or bad status
                 jsr _basin                               ; buffer next character
                 sta dosstr,x
                 bne l218_3                               ; loop until eol (null terminator)

; Print one line of directory

                 jsr dcato                                ; get output channel
                 ldx dosstr
                 lda dosstr+1
                 jsr linprt                               ; print blocks

                 lda #' '
                 jsr _bsout                               ; print space

                 ldx #2
l218_4           lda dosstr,x
                 beq l218_5
                 jsr _bsout                               ; print filename (null terminated)
                 inx
                 bne l218_4

l218_5           jsr crdo                                 ; print return
                 jsr _clrch
                 jsr _stop                                ; check stop key
                 beq ldir_end                             ; exit if stop request

; Continue with next line

                 ldy #2                                   ; set to skip fake link bytes
                 bra l218_1                               ; loop


ldir_end
                 lda sid_speed_flag                       ; serial bus kludge for open4,4:cmd4:dir ????
                 tsb $d609                                ; restore fast serial bus enables
                 bra dcat11



dcato            jsr _clrch
                 ldx channl                               ; restore output channel
                 beq l219_1                               ; branch if screen (default output)
                 jmp _chkout                              ; else get output channel

l219_1           rts

