


list             rmb7 helper                              ; clear 'help' flag for p1line

; Determine which form of LIST we have...

                 +lbeq list_memory                        ; branch if terminator (no parameter)
                 +lbcc list_memory                        ; branch if a number (assume range parameter)
                 cmp #minus_token
                 +lbeq list_memory                        ; branch if a dash (assume range parameter)


; LIST command is of the form  LIST filename [,U#] [,D#]

list_file
                 lda #$e6                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; (like dopen:  0 0 0 *  * 0 0 1 )
                 jsr chk1                                 ; check parameters
                 lda #0
                 sta dossa                                ; setup as dload would (0 = load channel)
                 jsr find_la                              ; find an available la to use (cannot use reserved one)
                 ldy #fopn
                 ldx #4
                 jsr open_file                            ; open the file
                 bcs list_err                             ; exit if error

                 ldx dosla
                 jsr _chkin                               ; get input channel
                 bcs list_err                             ; exit if bad??
                 jsr _basin                               ; waste 'load address'
                 jsr _basin

l35_1            jsr _basin                               ; get link bytes
                 sta dosstr
                 jsr _basin
                 sta dosstr+1
                 ora dosstr
                 beq list_exit                            ; done if null pointer
                 jsr _readst
                 bne list_exit                            ; done if eof or bad status
; ???? assumes serial bus
                 lda #>dosstr                             ; point p1line's pointer at our line buffer
                 ldx #<dosstr
                 sta lowtr+1
                 stx lowtr

                 ldx #2
                 jsr _basin                               ; read line into buffer
                 sta dosstr,x
                 inx
                 jsr _basin                               ; 2-byte line #
                 sta dosstr,x
                 inx
l35_2            cpx #255                                 ; check buffer (buflen????)
                 +lbcs errlen                             ; 'too long' error
                 jsr _basin
                 sta dosstr,x
                 inx
                 tay                                      ; save char
                 jsr _readst                              ; check channel status (serial bus????)
                 bne list_exit                            ; exit if eof or error
                 jsr _stop
                 beq list_exit                            ; exit if stop key down
                 tya
                 bne l35_2                                ; loop until eol

                 jsr dcato                                ; get output channel
                 jsr crdo                                 ; start new line
                 ldx dosstr+2                             ; get line #
                 lda dosstr+3
                 jsr p1line                               ; print line #, space, and the line of code
                 jsr _clrch
                 ldx dosla
                 jsr _chkin                               ; get input channel
                 bcc l35_1                                ; [900730]

list_exit
                 jsr dcato                                ; flush last line with a <cr>
                 jsr crdo                                 ; flush current line
                 clc                                      ; no errors    [910404]
list_err
                 php                                      ; save error status   [910404]
                 pha
                 jsr release_channels                     ; release cmd channel, restore terminal
                 lda dosla
; bra close_out  ;    removed [900725]
                 clc                                      ; a real close   new [910404]
                 jsr _close
                 pla                                      ; pop error status, if any
                 plp
                 +lbra exit_disk_op


; LIST command is of the form  LIST [range]

list_memory
                 jsr range                                ; set up line range

l36_1            ldy #1
                 jsr indlow                               ; get ms byte of line to list's pointer
                 bne l36_2                                ; ok if not zero, but..
                 dey
                 jsr indlow
                 +lbeq crdo                               ; ..if ls byte is also zero, we're done

l36_2            jsr is_stop_key_down
                 jsr crdo                                 ; new line
                 ldy #2
                 jsr indlow                               ; get ms byte of line number
                 tax
                 iny
                 jsr indlow                               ; get ls byte

                 cmp linnum+1                             ; test if we are past the last line requested
                 bne l36_3
                 cpx linnum
                 beq l36_4
l36_3            +lbcs crdo                               ; next line is > last line requested, exit
l36_4            jsr p1line                               ; print line #, space, and the line of code
                 ldy #0                                   ; move 'pointer to next line' into (lowtr)
                 jsr indlow
                 tax
                 iny
                 jsr indlow
                 stx lowtr
                 sta lowtr+1
                 bra l36_1
