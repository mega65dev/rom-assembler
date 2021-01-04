


;*****************************************************************
;*   LOADIFF "filename" [,U#] [,D#]
;*
;* ???? requires SCREEN already opened   910402 FAB
;*****************************************************************

loadiff
                 jsr CheckGraphicMode
                 lda #$e6                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; (like dopen:  0 0 0 *  * 0 0 1 )
                 jsr chk1                                 ; check parameters
                 lda #0
                 sta dossa                                ; setup as dload would (0 = load channel)
                 jsr find_la                              ; find an available la to use (cannot use reserved one)
                 ldy #fopn
                 ldx #4
                 jsr open_file                            ; open the file
                 bcs l279_1                               ; exit if error

                 ldx dosla
; stx GKI__parm1
                 jsr _chkin                               ; get input channel
l279_1           +lbcs list_err                           ; exit if error

                 jsr ($802a)                              ; Load it

exit_GKI_disk_op
                 php                                      ; preserve completion status
                 pha
                 jsr _clrch
                 lda dosla
                 jsr close_out                            ; close channel

                 jsr is_stop_key_down                     ; weed out BREAK error
                 plx
                 plp
                 +lbcs error                              ; must be I/O or file data error
                 rts                                      ; load was successful
