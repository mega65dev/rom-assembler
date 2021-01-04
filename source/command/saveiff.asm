
;*****************************************************************
;*
;*   SAVEIFF "[@]filename" [,U#] [,D#]      [910930] FAB
;*
;*****************************************************************

saveiff
                 jsr CheckGraphicMode
                 lda #$66                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk2                                 ; check required parameters
                 lda #1
                 sta dossa                                ; setup as dsave would (1 = save channel)
                 jsr find_la                              ; find an available la to use (cannot use reserved one)
                 ldy #fopn
                 ldx #4
                 jsr open_file                            ; open the file
                 bcs l280_1                               ; exit if error

                 ldx dosla
; stx GKI__parm1
                 jsr _chkout                              ; get output channel
l280_1           +lbcs list_err                           ; exit if error

                 jsr ($803a)                              ; Save it
                 bra exit_GKI_disk_op

; php   ;preserve completion status
; pha
; jsr _clrch
; lda dosla
; jsr close_out  ;close channel
;
; jsr is_stop_key_down ; weed out BREAK error
; plx
; plp
; bcs error  ; must be I/O or file data error
; rts   ; load was successful

;.end