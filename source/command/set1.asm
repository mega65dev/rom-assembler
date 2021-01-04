; STOP, STOP KEY, and END handlers
;

is_stop_key_down
                jsr _stop                               ; test stop key
                bne do_rts                              ; not down, exit

; ldy trapno+1  ;test if trap on????   removed [910925]
; iny
; beq stop_1  ;no, do a normal stop


break_exit                                              ; STOP KEY:     [910104]
l16_1           jsr _stop                               ; wait for the user to release the key
                beq l16_1
                ldx #erbrk                              ; take the vector thru error to ready
                +lbra error



stop            bcs stopc                               ; STOP: .c=1

end             clc                                     ; END: .c=0
stopc           +lbne snerr                             ; error if args present   [910410]

stop_1          bbr7 runmod,l17_1                       ; branch if direct mode
                jsr tto                                 ; transfer txtptr to oldtxt
                lda curlin
                ldy curlin+1
                sta oldlin
                sty oldlin+1
l17_1           pla                                     ; .diris
                pla
                +lbcc ready                             ; say 'ready' if END, say 'break' if STOP


break           jsr release_channels                    ; make sure we're in text mode????  [910909]
                jsr RestoreTextScreen
                jsr highlight_text                      ; ????      [910624]
                jsr _primm
                !text cr,"BREAK",0
                +lbra errfin                            ; exit via 'in line #'

do_rts          rts

;.end