; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      readyerror.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


bad_command
                ldx     #err_bad_command                ; unimplemented command
                !text $2c

userr           ldx     #errus                          ; undefined statement
                !text $2c

omerr           ldx     #errom                          ; out of memory
                !text $2c

doverr          ldx     #errdvo                         ; division by zero
                !text $2c

overr           ldx     #errov                          ; overflow
                !text $2c

snerr           ldx     #errsn                          ; syntax error
                !text $2c

ready           ldx     #$80                            ; no error

error           jmp     (ierror)

nerror          txa
                +lbmi   ready_1                         ; ...branch if no error (from 'ready')
                stx     errnum                          ; save error # for 'er'
                bbr7    runmod,errisd                   ; branch if direct mode- always display error

                ldy     #1                              ; copy curlin to errlin, oldtxt to errtxt
l21_1           lda     curlin,y
                sta     errlin,y                        ; line# where error occurred
                lda     oldtxt,y
                sta     errtxt,y                        ; statement where error occured
                dey
                bpl     l21_1
                inc     errtxt                          ; point to a token, not ':' for HELP
                bne     l21_2
                inc     errtxt+1

l21_2           ldy     trapno+1                        ; is trap set?
                cpy     #$ff
                beq     errisd                          ; no
                sty     linnum+1
                sty     tmptrp                          ; save until a resume is executed
                ldy     trapno
                sty     linnum

                ldx     #$ff
                stx     trapno+1                        ; flag no more traps
                ldx     #tempst                         ; clear any accumulated string temps
                stx     temppt
                ldx     oldstk
                txs
                jsr     luk4it
                +lbra   newstt



; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
