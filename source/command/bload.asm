
; BLOAD dfn

bload            lda #0                                   ; flag 'load'
                 sta verck                                ; eventually the 'load' routine will look here

                 lda #$e6                                 ; std error flag
                 ldx #$fc                                 ; aux error flag (allow bank & load address)
                 jsr dosprx                               ; parse options

bload_boot                                                ; <<<<<<<<<<<<<<<<<<<<<< entry for BOOT'filename'
                 jsr chk2                                 ; check required parameters
                 ldx dosofl                               ; get starting address high
                 ldy dosofl+1                             ; ..and lo
                 lda #0                                   ; assume x & y not both=ff (means real add., not def)
                 cpx #$ff
                 bne l225_1
                 cpy #$ff
                 bne l225_1
                 lda #$ff                                 ; use defaults
l225_1           sta dossa

                 ldy #fopn                                ; table offset
                 lda #4                                   ; ..length,
                 jsr sendp                                ; ...and go send parameters

                 lda dosbnk
                 ldx #sys_bank
                 jsr _setbank

                 lda verck                                ; flag LOAD or VERIFY
                 ldx dosofl                               ; get starting address high
                 ldy dosofl+1                             ; ..and lo (in case this isn't a 'default' load)
                 jsr _loadsp                              ; load it

; Any changes to the following code must be duplicated at:
;  load (load_file)
;  save (exit_disk_op)

                 php                                      ; save kernel load status (.c)
                 pha                                      ; save kernel error # (.a)
                 jsr _readst                              ; save I/O status byte
                 sta parsts
                 jsr print_dos_error                      ; report DOS problems
                 pla                                      ; restore error stuff
                 plp
                 bcc l225_4                               ; branch if no error (rts)
                 bbs7 runmod,l225_3                       ; branch if run mode (erexit)
                 cmp #errfnf                              ; is it 'file not found' catch-all?
                 bne l225_2                               ; no  (erexit)
                 sta errnum                               ; yes- save error # for 'er'
                 ora #$80                                 ; but no errdis
l225_2           sec
l225_3           +lbcs erexit                             ; exit if kernel problem (rts)

l225_4           lda verck                                ; load or verify operation?
                 +lbne verify_check                       ; verify

; jsr _readst  ;  read status
                 lda parsts                               ; load
                 and #%10111111                           ; EOI is okay, so mask it
                 +lbne load_error                         ; load error
                 clc
                 rts
