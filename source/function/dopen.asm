; DOPEN dfn(,t(,r))

dopen            lda #$22                                 ; set error flag
                 jsr dosprs                               ; parse the line
                 jsr chk6                                 ; check required parameters
                 jsr find_sa                              ; find unused secondary address
                 ldy #fopn                                ; fcb format pointer
                 ldx #4                                   ; normal length
                 bbr6 parsts,open_it                      ; relative record? branch if not relative
                 ldx #8                                   ; random access length
                 bra open_it                              ; [910925]

;l220_1 jsr open_file  ;open it
; bra exit_disk_op ;report any DOS errors, & return to main [910404]



; APPEND

append           lda #$e2                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk6                                 ; check required parameters
                 jsr find_sa                              ; find secondary address
                 ldy #fapn                                ; tabld index
                 ldx #5                                   ; length
open_it
                 jsr open_file                            ; open it
                 +lbra exit_disk_op                       ; report any DOS errors, & return to main [910404]



open_file                                                 ; dop2.
                 txa                                      ; set length into a
                 jsr sendp
                 jsr _clrch
                 ldx #sys_bank                            ; fname is in system space, bank0  [910620]
                 txa                                      ; (load bank not req'd)????
                 jsr _setbank
                 jmp _open


; Find an available secondary address

find_sa
                 ldy #$61                                 ; 2-14 possible

l220_1           iny
                 cpy #$6f
                 beq too_many_files                       ; if none available error
                 jsr _lkupsa                              ; kernel will lookup this sa in its tables
                 bcc l220_1                               ; if used keep looking
                 sty dossa                                ; save secondary address
                 rts                                      ; return .y = sa



; Find an available logical address

find_la
                 lda #0                                   ; 1-127 possible

l221_1           inc
                 bmi too_many_files                       ; if none available error
                 jsr _lkupla                              ; kernel will lookup this la in its tables
                 bcc l221_1                               ; if used keep looking
                 sta dosla                                ; save logical address
                 rts                                      ; return .a = la




too_many_files
                 ldx #errtmf                              ; too many files open
                 +lbra error


; Close disk file

dclose           lda #$f3                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr Clear_DS
                 bbr2 parsts,dclall                       ; any la given?  branch if not
                 lda dosla
                 +lbra close_out

dclall           lda dosfa                                ; get disk #
; jsr put_io_in_map
                 jmp _close_all                           ; close all channels



; DSAVE dfn

dsave            bbr4 runmod,l222_1                       ; PROGRAM or EDIT mode?    [910620]
                 +lbra edit_save                          ; edit

l222_1           lda #$66                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk2                                 ; check required parameters
                 ldy #fopn                                ; table offset
                 lda #4                                   ; ..length,
                 jsr sendp

                 lda text_bank                            ; default to text bank set up banks???? [910620]
                 ldx #sys_bank                            ; fname is in system space, bank0
                 jsr _setbank
                 +lbra savenp


; DVERIFY

dverify          lda #1                                   ; flag 'verify'
                 !text $2c



; DLOAD dfn

dload            lda #0
                 sta verck                                ; set load flag (for verify check later)

                 bbr4 runmod,l223_1                       ; PROGRAM or EDIT mode?    [910620]
                 +lbra edit_load                          ; edit

l223_1           lda #$e6                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk2                                 ; check required parameters


dload_boot                                                ; <<<<<<<<<<<<<<<<<< entry for BOOT'AUTOBOOT.C65'
                 lda #0
                 sta dossa                                ; set relocate flag
                 ldy #fopn                                ; table offset
                 lda #4                                   ; ..length
                 jsr sendp

                 lda text_bank                            ; set up banks ???? want text_bank ????  [910620]
                 ldx #sys_bank                            ; fname is in system space, bank0
                 jsr _setbank

                 +lbra cld10                              ; finish load, using 'LOAD' code.


; BSAVE

bsave            lda #$66                                 ; std error flag
                 ldx #$f8                                 ; auxiliary error flag (allow bank, start & end address)
                 jsr dosprx                               ; parse options
                 jsr chk2                                 ; check required parameters

                 lda parstx                               ; check for starting & ending addresses
                 and #6
                 cmp #6
                 +lbne snerr                              ; ..if not present, syntax error

                 lda dosofh+1                             ; check that ea>sa
                 cmp dosofl+1
                 +lbcc fcerr                              ; ...error
                 bne l224_1
                 lda dosofh
                 cmp dosofl
                 +lbcc fcerr                              ; ...error
                 +lbeq fcerr

l224_1           ldy #fopn                                ; table offset
                 lda #4                                   ; ..length
                 jsr sendp

                 lda dosbnk                               ; get requested bank
                 ldx #sys_bank                            ; ..and name will be in system bank
                 jsr _setbank                             ; ..and go set up bank

                 ldx dosofl                               ; start addr
                 ldy dosofl+1
                 lda #highds                              ; ..and a pointer to start address
                 stx highds
                 sty highds+1
                 ldx dosofh                               ; end addr
                 ldy dosofh+1
                 +lbra savenb



; DVERIFY

bverify          lda #1                                   ; flag 'verify'
                 !text $2c

