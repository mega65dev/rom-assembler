; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      set.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;  SET  A multipurpose command initiator


C65__set
                cmp     #verify_token                   ; SET VERIFY <ON | OFF>  new [910429]
                +lbeq   verify_mode
                cmp     #def_token                      ; SET DEF unit
                bne     l275_1
                jsr     getdisknum_1
                stx     _default_drive
                stx     dosfa                           ; Make last DOS device = current device
                +lbra   Clear_DS



l275_1          jsr     chkesc                          ; Must be ESCape token
                cmp     #disk_token                     ; ok so far
                +lbne   bad_command                     ; unknown command




                jsr     getdisknum_1                    ; SET DISK # [<,|TO> #]
                stx     dosfa                           ; got current disk unit #

                jsr     chrgot                          ; check delimiter (comma, 'TO', or eos)
                +lbeq   Clear_DS                        ; eos- just change DOS' current drive [910417]
                cmp     #','                            ; not eos, must be comma or 'TO'
                beq     l275_2
                cmp     #to_token
                +lbne   snerr

l275_2          jsr     getdisknum_1                    ; skip delimiter
                stx     dosds2                          ; got new disk unit #



;  Open disk command channel & pass it 'renumber' command

                jsr     dclall                          ; Close any open files????

                ldx     #6-1
l275_3          lda     disk_renum_cmd,x                ; move command to RAM, setup for open
                sta     savram,x
                dex
                bpl     l275_3
                lda     dosds2
                ora     #32                             ; make new # a talk/listen address
                sta     savram+6
                lda     dosds2
                ora     #64
                sta     savram+7

                lda     #8                              ; command string length
                jsr     SendDiskCmd                     ; Send command
                lda     dosds2
                sta     dosfa                           ; Make last DOS device = current device
                +lbra   close_out_1                     ; common error check & exit path ????


disk_renum_cmd  !text "M-W",119,0,2                     ; Renumber Drive command



;  GetDiskNum - Get a (required) disk number and check it

getdisknum_1
                jsr     chrget                          ; skip current character
getdisknum
                jsr     getbyt                          ; get number in .x
                cpx     #8                              ; check range (8-30)
                +lbcc   fcerr
                cpx     #31
                +lbcs   fcerr
                rts                                     ; returns only if okay



;  SendDiskCmd - Send command in SAVRAM to disk, length in .A

SendDiskCmd
; lda #   ; command string length
                ldx     #<savram                        ; address
                ldy     #>savram
                jsr     _setnam
                ldx     #sys_bank                       ; ???? sysbank ????
                jsr     _setbank
                jsr     _clrch                          ; Restore normal channels, establish our's
                ldx     dosfa                           ; fa
                lda     #doslfn                         ; la (reserved la)
                ldy     #$6f                            ; sa (command channel)
                jsr     _setlfs
                jsr     _open                           ; open channel & send command
                lda     #doslfn                         ; close it already
                sec                                     ; not a real close
                jsr     _close
                +lbra   Clear_DS                        ; Exit


;  SET VERIFY <ON | OFF> Set DOS verify-after-write mode for 3.5" drives

verify_mode
                jsr     chrget                          ; eat 'verify' token, get next  new [910429]
                cmp     #on_token
                sec
                beq     l276_1                          ; turn verify on (.c=1)
                jsr     chkesc
                cmp     #off_token                      ; turn cursor off (.c=0)
                +lbne   snerr
                clc

;  Open disk command channel & pass it 'verify' command

l276_1          php                                     ; Save mode
                jsr     chkeos                          ; eat 'on/off' token, error if not eos

                ldx     #4-1
l276_2          lda     verify_cmd,x                    ; move command to RAM, setup for open
                sta     savram,x
                dex
                bpl     l276_2

                lda     #0                              ; form on/off flag
                plp
                rol
                ora     #$30
                sta     savram+4

                lda     #5                              ; command string length
                jsr     SendDiskCmd                     ; Send command
                +lbra   close_out_1                     ; common error check & exit path ????


verify_cmd      !text "U0>V"                            ; Verify on/off command

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
