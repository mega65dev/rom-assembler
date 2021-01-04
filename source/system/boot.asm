


;****************************************************************************
; BOOT  Boot has three modes of operation...
;
;   1. *B*LOAD a given binary file and SYS to its load address.
;   2. *D*LOAD a BASIC file named AUTOBOOT.C65* and RUN it.
;   3. BOOT SYS loads the home sector to $0400 and JMPs to it.
;
; For modes 1 & 2, syntax is the same as BLOAD.  Differentiate
; between the two modes via the presence of a filename.  All other
; parameters, such as drive and device numbers, are utilized in the
; normal manner.  For mode 3 there are no options.  It's intended
; to boot a new OS.  BASIC is turned off if it's successful.  If it
; fails, the run time stack & sprites might be corrupted.
;****************************************************************************

boot            cmp #sys_token                          ; BOOTSYS?      [910111]
                bne l189_1                              ; no
                jsr chrget                              ; yes- eat token
                jsr _bootsys                            ; attempt to boot a new OS
                bcc l189_4                              ; returned to us after successful install
                ldx #errbdk                             ; bootsys failed, report 'bad disk'????
                +lbra error

l189_1          bbr4 runmod,l189_2                      ; Error if in Edit mode     [910620]
                +lbra edit_err

l189_2          lda #0                                  ; BOOT "filename"     [910417]
                sta verck                               ; want 'load', not 'verify'
                lda #$e6                                ; set up parameters for DOS parser like BLOAD
                ldx #$fc
                jsr dosprx                              ; parse the command
                bbr0 parsts,l189_5                      ; was there a filename?  branch if not
                jsr bload_boot                          ; yes- bload it
                +lbcs erexit                            ; load error

; ldx current_bank ;assume no B(ank) arg was given    [910114]
; bbr0 parstx,l189_3  ; correct, use current setup
                ldx dosbnk                              ; else use given bank number
l189_3          stx _bank
                lda _starting_addr                      ; set up address BLOAD loaded to
                sta _pclo
                lda _starting_addr+1
                sta _pchi
                jsr _jsr_far                            ; call it
l189_4          rts

l189_5          ldy #$ff
l189_6          iny                                     ; Copy default filename from ROM into buffer
                lda autoboot_filename,y
                sta savram,y
                bne l189_6                              ; null terminated

                sty dosf1l                              ; length not counting terminator
                smb6 runmod                             ; set flag for load not to go to ready
                jsr dload_boot                          ; Load it
                +lbcs erexit                            ; error if problems
                +lbra run_a_program                     ; else go run it


; AUTOBOOT_CSG Runs a system diagnostic if PB0 is low after initialization.
;  Diagnostic is copied to RAM-0 from ROM-2 and jumped to.

autobootCSG                                             ; Run ROMed diagnostic if PB0 low   [911105]
                lda $dd01
                lsr
                bcs autoboot                            ; no, try to boot from disk

                sei                                     ; prevent IRQ from wacking code DL'd to $1xxx  [911106]
                ldx #12-1
l190_1          lda l190_2,x                            ; prep DMA list
                sta dma1_cmd,x
                dex
                bpl l190_1

                lda #0
                ldx #>dma1_cmd                          ; copy program from ROM to RAM
                ldy #<dma1_cmd
                sta dma_ctlr+2                          ; dma_list bank
                stx dma_ctlr+1                          ; dma_list hi
                sty dma_ctlr                            ; dma_list lo & trigger

; jmp run_a_program ;if 'program' was BASIC
; lda #0   ;else set up arg's for call to 'long jmp'  [911105]
                sta _bank
                sta _pclo
                lda #$10
                sta _pchi
                jmp _jmp_far                            ; jump to code, no return.  NOTE: this *MAPs* RAM-0 into context!

; move from $024001 to $002001, $3FFF bytes  BASIC program
;l190_2 .byte $00,$ff,$3f,$01,$40,$02,$01,$20,$00,$00,$00,$00

; move from $024000 to $1000, $4000 bytes   Diagnostic  [911105]
l190_2          !text $00,$00,$40,$00,$40,$02,$00,$10,$00,0,0,0


; AUTOBOOT Attempts to RUN a disk program after cold startup.  The
;  program must be a BASIC program called "AUTOBOOT.C65*"

autoboot
                lda #0                                  ; Select internal drive
                sta fdc
l191_1          bit fdc+2                               ; busywait
                bmi l191_1
                lda fdc+3                               ; See if a diskette is present
                and #$08
                beq l191_3                              ; exit with no action taken if not

                lda #$e6                                ; set up parameters for DOS parser like BLOAD
                ldx #$fc
                jsr dosprx                              ; let the parser init DOS stuff

                ldy #$ff
l191_2          iny                                     ; Copy filename from ROM into buffer
                lda autoboot_filename,y
                sta savram,y
                bne l191_2                              ; null terminated
                sty dosf1l                              ; length not counting terminator

                lda #%01000001                          ; set flag for load indicating autoboot
                sta runmod                              ; set flag for load not to go to ready
                jsr dload_boot                          ; skip parser & load it

                lda #0                                  ; clear autoboot flags
                sta runmod
                phx                                     ; save end address
                phy
                jsr _readst                             ; get status report, but check it later
                pha
                jsr Suck_DS                             ; clear any DOS errors (to kill error LED)
                pla                                     ; now check I/O status
                ply
                plx
                and #$bf                                ; EOI is okay
                bne l191_3                              ; outside problems
                bcs l191_3                              ; inside problems

                stx text_top                            ; success- set end address & run it
                sty text_top+1
                cli
                +lbra run_a_program

l191_3          rts                                     ; failure- go_ready


autoboot_filename
                !text "AUTOBOOT.C65*",0




erexit          tax                                     ; set termination flags
                +lbne error                             ; normal error
                +lbra break_exit                        ; user break



outch           jsr _bsout
                bcs erexit
                rts



inchr           jsr _basin
                bcs erexit
                rts


coout
; jsr put_io_in_map
                jsr _chkout
                jsr dschk                               ; see if device # >=4, and clear DS if so
                bcs erexit                              ; take error exit of there was one
                rts


coin
; jsr put_io_in_map
                jsr _chkin
                jsr dschk                               ; see if device # >=4, and clear DS if so
                bcs erexit
                rts

cgetl
; jsr put_io_in_map
                jsr _getin
                +lbcs break_exit                        ; 'stop' key was pressed
                rts


save            jsr plsv                                ; parse parameters, dschk


savenp                                                  ; Save Program (from DSave)
                ldx text_top                            ; ending address
                ldy text_top+1
                lda #<txttab                            ; pointer to start address


savenb                                                  ; Save Binary (from BSave & KEY SAVE)
; jsr put_io_in_map
                jsr _savesp                             ; save it

; Any changes to the following code must be duplicated at:
;  bload
;  load (load_file)

exit_disk_op
exit_disk_operation
                php                                     ; preserve kernel error status (.c)
                pha                                     ; preserve kernel error # (.a)
                jsr print_dos_error                     ; print DOS error msg if any only in direct mode
                pla
                plp
                bcc l192_3                              ; branch if no error (rts)
                bbs7 runmod,l192_2                      ; branch if run mode (erexit)
                cmp #errfnf                             ; is it 'file not found' catch-all?
                bne l192_1                              ; no  (erexit)
                sta errnum                              ; yes- save error # for 'er'
                ora #$80                                ; but no errdis
l192_1          sec
l192_2          bcs erexit                              ; exit if kernel problem (rts)
l192_3          rts


verify          lda #1                                  ; verify flag
                !text $2c                               ; skip two bytes

load            lda #0                                  ; load flag
                sta verck
l193_1          bbr4 runmod,l193_2                      ; Error if in Edit mode     [910620]
                +lbra edit_err
l193_2          jsr plsv                                ; parse parameters, dschk

cld10                                                   ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< entry from dload
; jsr put_io_in_map
                lda verck
                ldx txttab                              ; .x and .y have alt...
                ldy txttab+1                            ; ...load address

; Any changes to the following code must be duplicated at:
;  bload
;  save (exit_disk_op)

load_file
                jsr _loadsp                             ; load it
                bbs0 runmod,cld20                       ; skip error checks if autoboot (rts)
                phx                                     ; save end address
                phy
                php                                     ; save kernel load status (.c)
                pha                                     ; save kernel error # (.a)
                jsr _readst                             ; save I/O status byte
                sta parsts
                jsr print_dos_error                     ; report error msg if any only in direct mode
                pla                                     ; restore error stuff
                plp
                bcc l194_3                              ; branch if no error (rts)
                bbs7 runmod,l194_2                      ; branch if run mode (erexit)
                cmp #errfnf                             ; is it 'file not found' catch-all?
                bne l194_1                              ; no  (erexit)
                sta errnum                              ; yes- save error # for 'er'
                ora #$80                                ; but no errdis
l194_1          sec
l194_2          +lbcs erexit                            ; exit if kernel problem
l194_3          ply                                     ; restore end address
                plx
                lda verck
                beq cld50                               ; was load

; Finish verify

verify_check
                ldx #ervfy                              ; assume error
; jsr _readst  ;read status
                bbs4 parsts,cld55                       ; branch if 'verify' error
                bbs7 runmod,cld20                       ; branch if not direct mode
verify_ok
                jsr _primm
                !text cr,"OK", cr,0
cld20           rts



; Finish load

cld50
; jsr _readst  ;check I/O status
                lda parsts
                and #%10111111                          ; EOI is okay, so mask it
                beq cld60                               ; good- finish load operation

load_error
                ldx #erload
cld55           +lbra error


cld60           stx text_top
                sty text_top+1                          ; end load address

                bbs7 runmod,cld70                       ; branch if not direct mode
                bbs6 runmod,cld20                       ; special "RUN file_name" flag...get out here (rts)

                jsr link_program                        ; relink
                jsr runc                                ; clear vars
                +lbra ready_2                           ; print 'ready' & return to main


; Program load

cld70           jsr reset_txtptr
                jsr link_program
                +lbra fload


open            jsr paoc                                ; parse statement
                jsr _open                               ; open it
                bra close_out_1



close           jsr paoc                                ; parse statement
; jsr put_io_in_map
                lda andmsk                              ; get la


close_out                                               ; enter with .a=LA   [900725]
                clc                                     ; flag a real close
                jsr _close                              ; close it

close_out_1
                php
                pha
                lda _fa                                 ; special error checking if disk op
                cmp #8
                bcc l195_1
                pla
                plp
                +lbra exit_disk_operation               ; disk

l195_1          pla                                     ; something else
                plp
                +lbcs erexit
                rts


; Parse LOAD, SAVE, & VERIFY commands

plsv
                lda #0                                  ; set default filename (none)
                jsr _setnam
                ldx _default_drive                      ; set default device # (dosffn)
                ldy #0                                  ; command 0
                jsr _setlfs
                lda text_bank                           ; all loads to   bank 0 ???? set default memory banks
                ldx var_bank                            ; all names from bank 1 ????   [910620]
                jsr _setbank

                jsr paoc20                              ; by-pass junk
                jsr paoc15                              ; get/set file name
                jsr paoc20                              ; by-pass junk
                jsr plsv7                               ; get ',fa'
                ldy #0                                  ; command 0
                stx andmsk
                jsr _setlfs
                jsr paoc20                              ; by-pass junk
                jsr plsv7                               ; get ',sa'
                txa                                     ; new command
                tay
                ldx andmsk                              ; device #
                jsr _setlfs
                bra dschk                               ; make dosfa current   [900801]



; Look for comma followed by byte

plsv7           jsr paoc30
                +lbra getbyt



; Skip return if next char is end

paoc20          jsr chrgot
                bne paocx
                pla
                pla
paocx           rts



; Check for comma and good stuff

paoc30          jsr chkcom                              ; check comma

paoc32          jsr chrgot                              ; get current character
                bne paocx                               ; is okay
                +lbra snerr                             ; bad...end of line


; Parse OPEN/CLOSE

paoc            lda #sys_bank                           ; ????      [910620]
                ldx var_bank                            ;
                jsr _setbank                            ; filename bank     (string bank)????
                jsr _setnam                             ; default file name (null)
                jsr paoc32                              ; must get something
                jsr getbyt                              ; get la
                stx andmsk
                txa
                ldx _default_drive                      ; default device
                ldy #0                                  ; default command
                jsr _setlfs                             ; store it
                jsr paoc20                              ; skip junk
                jsr plsv7
                stx eormsk
                ldy #0                                  ; default sa (command)
                lda andmsk                              ; get la
                cpx #3
                bcc l196_1
                dey                                     ; if sa not given and fa=serial bus, default to $ff
l196_1          jsr _setlfs                             ; store them
                jsr paoc20                              ; skip junk
                jsr plsv7                               ; get sa
                txa
                tay
                ldx eormsk
                lda andmsk
                jsr _setlfs                             ; set up real everything
                jsr paoc20
                jsr paoc30

paoc15          jsr frmstr                              ; do frmevl, frestr. return with len in a, index =~string
                jsr getspa                              ; ????fixes old PET bug- load"string",val(chr$(56)) [910917]
; ldx index1
; ldy index1+1
                jsr _setnam                             ; bank always set at plsv
;fall into dschk     [900801]



dschk           php                                     ; check if current device >=8, and clear DS if so
                pha
                lda _fa
                cmp #1
                bne l197_1
                lda _default_drive
                sta _fa
l197_1          cmp #8                                  ; ????     [900807]
                bcc l197_2
                sta dosfa                               ; also make last DOS device = current device
                jsr Clear_DS
l197_2          pla
                plp
                rts


;k_readst
; jsr put_io_in_map
; jmp _readst



;k_setlfs
; jsr put_io_in_map
; jmp _setlfs



;k_setnam
; jsr put_io_in_map
; jmp _setnam



;k_basin
; jsr put_io_in_map
; jmp _basin


;k_bsout
; jsr put_io_in_map
; jmp _bsout


;k_clrch
; jsr put_io_in_map
; jmp _clrch



;k_close
; jsr put_io_in_map
; jmp _close



;k_clall
; jsr put_io_in_map
; jmp _clall



;k_primm
; jsr put_io_in_map
; jmp _primm


;k_setbank
; jsr put_io_in_map
; jmp _setbank
; rts


;k_plot
; sta sw_rom_ram0  ;????
; jmp _plot


;k_stop
; jsr put_io_in_map
; jmp _stop

;.end