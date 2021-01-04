


;**************************************************************
;
;   KEY  Programmable Key Functions    [900725]
;
;**************************************************************

key              beq Key_List                             ; KEY ? yes- no args

l97_1            ldx _kyndx                               ; is function key buffered?
                 bne l97_1                                ; yes- hang until IRQ finishes processing it ????

                 cmp #on_token                            ; KEY ON ?
                 bne l97_2
                 rmb5 _locks                              ; yes- reset Editor's lock bit
                 bra l97_4                                ; exit

l97_2            cmp #load_token                          ; KEY LOAD <filename>[,D#,U#]
                 +lbeq Key_load

                 cmp #save_token                          ; KEY SAVE <filename>[,D#,U#]
                 +lbeq Key_Save

                 cmp #restore_token                       ; KEY RESTORE ?      [910925]
                 bne l97_3                                ; no
                 rmb5 _locks                              ; yes- reset Editor's lock bit (enable keys)
                 jsr key_restore                          ; init key definitions
                 bra l97_4                                ; exit

l97_3            cmp #esc_command_token                   ; KEY OFF ?
                 +lbne Key_Change                         ; no- must be new key definition
                 jsr chrget
                 cmp #off_token
                 +lbne snerr                              ; no- bad syntax
                 smb5 _locks                              ; yes- set Editor's lock bit
l97_4            +lbra chrget                             ; exit


;**************************************************************
;
;   Key_List  List all function key definitions
;
;**************************************************************

Key_List
                 ldx #0                                   ; display all key definitions
                 ldy #0

lstky1           inx                                      ; get key number = 1-16
                 lda _pky_lengths-1,x                     ; get key size
                 beq lstest                               ; skip if key not defined
                 sta keysiz                               ; save size
                 stx z_p_temp_1                           ; save key number

                 phy
                 ldx #3
l98_1            lda preamb,x                             ; print key preamble:
                 jsr _bsout
                 dex                                      ; 'KEY '
                 bpl l98_1
                 ldx z_p_temp_1                           ; key number
                 lda #0
                 jsr linprt
                 lda #','
                 jsr _bsout                               ; comma

                 ply
                 ldx #7                                   ; (length of 'keydat' string)
lsloop           lda _pky_buffer,y                        ; print key definition
                 iny
                 pha                                      ; save character
                 phx                                      ; save position in output string

                 ldx #4                                   ; check for special (non-printable) characters
l99_1            cmp keychr-1,x
                 beq list_special                         ; yes, display it as 'CHR$(...)'
                 dex
                 bne l99_1

                 plx                                      ; restore position
                 cpx #8
                 bcc l99_2                                ; 1st time thru- display leading quote
                 bne l99_3                                ; previous was a character- no additions needed
                 lda #'+'                                 ; add since previous was quote or return
                 jsr _bsout
l99_2            lda #'"'
                 jsr _bsout                               ; add leading quote
l99_3            pla                                      ; restore character
                 jsr _bsout                               ; display it
                 ldx #9                                   ; mark normal character

lstnd            dec keysiz
                 bne lsloop                               ; loop to end of definition
                 cpx #9
                 bcc l100_1                               ; skip if previous not normal character
                 lda #'"'
                 jsr _bsout                               ; add ending quote

l100_1           lda #$8d
                 jsr _bsout                               ; add ending return (shifted)

                 ldx z_p_temp_1                           ; key number
lstest           cpx #number_fkeys
                 bne lstky1                               ; ...loop until done all keys
                 rts


list_special
                 plx                                      ; restore .x
l101_1           lda keydat-3,x                           ; display something like  ' "+CHR$( '
                 jsr _bsout
                 dex
                 cpx #3
                 bcs l101_1
                 pla                                      ; restore character
                 jsr prtdec                               ; display decimal value of chr in .a
                 lda #')'                                 ; finish off with closing paren.
                 jsr _bsout
                 ldx #8                                   ; mark end of special
                 bra lstnd                                ; ..always


preamb           !text " YEK"                             ; key preamble

keydat           !text "($RHC+",$22                       ; chr$( string

keychr           !text cr,$8d,$22,esc                     ; special KEY chars- return, sft-return, quote, esc


;************************************************************************
;
;   Key_Change  Add, Delete or Change function key definition
;
;************************************************************************

Key_Change
                 jsr getbyt                               ; get key number (1-16)
                 stx z_p_temp_1                           ; save key number     [910925]
                 dex
                 cpx #number_fkeys
                 +lbcs fcerr                              ; exit - key number invalid

; stx z_p_temp_1 ;save key number
                 jsr chkcom                               ; look for comma
                 jsr frmstr                               ; do frmevl, frestr. returns len in .a, addr in 'index'
                 tay                                      ; set up for call to do-a-key
                 lda #1                                   ; tell do-a-key that string is in bank 1
                 sta index+2
                 lda #index                               ; now .A points to (adr lo, adr hi, bank #)
                 ldx z_p_temp_1
; inx         [910925]
key_restore
; jsr put_io_in_map
                 jsr _doakey                              ; re-define the key
                 +lbcs omerr                              ; bad return (.c=1)
                 rts                                      ; ok return  (.c=0)


;************************************************************************
;   Key_Load  Load function key definitions (from disk)   [900725]
;************************************************************************

Key_load
                 jsr GetLoadChannel                       ; get a channel      [911001]
                 ldy #>_pky_lengths
                 lda #<_pky_lengths
                 jsr LoadBlock                            ; load it
                 +lbra list_err                           ; release channel, close file, return to main


GetLoadChannel                                            ; Used by KeyLoad and SpriteLoad    [911001]
                 jsr chrget                               ; eat LOAD token
                 lda #$e6                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; (like dopen:  0 0 0 *  * 0 0 1 )
                 jsr chk1                                 ; check parameters
                 lda #0
                 sta dossa                                ; setup as dload would (0 = load channel)
                 jsr find_la                              ; find an unused LA to use (cannot use reserved one)
                 ldy #fopn
                 ldx #4
                 jsr open_file                            ; open the file
                 bcs LoadEOF                              ; exit if problem
                 ldx dosla
                 jsr _chkin                               ; get input channel
                 bcs LoadEOF                              ; exit if bad??
                 jsr _basin                               ; waste dummy load address
                 jsr _basin
                 jsr _readst                              ; prevent corruption if there's a problem
                 bne LoadERR                              ; exit if problem
                 clc
                 rts


LoadBlock
                 sta highds                               ; where to put data
                 sty highds+1
LoadBlockNext
                 ldy #0
l102_1           jsr _basin                               ; read definitions
                 sta (highds),y
                 jsr _readst                              ; check channel status
                 bne LoadEOF                              ; exit if eof or error
                 jsr _stop
                 beq LoadEOF                              ; exit if stop key down
                 iny
                 bne l102_1                               ; continue up to 1 page maximum
                 clc                                      ; indicate "more"
                 rts


LoadERR
                 ldx #erload                              ; Load Error
                 sec
                 !text $89

LoadEOF
                 clc
                 +lbra list_err                           ; release channel, close file, return to main


;************************************************************************
;   Key_Save  Save function key definitions (from disk)   [900725]
;************************************************************************

Key_Save
                 jsr GetSaveChannel                       ; [910930]
                 lda #highds                              ; set starting & ending addresses
                 ldy #>_pky_lengths                       ; start address & pointer to it
                 ldx #<_pky_lengths
                 sty highds+1
                 stx highds
                 iny                                      ; end address = start address + 256 + 1
                 inx
                 +lbra savenb                             ; [910925]
; jsr _savesp  ;save it
;; clc   ; return no error  ????why not  [910404]
; bra exit_disk_op ; but if direct mode print DOS error  [910404]



GetSaveChannel                                            ; Used by KeySave and SpriteSave    [910930]
                 jsr chrget                               ; eat SAVE token
                 lda #$66                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; parse options
                 jsr chk2                                 ; check required parameters

                 ldy #fopn                                ; DOS table offset
                 lda #4                                   ; length
                 jsr sendp
                 lda #sys_bank                            ; set banks ???? buffer in system bank ????
                 ldx #sys_bank
                 jmp _setbank

;.end
