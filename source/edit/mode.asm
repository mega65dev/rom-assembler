

; Edit mode is simply a poor man's word processor.  Text is entered normally
; as if the user were typing in a program, but tokenization is turned off.
; This affects only that text which follows a line number.  CRUNCH and QPLOP
; test for this mode, and deal with the text accordingly.  RUN, GOTO, etc.
; test for this mode and error-out if it's enabled.  LOADing a text file
; will automatically add line numbers, SAVEing a text file will remove them.
;
; Syntax:  EDIT < ON | OFF >
;
; F. Bowen        [910620]

edit             jsr errind                               ; direct mode only command
                 cmp #on_token
                 bne l311_1
                 lda #%00010000                           ; EDIT ON
                 bra l311_3                               ; (this kills trace mode, too)

l311_1           jsr chkesc                               ; [910930]
; cmp #esc_command_token
; bne l311_2
; jsr chrget
                 cmp #off_token
l311_2           +lbne snerr
                 lda #0                                   ; EDIT OFF
l311_3           sta runmod
                 jmp chrget                               ; exit


edit_crunch                                               ; Edit mode only, find end of plain text in input buffer
                 phw txtptr                               ; save current position in input buffer
                 jsr rem                                  ; find the end of the line
                 ldx txtptr
                 pla                                      ; restore buffer pointer
                 sta txtptr+1
                 pla
                 sta txtptr
                 sec                                      ; compute length of line
                 txa
                 sbc txtptr
                 tay
                 iny
                 rts                                      ; done


edit_p1line                                               ; Edit mode only, list a line of plain text
                 jsr linprt                               ; print line number in (a,x)
                 lda #' '                                 ; print a space

                 ldy #3                                   ; start printing at text following line number
l312_1           jsr outch                                ; print character
                 iny
                 bbr5 helper,l312_2                       ; if called from FIND/CHANGE check for highlighting
                 jsr helpsb
l312_2           jsr indlow                               ; get next character
                 bne l312_1                               ; loop until eol
                 rts                                      ; done


;.end

; EDIT LOAD/SAVE  Load or Save a plain text SEQ file in memory

edit_load                                                 ; Called by DLOAD/DVERIFY when in EDIT mode
                 ldz #1
                 jsr open_SEQ_file                        ; Open the file just like TYPE: filename [,U#] [,D#]
                 jsr Check_DS                             ; check current disk error message
                 ldy #0
                 lda #dsdesc+1
                 jsr lda_far_ram1                         ; lda (dsdesc+1),y peek at first character
                 cmp #'2'
                 +lbcs l313_12                            ; exit if error
                 jsr Clear_DS                             ; else zap 'ok' message so user gets fresh one
                 ldx dosla
                 jsr _chkin                               ; get input channel
                 +lbcs l313_12                            ; error

                 bbs0 verck,l313_1
                 jsr _primm
                 !text cr,"LOADING",0
                 bra l313_2
l313_1           jsr _primm
                 !text cr,"VERIFYING",0

l313_2           lda #<1000                               ; default starting line #
                 ldx #>1000
                 sta linnum
                 stx linnum+1

                 lda txttab                               ; load address
                 ldx txttab+1
                 sta index
                 stx index+1

l313_3           ldy #0                                   ; Input one line of text
                 jsr _stop                                ; check stop key
                 beq l313_11                              ; exit if down
                 jsr _readst                              ; check channel status
                 bne l313_11                              ; exit if eof or error

                 bbr0 verck,l313_4
                 ldy #3
                 bra l313_5                               ; skip ahead if verify op

l313_4           lda #1                                   ; install fake line links for this line
                 jsr sta_far_in1
                 iny                                      ; 1
                 jsr sta_far_in1
                 iny                                      ; 2
                 lda linnum                               ; install line number for this line
                 jsr sta_far_in1
                 iny                                      ; 3
                 lda linnum+1
                 jsr sta_far_in1
                 clc
                 lda linnum                               ; generate next line number
                 adc #10
                 sta linnum
                 bcc l313_5
                 inc linnum+1


l313_5           iny                                      ; bump buffer pointer
                 cpy #buflen                              ; check buffer (160 max. input buffer size to edit)
                 beq l313_8                               ; split long lines into two????
                 jsr _basin                               ; read file data
                 beq l313_8                               ; CR or null terminates line
                 cmp #cr
                 beq l313_8
; cmp #$20  ;adjust invisible characters less than space
; bcc l313_6   ; ????make them appear in reverse field, but note
; ora #$80  ; that these lines can't be edited without losing them.

l313_6           bbr0 verck,l313_7
                 jsr indcmp_in1                           ; Compare to memory
                 beq l313_5                               ; ok
                 jsr list_exit
                 ldx #ervfy                               ; verify error
                 +lbra error

l313_7           jsr sta_far_in1                          ; Load into memory
                 bra l313_5                               ; loop until eol or error (kernel returns CR in case of error)

l313_8           bbs0 verck,l313_9
                 lda #0
                 jsr sta_far_in1                          ; terminate line with null (replaces CR)
l313_9           iny
                 tya
                 clc
                 adc index
                 sta index
                 bcc l313_10
                 inc index+1
l313_10          lda index+1
                 cmp max_mem_0+1                          ; out of memory????
                 bcc l313_3                               ; no, continue until eof
                 bsr edit_load_done                       ; yes, patch things up best we can
                 jsr list_exit                            ; close disk
                 +lbra omerr                              ; report error & exit

l313_11          bbs0 verck,l313_12
                 jsr edit_load_done                       ; EOF: terminate memory with a pair of nulls
l313_12          +lbra list_exit                          ; release channel, close file, etc.

; bbr0 verck,40$
; jsr verify_ok  ;if Verify, report 'ok'
;40$ lda #0   ;exit directly to main????
; bra end

edit_load_done
                 lda #0                                   ; EOF: terminate memory with a pair of nulls
                 tay
                 jsr sta_far_in1
                 iny
                 jsr sta_far_in1
                 inw index
                 inw index
                 ldx index                                ; set top
                 ldy index+1
                 stx text_top
                 sty text_top+1
                 +lbra link_program                       ; relink & RTS


edit_save
                 lda #$e6                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; (like dopen:      0 0 0 *  * 0 0 1 )
                 jsr chk1                                 ; check parameters
                 jsr find_la                              ; find an available LA
                 jsr find_sa                              ; find an available SA
                 ldy #fsavseq
                 ldx #8
                 jsr open_file                            ; open the file
                 +lbcs list_err                           ; exit if error
                 ldx dosla
                 jsr _chkout                              ; get output channel
                 bcs l314_5                               ; error

                 lda txttab                               ; save address
                 ldx txttab+1
                 sta index
                 stx index+1

l314_1           jsr _stop                                ; check stop key
                 beq l314_5                               ; exit if down
                 jsr _readst                              ; check channel status
                 bne l314_5                               ; exit if eof or error????

                 ldy #3                                   ; save a line, starting past links & line#
l314_2           iny                                      ; bump buffer pointer
; cpy #buflen  ;check buffer (160 max. input buffer size to edit)
; beq ??$   ; split long lines into two????
                 jsr indin1
                 tax                                      ; save character for eol check
                 bne l314_3
                 lda #cr                                  ; eol: substitute CR ???? allow some other terminator
l314_3           jsr _bsout                               ; write file data
                 txa
                 bne l314_2                               ; loop until eol

                 iny                                      ; advance text index to start of next line
                 tya
                 clc
                 adc index
                 sta index
                 bcc l314_4
                 inc index+1

l314_4           ldy #0                                   ; check for EOF: a pair of null links
                 jsr indin1
                 bne l314_1
                 iny
                 jsr indin1
                 bne l314_1                               ; loop until end of text

l314_5           +lbra list_exit                          ; release channel, close file, exit

;.end


Sound_CLR
                 jsr chkeos                               ; eat CLR token, check eos   [910717] new
Sound_CLR_1
                 php
                 sei
; jsr go_slow  ;      [910716] 4567R7A
                 lda #0
                 ldx #24-1
l315_1           sta sid1,x                               ; initialize SID chips
                 sta sid2,x
                 dex
                 bpl l315_1

                 sta filters1+2                           ; set filters off
                 sta filters2+2

                 lda #8                                   ; set default volume
                 sta filters1+3
                 sta filters2+3                           ; [910612]
; sta filters+4  ;why?      [910612]
                 sta sid1+24
                 sta sid2+24

; jsr go_fast  ;      [910716] 4567R7A

                 bit _pal_ntsc                            ; determine if PAL or NTSC system  [910724]
                 bmi l315_2                               ; ...branch if PAL
                 lda #(<beats_ntsc)/4                     ; set beat to quarter note (4/4 time = .5 sec)
                 ldy #>beats_ntsc/4
                 bra l315_3
l315_2           lda #<beats_pal/4
                 ldy #>beats_pal/4
l315_3           sta ntime
                 sty ntime+1

                 lda #4                                   ; set default octave
                 sta octave
                 lda #12                                  ; set default tempo    [910220]
                 sta tempo_rate                           ; 12 makes whole note in 4/4 time last 2 seconds

                 ldy #30-1                                ; initialize music tables
l315_4           lda atkmus,y
                 sta atktab,y
                 dey
                 bpl l315_4

                 ldx #10-1                                ; initialize pulse widths
l315_5           lda pwhmus,x
                 sta pulshi,x
                 dex
                 bpl l315_5

                 txa                                      ; $ff
                 ldx #6-1                                 ; stereo SIDs   (save space) [911119]
                 ldy #1
l315_6           sta sound_time_hi,x                      ; turn all SOUND counters off
                 sta voices,y                             ; turn all PLAY counters off
                 iny
                 iny
                 dex
                 bpl l315_6

                 ldy #6-1                                 ; set default envelope (piano) for all voices (6)
                 sty voice
l315_7           ldx #0
                 jsr set_envelope_1
                 dec voice
                 bpl l315_7
                 inc voice                                ; set default voice (0)

                 plp
                 rts
