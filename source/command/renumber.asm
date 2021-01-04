

;***********************************************************************
;
; RENUMBER Command
;
; Syntax:  RENUMBER [n1 [,[n2] ,n3]]
;
;  n1 = new start line number, default 10
;  n2 = line increment, default 10
;  n3 = start line, default first
;
; - Syntax error may occur for missing commas or bad line numbers.
; - Illegal quantity error for line increment of 0 or for bad range.
; - Overflow error if increment wraps line number during renumber,
;  line number too large error if renumbering would force line
;  numbers greater than 63999.
; - Out of memory error if the renumbered program would be too large.
; - Unresolved reference error if an imbedded line number references
;  a line which does not exist.
;
; Otherwise returns to "ready" mode upon completion.
;
;***********************************************************************


; Before any data is changed in any way, two preliminary passes are
; made to insure no errors would occur during the actual renumbering
; process (as detailed below).
;
; Pass 1 makes sure that the renumbered program would have no line
; numbers greater than 63999 (nothing is actually renumbered; the
; statement table is not modified).
;
; Pass 2 checks if the renumbered program would be too long and also
; checks for non-existant line number destinations.
;
; Pass 3 examines the entire statement table first for imbedded line
; numbers (branches) to fix. This is done by looking for keywords (GOTO,
; GOSUB, THEN, RUN) which are usually followed by line numbers. The old
; line number is mapped to a new value and the string representing the
; new branch label replaces the original text.
;
; Pass 4 then replaces the statement number bytes by their final values.
; and the table is relinked.


testwd
                 !text goto_token,run_token,gosub_token,then_token
                 !text restore_token,resume_token,trap_token,else_token

renumber
                 jsr errind                               ; allowed only in direct mode

; Set up default values for n1, n2, and n3

                 lda #0                                   ; line #10...
                 ldx #10
                 stx renum_tmp_1                          ; default renum origin (n1)
                 sta renum_tmp_1+1
                 stx renum_tmp_2                          ; default increment (n2)
                 sta renum_tmp_2+1
                 sta hightr                               ; default start line # (n3)
                 sta hightr+1

                 jsr chrgot                               ; any parameters?
                 beq ren_pass_1                           ; no...


; Check for new starting line number (n1)

                 jsr linget                               ; check for a number
                 lda endchr                               ; was there one?
                 beq renum_10                             ; no...use default
                 lda linnum
                 ldx linnum+1
                 sta renum_tmp_1
                 stx renum_tmp_1+1

; Check for new increment

renum_10
                 jsr optwrd                               ; an increment given?
                 bcc renum_30                             ; no...use default

                 sty renum_tmp_2
                 sta renum_tmp_2+1
                 ora renum_tmp_2                          ; increment must be >0
                 +lbeq fcerr                              ; illegal quantity error

; Check for starting line number

renum_30
                 jsr optwrd                               ; starting line number given?
                 bcc ren_pass_1                           ; no...

                 sty hightr
                 sty linnum
                 sta hightr+1
                 sta linnum+1
                 jsr FindLine                             ; test for illegal renumber range
                 lda lowtr                                ; (n1 must be >= n3)
                 ldx lowtr+1
                 sta highds                               ; pointer to first statement to renumber
                 stx highds+1
                 lda renum_tmp_1
                 ldx renum_tmp_1+1
                 sta linnum
                 stx linnum+1
                 jsr FindLine                             ; lowtr = ptr to 1st stmt to be overlapped
                 sec
                 lda lowtr                                ; can't be smaller
                 sbc highds
                 lda lowtr+1
                 sbc highds+1
                 +lbcc fcerr                              ; bad...


;***********************************************************************
;**************  R E N U M B E R    P A S S    O N E  ******************
;***********************************************************************

; Pass 1 makes sure that the renumbered program will not have any line numbers
; greater than 63999 (however, nothing is actually renumbered in this pass).

ren_pass_1
                 jsr tto                                  ; save txtptr for restoration when done
                 jsr n1_reset                             ; put n1 in FAC, reset txtptr
                 jsr chargt                               ; skip low link
                 iny                                      ; (.y=1)
                 jsr indtxt                               ; skip high link
                 beq ren_pass_2                           ; end of program => begin pass 2 (assumes txttab > 0)

r_pass1_10
                 iny                                      ; (.y=2)
                 jsr indtxt                               ; line number low
                 sec
                 sbc hightr                               ; in line range which is to be renumbered?
                 iny                                      ; (.y=3)
                 jsr indtxt                               ; line number high
                 sbc hightr+1
                 bcs r_pass1_20                           ; yes => fake renumbering
                 jsr set_next                             ; goto next line
                 bne r_pass1_10                           ; if z=0 then not end-of-text => keep going
                 beq ren_pass_2                           ; else end

r_pass1_20
                 jsr set_next                             ; goto next line
                 beq ren_pass_2                           ; if z=1 then end-of-text => exit
                 jsr new_num                              ; create next line number
                 bcs r_pass1_30                           ; if c=1 then it wrapped => error
                 cmp #>63999                              ; can't have lines > 63999
                 bcc r_pass1_20                           ; if c=0 then ok

r_pass1_30                                                ; renumbering will generate an illegal line #
                 ldx #err_too_large                       ; 'line number too large' error
                 +lbra error

set_next
                 ldy #0                                   ; set for next BASIC line
                 jsr indtxt                               ; low link
                 tax
                 iny                                      ; (.y=1)
                 jsr indtxt                               ; high link
                 beq set_end                              ; if z=1 then end of program => exit
                 stx txtptr
                 sta txtptr+1
set_end          rts


;***********************************************************************
;**************  R E N U M B E R    P A S S    T W O  ******************
;***********************************************************************

; Pass 2 checks if the renumbered program will be too long and also
; checks for non-existant line number destinations.

ren_pass_2
                 bbr4 runmod,l74_1                        ; skip pass two and three if plain text (edit mode) [910620]
                 jsr n1_reset                             ; yes- just setup up starting line # and reset txtptr
                 bra ren_pass_4                           ; then renumber just the text's line numbers

l74_1            lda #$01                                 ; set flag for 'pass 2'
                 sta z_p_temp_1
                 lda text_top                             ; copy top-of-text pointer for later use
                 ldx text_top+1                           ; (we don't want to change original here)
                 sta fndpnt
                 stx fndpnt+1
                 jsr imbed_lines                          ; search for imbedded lines (but don't change)



;***********************************************************************
;************  R E N U M B E R    P A S S    T H R E E  ****************
;***********************************************************************

; Pass 3 actually renumbers the imbedded destination line numbers
; which follow goto, gosub, trap, etc.

ren_pass_3
                 dec z_p_temp_1                           ; z_p_temp_1 = 0 (for pass 3)
                 jsr imbed_lines                          ; search for and update imbedded line #'s


;***********************************************************************
;*************  R E N U M B E R    P A S S    F O U R  *****************
;***********************************************************************

; Pass 4 actually renumbers the program line numbers & exits

ren_pass_4
                 jsr chargt_x2                            ; skip link
                 beq renumber_exit                        ; null link=> end-of-text, exit (assumes txttab > 0)
                 jsr chargt                               ; not null...
                 sta linnum                               ; if line# >= start#, replace with facho
                 iny
                 jsr indtxt
                 sec
                 sbc hightr+1
                 bcc r_pass4_20                           ; no, let alone
                 bne r_pass4_10                           ; yes, replace
                 lda linnum
                 sbc hightr
                 bcc r_pass4_20                           ; no, let alone

r_pass4_10
                 lda facho
; phx
                 jsr sta_far_txt                          ; sta (txtptr),y  hi  (bleed-thru)
                 dey
                 lda facho+1
                 jsr sta_far_txt                          ; sta (txtptr),y  lo (bleed-thru)
; plx
                 jsr chargt                               ; skip past 2nd byte of line#
                 jsr line_inc                             ; incr line# and scan to eol
                 bra ren_pass_4                           ; always...

r_pass4_20
                 jsr chargt                               ; skip past line#
                 jsr scan_thru                            ; scan to eol
                 bra ren_pass_4                           ; always...


renumber_exit
                 jsr fix_links                            ; patch things up: relink & set eot

direct_mode_exit
                 jsr ott                                  ; restore txtptr for next command in buffer
                 lda #0                                   ; but disallow continuing
                 sta oldtxt+1
                 rts


;***********************************************************************
;*************  R E N U M B E R   S U B R O U T I N E S  ***************
;***********************************************************************

; Look for imbedded line #'s (after GOTO, GOSUB, etc.)
; but only change them in pass 3 (ie. z_p_temp_1 = 0)

imbed_lines
                 jsr reset_txtptr                         ; start at first line: load (txtptr) with (txttab)-1

next_line
                 jsr chargt_x2                            ; skip link (assumes txttab > 0)
                 +lbeq n1_reset                           ; null link: put current line # in fac, reset txtptr, exit
                 jsr chargt                               ; line number
                 sta forpnt                               ; save in case there is an error
                 jsr chargt
                 sta forpnt+1

next_char
                 jsr chargt                               ; first character in the line

chk_quote
                 cmp #'"'                                 ; opening double quote?
                 bne not_quote                            ; no...
l75_1            jsr chargt                               ; scan line
                 beq next_line                            ; end...
                 cmp #'"'                                 ; close double quote
                 bne l75_1                                ; no... continue
                 bra next_char                            ; yes... resume renumber

not_quote
                 tax                                      ; end of line?
                 beq next_line                            ; yes...
                 bpl next_char                            ; not a token...

                 ldx #8                                   ; check special token list
l76_1            cmp testwd-1,x
                 beq iline_10                             ; a match...
                 dex
                 bne l76_1                                ; continue until zero

                 cmp #go_token                            ; wasn't in the token list. check for 'go to'
                 bne chk_escape                           ; not 'go', go check for 'collision' *c128 fix*
hop_1            jsr chrget                               ; got a 'go', look for 'to'
                 beq next_line                            ; end of line, abort
                 cmp #to_token
                 beq iline_10                             ; got it! go to fix number routine
                 bra next_char                            ; no 'to', keep looking

; Look for 'COLLISION'.  This is an escape command. *c128 fix* ?????????

chk_escape
                 cmp #esc_command_token
                 bne next_char
                 jsr chrget
                 beq hop_1                                ; end of line ,abort
                 cmp #collision_token
                 bne next_char
l77_1            jsr chrget                               ; got it! skip over first argument
                 beq hop_1                                ; end of line, abort
                 cmp #','
                 bne l77_1                                ; not there yet


iline_10
                 lda txtptr                               ; save current txtptr
                 sta oldlin
                 lda txtptr+1
                 sta oldlin+1
                 jsr chrget
                 bcs chk_quote                            ; not a #...
                 jsr linget                               ; get line # from text
                 jsr form_line                            ; replace if this line # > n3
                 lda oldlin                               ; restore old txtptr
                 sta txtptr
                 lda oldlin+1
                 sta txtptr+1

                 jsr chrget                               ; skip over leading spaces
                 dew txtptr                               ; then backup (txtptr) by 1
                 ldx #$ff
                 lda z_p_temp_1                           ; if this is pass2 then don't actually change
                 beq p3code                               ; if z=1 then pass3 => ok to change
                 jsr p2code                               ; renumber 'pass two': trial run to see if enough room
                 jsr chrgot                               ; re-get last character from BASIC text & rts

iline_20
                 cmp #','                                 ; comma from 'on'?
                 beq iline_10                             ; it is...
                 bra chk_quote                            ; no...


;*********** This part of imbed_lines executed in pass 2 only **********

p2code                                                    ; updates text_top without actually changing lines
                 inx
                 lda fbuffr+1,x                           ; get character from number
                 beq l78_3                                ; end of number
                 jsr chrget                               ; get digit from old number
                 bcc p2code                               ; digit...move on

l78_1            inw fndpnt
                 sec                                      ; have we run out of memory (theoretically)?
                 lda fndpnt                               ; (compare with limit-of-memory pointer)
                 sbc max_mem_0
                 lda fndpnt+1
                 sbc max_mem_0+1
                 +lbcs omerr                              ; yes- out of memory error
                 inx                                      ; no - next...
                 lda fbuffr+1,x
                 bne l78_1
l78_2            rts                                      ; no more

l78_3            jsr chrget
                 bcs l78_2                                ; old stuff after # is other char
                 dew fndpnt                               ; digit...move down
                 bra l78_3                                ; still digits...


;*********** This part of imbed_lines executed in pass 3 only **********

p3code
                 inx
                 lda fbuffr+1,x                           ; get character from number
                 beq l79_3                                ; end of number

                 pha                                      ; save digit from new number
                 jsr chargt                               ; get digit from old number
                 cmp #':'                                 ; command terminator or letter?
                 bcs l79_1
                 cmp #' '                                 ; space? (fix for goto10 :rem)
                 beq l79_1
                 sec
                 sbc #'0'                                 ; number?
                 sec
                 sbc #$d0
                 bcc l79_2                                ; digit...move on

l79_1            jsr move_init                            ; other char...move up
                 jsr moveup
                 inw text_top

l79_2            pla
                 phx
                 ldy #0
                 jsr sta_far_txt                          ; put new digit in new number (bleed-thru)
                 plx
                 bra p3code


l79_3            jsr chrget
                 bcs iline_20                             ; old stuff after # is other char

l79_4            jsr move_init                            ; digit...move down
                 jsr movedown
                 dew text_top
                 jsr chrgot
                 bcc l79_4                                ; still digits...

                 bra iline_20                             ; branch always


;*************************** FORM_LINE *********************************

; Remaps the destination line if it is greater than n3

form_line
                 jsr n1_reset
find_it
                 jsr chargt_x2                            ; new line, skip over link
                 bne l80_1                                ; if we get to end-of-text without finding the
                 ldx #err_ref                             ; line # then 'unresolved reference' error
                 lda forpnt
                 sta curlin                               ; fake error routine into saying 'in line xxxxx'
                 lda forpnt+1
                 sta curlin+1
                 +lbra error

l80_1            jsr chargt                               ; get line number low
                 sta highds                               ; highds = current line# in loop
                 cmp linnum
                 bne l80_4
                 jsr chargt                               ; get line number high
                 sta highds+1
                 cmp linnum+1
                 bne l80_5
                 sec                                      ; if linnum < start#, no remapping
                 sbc hightr+1
                 bcc l80_2
                 bne l80_3
                 lda linnum
                 sbc hightr
                 bcs l80_3

l80_2            lda linnum                               ; use same line#
                 sta facho+1
                 lda linnum+1
                 sta facho

l80_3            ldx #$90                                 ; make replacement string
                 sec
                 jsr floatc
                 +lbra fout


l80_4            jsr chargt
                 sta highds+1                             ; (** 01/27/84 fix)

l80_5            jsr line_add                             ; scan to end of line
                 bra find_it                              ; always


;*************************** N1_RESET **********************************

; Copies n1 (new renumber origin) into facho & sets (txtptr) = (txttab)-1

n1_reset
                 lda renum_tmp_1
                 sta facho+1
                 lda renum_tmp_1+1
                 sta facho
                 +lbra reset_txtptr


;*************************** LINE_ADD **********************************

; Adds n2 (new line increment) to line number stored in facho if the
; current line number (highds) >= n3 (line to start renumbering with).
; The line is then scanned.

line_add
                 lda highds                               ; if line# >= start# then incr new#
                 sec
                 sbc hightr
                 lda highds+1
                 sbc hightr+1
                 bcc scan_thru

line_inc
                 jsr new_num

scan_thru
                 jsr chargt                               ; scan to end of line
                 bne scan_thru
                 rts


;**************************** NEW_NUM **********************************

; Adds n2 (the new line increment) to the line number stored in facho.

new_num
                 lda facho+1                              ; increment new line#
                 clc
                 adc renum_tmp_2
                 sta facho+1
                 lda facho
                 adc renum_tmp_2+1
                 sta facho
                 rts


;********************** CHARGT & CHARGT_X2 *****************************

; Chargt simulates chrget but doesn't ignore spaces & carry has no
; significance.  Chargt_x2 executes chargt twice.
; Used by Renumber, Find/Change, etc.

chargt_x2
                 inw txtptr                               ; jsr chargt
chargt
                 ldy #0                                   ; increment txtptr
                 inw txtptr
                 +lbra indtxt


;***********************************************************************
;************************* MEMORY MOVE ROUTINES ************************
;***********************************************************************

;****************************** MOVEINIT *******************************

; Setup for Renumber memory move.

move_init
                 lda txtptr                               ; index1 = txtptr
                 sta index1
                 lda txtptr+1
                 sta index1+1

                 lda text_top                             ; index2 = text_top
                 sta index2
                 lda text_top+1
                 sta index2+1

                 lda #1                                   ; move 1 character
                 sta count                                ; lo
                 dec
                 sta argmo                                ; hi

                 rts


;****************************** MOVEDOWN *******************************

; Move block of BASIC text from INDEX1+COUNT to INDEX2 down to INDEX1.
; Used by commands Renumber, Find/Change.

movedown
                 sec                                      ; set up DMA list:   [900524]
                 lda index2
                 sbc index1
                 sta dma1_cnt_lo                          ; cnt = index2-index1-count
                 lda index2+1
                 sbc index1+1
                 sta dma1_cnt_hi
                 sec
                 lda dma1_cnt_lo
                 sbc count                                ; lo
                 sta dma1_cnt_lo
                 lda dma1_cnt_hi
                 sbc argmo                                ; hi
                 sta dma1_cnt_hi

                 clc
                 lda index1
                 sta dma1_dest_lo                         ; dest = index1
                 adc count
                 sta dma1_src_lo                          ; src = index1+count
                 lda index1+1
                 sta dma1_dest_hi
                 adc argmo
                 sta dma1_src_hi

                 lda text_bank                            ; bank = BASIC text bank
; and #%00001111  ;      [910520] F018A
                 sta dma1_src_bank
                 sta dma1_dest_bank

execute_DMA1                                              ; [910620] Edit
                 lda #0
                 sta dma1_cmd                             ; command = copy from startpoint
                 sta dma1_subcmd                          ; [910520] F018A

                 sta dma_ctlr+2                           ; dma_list bank
                 lda #>dma1_cmd
                 sta dma_ctlr+1                           ; dma_list hi
                 lda #<dma1_cmd
                 sta dma_ctlr                             ; dma_list lo & trigger
                 rts


;******************************* MOVEUP ********************************

; Move block of BASIC text from INDEX1 to INDEX2 up to INDEX2+COUNT.
; Used by commands Renumber, Find/Change.

moveup
                 sec                                      ; set up DMA list:   [900524]
                 lda index2
                 sbc index1
                 sta dma1_cnt_lo                          ; cnt = index2-index1
                 lda index2+1
                 sbc index1+1
                 sta dma1_cnt_hi

                 dew index2                               ; (index2 = text_top = end+1)
                 clc
                 lda index2
                 sta dma1_src_lo                          ; src = index2
                 adc count
                 sta dma1_dest_lo                         ; dest = index2+count
                 lda index2+1
                 sta dma1_src_hi
                 adc argmo
                 sta dma1_dest_hi

; lda dma_ctlr+3  ;dma controller version    [910520] F018A
; and #1
; beq l81_1   ; F018    removed [910808] F018B
                 lda #%00110000                           ; F018A,B
l81_1            sta dma1_cmd                             ; command=copy, source=start   [910102]
; php
                 lda text_bank                            ; bank = BASIC text bank   [910520] F018A
; plp   ;version?    removed [910808] F018B
; bne l81_2   ; F018A
; and #%00001111  ; F018     [910102]
; ora #%01000000  ;(copy source=endpoint)    [910102]
l81_2            sta dma1_src_bank                        ; banks
                 sta dma1_dest_bank

                 lda #0                                   ; [910219]
; sta dma1_cmd  ; command = copy, source=endpoint
                 sta dma1_subcmd                          ; [910520] F018A
; dec a   ;      [910219]
                 sta dma_ctlr+2                           ; dma_list bank
                 ldy #>dma1_cmd                           ; dma_list
                 lda #<dma1_cmd
                 sty dma_ctlr+1                           ; dma_list hi
                 sta dma_ctlr                             ; dma_list lo & trigger
                 rts

;.end
