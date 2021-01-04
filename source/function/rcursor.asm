

;************************************************************************
;*   RCURSOR Returns in variable list current cursor position *
;*         *
;*   Syntax: RCURSOR [column [,row] ]    *
;************************************************************************

rcursor          sec                                      ; new [910228]
                 jsr _plot                                ; get current cursor position & save it
                 stx srow
                 sty column

                 ldx #0                                   ; just like RREG and RMOUSE...
                 stx count
l262_1           jsr chrgot                               ; Get a variable name from variable list
                 beq l262_4                               ; eol- exit
                 cmp #','                                 ;
                 beq l262_3                               ; null- skip this arg
                 jsr ptrget                               ; Get pointer to target variable
                 sta forpnt                               ; set up so we can share LET code
                 sty forpnt+1
                 lda valtyp                               ; what kind of variable name did ptrget find?
                 +lbne chkerr                             ; string- type mismatch error

l262_2           ldx count                                ; Make assignment
                 ldy column,x                             ; low byte
                 lda #0                                   ; high byte
                 jsr givayf                               ; float it
                 lda intflg                               ; set flags for type of var (int/float)
                 jsr qintgr                               ; use part of LET to do the work

l262_3           inc count                                ; Next assignment
                 ldx count
                 cpx #2                                   ; there are 2 possible
                 bcs l262_4                               ; done 2, exit
                 jsr chrgot                               ; check terminator
                 beq l262_4                               ; eol- exit
                 jsr chkcom                               ; check delimiter
                 bra l262_1                               ; loop until done

l262_4           rts

;.end



AutoScroll
                 pha                                      ; save character for Editor
                 bbs7 runmod,AutoScrollno                 ; branch if not direct mode
                 ldy channl                               ; is output redirected?
                 bne AutoScrollno                         ; yes- can't do scroll (need to read screen)
                 lda txttab
                 ldx txttab+1                             ; is there a program in memory to scroll?
                 sta txtptr
                 stx txtptr+1
                 iny                                      ; (1)
                 jsr indtxt
                 bne AutoScrollyes                        ; yes- continue
                 bra AutoScrollno                         ; no-  exit

AutoScrollpop
                 pla
                 pla
AutoScrollng
                 ldx point                                ; restore cursor position
                 ldy point+1
                 clc
                 jsr _plot
AutoScrollno
                 rmb1 helper                              ; remove LINGET flag
                 pla                                      ; restore character
                 sec                                      ; return to Editor with no action taken
                 rts

AutoScrollyes
                 ror form                                 ; save .c=direction (character already on stack)
                 sec
                 jsr _plot                                ; get current cursor position & save it
                 stx point
                 sty point+1
                 smb1 helper                              ; set flag for LINGET not to go to error if it has problems
                 bbs7 form,AutoScrolldn                   ; branch according to direction of scroll...


AutoScrollup                                              ; wanting to scroll up
                 sec
                 lda _screen_bottom                       ; put cursor at bottom of screen
                 sbc _screen_top
                 sta form+1                               ; save where it is- we'll be printing line there
                 tax
                 jsr AutoSearch                           ; search for a line number on screen, put it in linnum
                 jsr FindLine                             ; find the line in program
                 bcc l263_1   ;  line not found           ; we have a pointer to the next line
                 ldy #0
                 jsr indlow                               ; find the next line, the one we want to print, via link bytes
                 tax
                 iny
                 jsr indlow
                 stx lowtr                                ; advance pointer to it
                 sta lowtr+1
l263_1           ldx form+1                               ; put cursor back at bottom of screen
                 ldy #0
                 clc
                 jsr _plot
l263_2           jsr crdo                                 ; get a blank line to print on- scroll screen up
                 ldy #1
                 jsr indlow                               ; end of program marker?
                 bne AutoScrollprint                      ; no-  print this line & exit
                 lda txttab                               ; yes- loop to start of program,
                 ldx txttab+1
                 sta lowtr
                 stx lowtr+1
                 jsr crdo                                 ; and add an extra newline
                 bra l263_2


AutoScrolldn                                              ; wanting to scroll down
                 ldx #0                                   ; put cursor at top of screen
                 jsr AutoSearch                           ; search for a line number on screen, put it in linnum
                 ldx #0                                   ; get a blank line to print on
                 ldy #0                                   ; put cursor at top of screen
                 clc
                 jsr _plot
l264_1           jsr _primm                               ; and scroll screen (kill any pending Editor modes, too)
                 !text esc,esc,esc,"W",0
                 jsr FindLine                             ; find the line in program whose number we found on screen
                 lda lowtr                                ; (does not matter if it or next higher line is found)
                 cmp txttab
                 bne l264_2
                 lda lowtr+1
                 cmp txttab+1
                 bne l264_2
                 lda #$ff                                 ; special case- it's the very first line, want to wrap to last line
                 sta linnum+1                             ; fake pointer to the last line,
                 jsr _primm                               ; scroll screen to insert extra space,
                 !text esc,"W",0
                 bra l264_1                               ; and go around again

l264_2           lda txttab                               ; start at beginning of program (txttab) and find the line which points at (lowtr)
                 ldx txttab+1
l264_3           sta index                                ; pointer to link bytes
                 stx index+1
                 ldy #1
                 jsr indin1                               ; get link bytes
                 tax
                 dey
                 jsr indin1
                 cpx lowtr+1                              ; do link bytes point at target line?
                 bne l264_3
                 cmp lowtr
                 bne l264_3                               ; no- use these link bytes to find next line

                 lda index                                ; yes- copy pointer
                 ldx index+1
                 sta lowtr
                 stx lowtr+1
; bra AutoScrollprint ; print the line & exit


AutoScrollprint
                 ldy #2                                   ; get line number to print
                 jsr indlow
                 tax
                 iny
                 jsr indlow
                 jsr p1line                               ; print the number & the line
; bra AutoScrolldone ;Normal exit

AutoScrolldone
                 jsr _primm                               ; kill special Editor modes
                 !text esc,esc,0
                 ldx point                                ; restore cursor position
                 ldy point+1
                 clc
                 jsr _plot
                 rmb1 helper                              ; remove LINGET flag
                 pla                                      ; restore character
                 clc                                      ; return to Editor, with flag we handled character
                 rts

AutoSearch
                 ldy #0                                   ; search for any line number on screen in leftmost column
                 clc
                 jsr _plot                                ; move to beginning of next line
; bcs AutoScrollpop ;  exit if no more lines
                 bcs l265_4                               ; no more lines- fake one   [910716]
                 sec
                 jsr _plot                                ; else check if wrapped line
                 bcs l265_1                               ; it's wrapped- move up one line
                 lda _pnt
                 adc _screen_left                         ; (.c=0)
                 sta txtptr                               ; copy screen address of logical line to txtptr
                 lda _pnt+1
                 adc #0
                 sta txtptr+1
                 ldy #0                                   ; get first character on this line in window
                 lda (txtptr),y
; jsr indtxt  ;    (I did not want to limit search to the first column,
                 cmp #'9'+1                               ; but it was way too slow searching the entire screen)
                 bcs l265_1                               ; it's not a number
                 cmp #'0'
                 bcs l265_3                               ; it's a digit 0-9, continue

l265_1           bbs7 form,l265_2                         ; not on this line- move to next line
                 dex                                      ; move up one line
                 !text $89
l265_2           inx                                      ; move down one line
                 bra AutoSearch                           ; loop until we find a numeric digit or run out of lines

l265_3           clc                                      ; found a digit, get entire number into linnum & rts
                 +lbra linget

l265_4           lda #$ff                                 ; no line found, fake end of program   [910716]
                 sta linnum+1
                 rts




;.end