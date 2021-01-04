


; FIND   "string"                    [,line_range]
; CHANGE "oldstring" TO "newstring"  [,line_range]
;
; where <"> delimiter can be any character, but only
; double-quotes will prevent tokenization of strings.
;
; N.B.: I am assuming that lines cannot be greater than 255 chars, as is
; the case where the line was entered "normally", that is, using LINGET.

find
                rmb7 op                                 ; FIND flag
                !text $2c

change
                smb7 op                                 ; CHANGE flag
                rmb6 op                                 ; reset change-all mode
                jsr errind                              ; report error if not in direct mode

                jsr chrgot                              ; get delimeter
                ldx #0                                  ; evaluate string args
                jsr delimit_string                      ; string1
                lda fstr1+2
                +lbeq fcerr                             ; error if string1 null
                bbr7 op,l86_1                           ; branch if no string2
                jsr chrget                              ; pick up required 'to' token
                cmp #to_token
                +lbne snerr                             ; error if missing
                jsr chrget
                +lbeq snerr                             ; error if eol
                ldx #3
                jsr delimit_string                      ; string2

l86_1           jsr chrget                              ; line number range given?
                beq l86_2                               ; no, eol
                jsr chkcom                              ; yes, pick up required comma
l86_2           jsr range                               ; set up line number range (lowtr,linnum)
                jsr tto                                 ; save txtptr for restoration when done
                rmb7 helper                             ; clear 'help' flag for 'p1line'
                lda helper
                pha
                rmb4 helper                             ; temporarily disable token highlighting
                smb5 helper                             ; set   'find' flag for 'p1line'
                bra find_loop_1                         ; begin


find_loop
                ldy #0                                  ; move to next line (copy link bytes to lowtr)
                jsr indlow
                tax
                iny
                jsr indlow
                stx lowtr
                sta lowtr+1

find_loop_1
                ldy #1
                jsr indlow                              ; check link
                bne l87_1                               ; not null- continue
                dey
                jsr indlow
                +lbeq find_exit                         ; null- exit

l87_1           ldy #2
                jsr indlow                              ; check line number
                tax
                iny
                jsr indlow
                cmp linnum+1
                bne l87_2
                cpx linnum
                beq l87_3                               ; line is <= last line requested, continue
l87_2           +lbcs find_exit                         ; line is >  last line requested, exit

l87_3           ldx #3                                  ; set initial position - 1 (past link & line#)
                stx fndpnt


find_loop_2
                jsr _stop                               ; check stop key
                +lbeq find_break                        ; exit if down

                ldx fndpnt                              ; duh, where are we?
                clc
                txa                                     ; program:
                adc lowtr                               ; txtptr = line start + position in line
                sta txtptr
                lda #0
                adc lowtr+1
                sta txtptr+1                            ; search string:
                ldz #0                                  ; at the beginning

l88_1           jsr chargt                              ; get next character from text
                beq find_loop                           ; eol (no match this line)
                inx                                     ; bump pointer to next character
                cmp (fstr1),z                           ; character match?  ind okay- buffer
                bne l88_1                               ; no
                stx fndpnt                              ; yes- save next position

l88_2           inz                                     ; bump position in search string
                cpz fstr1+2                             ; string match?
                bcs print_line                          ; yes
                jsr chargt
                beq find_loop                           ; no- eol
                cmp (fstr1),z                           ; ind okay- buffer
                bne find_loop_2                         ; no- rewind to beginning of search string
                beq l88_2                               ; maybe- still more chars to compare


; Print the line of text at LOWTR, highlighting the section of code
; beginning at LOWTR+FNDPNT and running for FIND_COUNT characters.

print_line
                jsr crdo                                ; get a new display line
                lda fstr1+2                             ; length of string to highlight
                sta find_count
                ldy #2
                jsr indlow                              ; get ms byte of line number
                tax
                iny
                jsr indlow                              ; get ls byte
                jsr p1line                              ; print #, space, and the line of code
                bbr7 op,find_loop_2                     ; Find op? branch if so and continue search


; Change operation
; Query the user and replace string1 with string2 if he wants to.
; Options are  'Y' (yes),  '*' (do all),  'CR' (quit),  anything else means no.

change_line
                bbs6 op,l89_1                           ; branch if change-all mode set
                jsr _primm                              ; prompt & get response
                !text cr," CHANGE? ",0
                jsr response_get
                cmp #'Y'
                beq l89_1                               ; yes, change it
                cmp #cr
                +lbeq find_exit                         ; cr only, abort entire operation
                cmp #'*'
                bne find_loop_2                         ; *, change all.  else don't change
                smb6 op

; Replace string1 with string2.  Requires moving text up/down beginning at
; LOWTR+FNDPNT+(LEN(string1)-LEN(string2)) through TEXT_TOP and copying
; string1 into text beginning at LOWTR+FNDPNT for LEN(string2) characters.

l89_1           lda text_top                            ; setup upper address of text to move (index2)
                sta index2
                lda text_top+1                          ; TEXT_TOP
                sta index2+1

                clc                                     ; setup lower address of text to move (index1)
                lda fndpnt
                adc lowtr
                sta index1                              ; LOWTR+FNDPNT
                lda #0
                sta argmo                               ; count hi
                adc lowtr+1
                sta index1+1

                sec                                     ; calc number of chars to insert/delete
                lda fstr1+2                             ; LEN(string1)-LEN(string2)
                sbc fstr2+2
                beq l89_6                               ; branch if string1 = string2 (no move)
                bpl l89_4                               ; branch if string1 > string2 (delete)
; else      string1 < string2 (insert)

                neg                                     ; Move memory up to make room for larger string2
                sta count
                ldy #0                                  ; first check for line too long
                jsr indlow
                adc count
                taz
                iny
                jsr indlow                              ; (link+#chr)-line_sa must be <256
                adc #0
                tay
                sec
                tza
                sbc lowtr
                tya
                sbc lowtr+1
                +lbne errlen                            ; error, line > 255 characters

                clc                                     ; now check for sufficient memory
                ldy text_top+1
                lda count
                adc text_top
                bcc l89_2
                iny
l89_2           cpy max_mem_0+1
                bcc l89_3                               ; result is less than top-of-memory: ok
                +lbne omerr                             ; msb >  top, overflow
                cmp max_mem_0                           ; msb's the same, test lsb's
                +lbcs omerr                             ; lsb >= top, overflow
l89_3           sta text_top
                sty text_top+1                          ; set new top of text pointer
                jsr moveup                              ; make room
                bra l89_6                               ; go copy string2 into area

l89_4           sta count                               ; Move memory down for smaller string2
                ldy text_top+1
                lda text_top
                sec
                sbc count
                bcs l89_5
                dey
l89_5           sta text_top
                sty text_top+1                          ; set new top of text pointer
                jsr movedown                            ; squish out excess space

l89_6           lda fstr2+2                             ; Copy string2 into text
                beq l89_8                               ; branch if null, nothing to copy
                sta find_count                          ; how many characters to copy
                ldx #lowtr
                ldy fndpnt                              ; index into text
                ldz #0                                  ; index into string2
l89_7           lda (fstr2),z                           ; ind okay- buffer
                jsr sta_far_ram0                        ; do the copy
                iny
                inz
                dec find_count
                bne l89_7

l89_8           jsr link_program                        ; relink program
                clc
                lda fndpnt                              ; place find position after new text
                adc fstr2+2
                dec
                sta fndpnt
                +lbra find_loop_2                       ; and resume searching


find_exit
                jsr crdo                                ; normal exit
                pla
                sta helper                              ; restore token highlight status
                rmb5 helper                             ; remove 'find' flag
                +lbra direct_mode_exit                  ; done



find_omerr                                              ; out of memory
                ldx #errom
                !text $2c
find_errlen                                             ; string too long
                ldx #errls
                sec
                !text $89
find_break                                              ; stop key break
                clc
                pla
                sta helper                              ; restore token highlight status
                rmb5 helper                             ; remove 'find' flag
                +lbcc break_exit                        ; [910925]
                +lbra error


delimit_string                                          ; command is in buffer, .x = ptr to strptr
                sta match                               ; delimiter character
                lda txtptr                              ; point to first character in string
                inc                                     ; (never wraps- string in input buffer)
                sta fstr1,x                             ; set pointer to string data
                lda txtptr+1
                sta fstr1+1,x
                lda #$ff                                ; set string length
                sta fstr1+2,x

l90_1           inc fstr1+2,x
                jsr chargt                              ; build string
                +lbeq snerr                             ; error if eol encountered inside string
                cmp match
                bne l90_1                               ; continue until matching delimiter found
                rts

;.end



puctrl          jsr frmstr                              ; do frmevl,frestr. return with a=len, index=~string
                tay
                dey
                cpy #4
                +lbcs fcerr                             ; len > 4 is illegal value error

l91_1           jsr indin1_ram1                         ; lda (index),y
                sta puchrs,y
                dey
                bpl l91_1
                rts

;.end
