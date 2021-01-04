; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      input.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


get             jsr     errdir                          ; direct mode illegal
                sta     z_p_temp_1                      ; flag to distinguish between GET and GETKEY

                cmp     #'#'                            ; is it GET# ?
                beq     getn                            ; yes
                cmp     #key_token                      ; is it GETKEY ?
                bne     gettty                          ; no, must be plain GET
                jsr     chrget                          ; yes, skip over KEY token
                bra     gettty


getn            jsr     chrget                          ; GET# move up to next byte
                jsr     getbyt                          ; get channel into x
                lda     #','                            ; comma?
                jsr     synchr
                stx     channl
                jsr     coin                            ; chkin


gettty                                                  ; GET
                ldx     #<buf+1                         ; point to 0
                ldy     #>buf
                lda     #0                              ; to stuff and to point
                sta     buf+1                           ; zero it
                lda     #$40                            ; turn on v-bit
                jsr     inpco1                          ; do the get
                ldx     channl
                bne     release_channels                ; restore terminal channels
                rts


linputn                                                 ; input line from channel into a string var
                jsr     chrget                          ; (eat input# token)
                smb7    op
                !text $2c

inputn          rmb7    op                              ; flag INPUT# vs. LINPUT#
                jsr     getbyt                          ; get channel number
                lda     #','                            ; a comma?
                jsr     synchr
                stx     channl
                jsr     coin                            ; chkin
                jsr     notqti                          ; do input to variables


release_channels                                        ; iodone, iorele.
                jsr     _clrch                          ; clear I/O channels
; ldx #0   ;restore normal terminal channels
                sta     channl                          ; (was stx)     [910909]
                rts


linput                                                  ; input line from console into a string var
                jsr     chrget                          ; (eat input token)
                smb7    op
                !text $2c

input           rmb7    op                              ; flag INPUT vs. LINPUT
                cmp     #'"'                            ; a quote?
                bne     notqti                          ; no message
                jsr     strtxt                          ; literalize the string in text

                jsr     chrgot                          ; looking for prompt string terminator  [910219]
                cmp     #','
                bne     l55_1
                sta     buf_txtptr                      ; is comma- supress '?' after prompt  [910219]
                jsr     chrget                          ; eat comma
                jsr     strprt                          ; print prompt
                jsr     errdir                          ; error if direct mode
                jsr     InputLine                       ; get first item
                bra     getagn1                         ; see if there's more to do

l55_1           lda     #';'                            ; must end in semicolon
                jsr     synchr
                jsr     strprt                          ; print prompt

notqti          jsr     errdir                          ; use common routine since def direct
                lda     #','                            ; get comma
                sta     buf_txtptr                      ; (data reader expects buffer to start with terminator)

getagn          jsr     PromptedInput                   ; type "?" and input a line of text
getagn1         lda     channl
                beq     l56_1
                jsr     _readst                         ; get status byte
; and #2   ; (assumes serial bus????)  [910618] eoi ok
                and     #%10000111                      ; serial: err if dnp, r/w timeout errors
                beq     l56_1                           ; a-ok rs232: err if brk, ovr, frm, par errors
                jsr     release_channels                ; bad, close channel
                +lbra   data                            ; skip rest of input

l56_1           lda     buf                             ; bufful. get anything?
                bne     inpcon                          ; yes- process input
; lda channl  ;didn't get anything.  is this keyboard? [901212]
; bne getagn  ; no- keep looking for data ????
                jsr     datan                           ; skip to end of statement
                +lbra   addon


read            rmb7    op                              ; flag READ vs. LREAD    [910102]
                ldx     datptr                          ; get last data location
                ldy     datptr+1
                lda     #$98                            ; initiator= read
                !text $2c

inpcon          lda     #0                              ; initiator= input
inpco1          sta     input_flag                      ; $98=read, $40=get, $00=input

; In the processing of DATA and READ statements, one pointer points to the data
; (i.e., the numbers being fetched) and another points to the list of variables.
;
; The pointer into the data always starts pointing to a terminator- a ",", ":", or EOL.
; At this point TXTPTR points to list of variables and (x,y) points to data or input line.

                stx     inpptr                          ; pointer to data
                sty     inpptr+1

inloop          jsr     ptrget                          ; get a pointer to the variable
                sta     forpnt                          ; store its address
                sty     forpnt+1

                ldx     #1
l57_1           lda     txtptr,x                        ; move variable list pointer to 'vartxt'
                sta     vartxt,x
                lda     inpptr,x                        ; move data line pointer to 'txtptr'
                sta     txtptr,x
                dex
                bpl     l57_1

                jsr     chrgot                          ; get first data byte
                bne     datbk1                          ; not null, so we got something
                bit     input_flag                      ; READ($98), GET($40), or INPUT($00)?
                bvc     qdata                           ; branch if READ or INPUT
                lda     z_p_temp_1                      ; GET or GETKEY?
                cmp     #key_token
                bne     l57_3                           ; branch if GET

l57_2           jsr     cgetl                           ; GETKEY
                tax                                     ; test if null
                beq     l57_2                           ; it is null, keep scanning
                bne     l57_4                           ; got a key, go put it in var

l57_3           jsr     cgetl                           ; get a key if pressed, otherwise gets a zero
l57_4           sta     buf
                ldx     #<buf_txtptr
                ldy     #>buf_txtptr
                bra     datbk


qdata           +lbmi   datlop                          ; branch if READ
                lda     channl                          ; else it's INPUT
                bne     l58_1
                jsr     outqst                          ; console input, so display '? ' prompt

l58_1           jsr     PromptedInput                   ; get another line

datbk           stx     txtptr                          ; set for CHRGET
                sty     txtptr+1

datbk1          bbr7    op,l59_1                        ; no chrgot if LINPUT (want leading spaces) [910513]
                jsr     chargt
                jsr     chrtst
                bra     l59_2

l59_1           jsr     chrget                          ; get next data byte
l59_2           bbr7    valtyp,l59_8                    ; get value type, input a number if numeric
                bbr6    input_flag,l59_4                ; branch if not get, set quote
                inx
                stx     txtptr
l59_3           lda     #0                              ; [901212]
                sta     charac
                bra     l59_5

l59_4           bbs7    op,l59_3                        ; no terminators if LINPUT or LREAD  [901212]
                sta     charac                          ; setqut.  assume quoted string
                cmp     #'"'                            ; terminators ok?
                beq     l59_6                           ; yes (sets .c)
                lda     #':'                            ; set terminators to ":" and...
                sta     charac
                lda     #','                            ; ...comma

l59_5           clc                                     ; resetc
l59_6           sta     endchr                          ; nowget
                lda     txtptr
                ldy     txtptr+1
                adc     #0                              ; .c is set properly above
                bcc     l59_7
                iny
l59_7           jsr     strlt2                          ; make a string descriptor for the value & copy if needed
                jsr     st2txt                          ; copy strng2 to txtptr (st-2-txt... get it?)
                jsr     inpcom                          ; do assignment
                bra     l59_9

l59_8           bbs7    op,l59_10                       ; error if LINPUT (string input only)  [901212]
                ldx     #0                              ; numins. flag 'text bank' (0)
                jsr     fin
                lda     intflg                          ; set codes on flags
                jsr     qintgr                          ; go decide on float

l59_9           jsr     chrgot                          ; strdn2. read last character
                beq     trmok                           ; ":" or EOL is ok
                cmp     #','                            ; a comma?
                beq     trmok

                lda     input_flag                      ; is this get, read, or input?
                beq     l59_11                          ; input
                bmi     l59_10                          ; read
                ldx     channl                          ; get. if not kbd, go use 'bad file data error'
                bne     l59_12

l59_10          ldx     #errtm                          ; tmerr. 'get from kbd' or 'read' saw a bad type
                bra     l59_13                          ; always

l59_11          lda     channl
                beq     l59_14                          ; do again if keybd input
l59_12          ldx     #errbd                          ; input saw bad file data
l59_13          +lbra   error


l59_14          jsr     highlight_text                  ; [911119]
                jsr     _primm
                !text "?REDO FROM START",cr,0
                jsr     highlight_done                  ; [911119]

ott             lda     oldtxt
                ldy     oldtxt+1
                sta     txtptr                          ; put user back to beginning of input
                sty     txtptr+1
                rts



trmok           ldx     #1
l60_1           lda     txtptr,x
                sta     inpptr,x                        ; save for more reads
                lda     vartxt,x
                sta     txtptr,x                        ; point to variable list
                dex
                bpl     l60_1

                jsr     chrgot                          ; look at last vartab character
                beq     l60_2                           ; that's the end of the list
                jsr     chkcom                          ; not end. check for comma
                +lbra   inloop

l60_2           lda     inpptr                          ; put away a new data pntr name
                ldy     inpptr+1
                bbr7    input_flag,l60_3
                sta     datptr
                sty     datptr+1
                rts

l60_3           ldy     #0                              ; last data chr could have been ',' or ':' but should be null
                lda     #inpptr
                jsr     lda_far_ram0
                beq     l60_4                           ; it is null
                lda     channl                          ; if not terminal, no type
                bne     l60_4

                jsr     highlight_text                  ; [911119]
                jsr     _primm
                !text "?EXTRA IGNORED", cr,0
                jsr     highlight_done                  ; [911119]

l60_4           rts                                     ; do next statement


; DATLOP Routine Subroutine to find data.
;
; The search is made by using the execution code for data to skip over
; statements, the start word of each statement is compared with "data_token".
; Each new line number is stored in "datlin" so that if any error occurs while
; reading data the error message can give the line number of the bad data.

datlop          jsr     datan                           ; skip some text
                iny
                tax                                     ; end of line?
                bne     l61_1                           ; no
                ldx     #errod                          ; yes, "no data" error
                iny
                jsr     indtxt
                +lbeq   error

                iny
                jsr     indtxt                          ; get high byte of line number
                sta     datlin
                iny
                jsr     indtxt                          ; get low byte
                iny
                sta     datlin+1

l61_1           jsr     addon                           ; nowlin.  txtptr+.y
                jsr     chrgot                          ; span blanks
                tax                                     ; used later
                cpx     #data_token                     ; is it a DATA statement?
                bne     datlop                          ; not quite right, keep looking
                +lbra   datbk1                          ; this is the one


;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
